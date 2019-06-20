#############################################################################
##
#W  toperations.gi
#Y  Copyright (C) 2017                               Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for operations that relate to transducers.

InstallMethod(InverseTransducer, "for a transducer",
[IsTransducer],
function(T)
  local newstates, ntfunc, nofunc, n, x, q, word, preimage, newstate, tdcrf;
  newstates := [];
  ntfunc := [[]];
  nofunc := [[]];
  newstates := [[[], 1]];

  n := 0;
  for q in newstates do
    n := n + 1;
    for x in OutputAlphabet(T) do
      word := [];
      Append(word, q[1]);
      Append(word, [x]);
      preimage := GreatestCommonPrefix(PreimageConePrefixes(word, q[2], T));
      tdcrf := TransducerFunction(T, preimage, q[2]);

      newstate := [Minus(word, tdcrf[1]), tdcrf[2]];

      if not newstate in newstates then
        Add(newstates, newstate);
        Add(ntfunc, []);
        Add(nofunc, []);
      fi;

      ntfunc[n][x + 1] := Position(newstates, newstate);
      nofunc[n][x + 1] := preimage;
    od;
  od;

  return Transducer(NrOutputSymbols(T), NrInputSymbols(T), ntfunc, nofunc);
end);

InstallMethod(TransducerProduct, "for two transducers",
[IsTransducer, IsTransducer],
function(tdcr1, tdcr2)
  local newstates, newstate, ntfun, nofun, tducerf, word, x, y, q, n;
  newstates := [];
  ntfun := [];
  nofun := [];

  if NrOutputSymbols(tdcr1) <> NrInputSymbols(tdcr2) then
    ErrorNoReturn("aaa: TransducerProduct: usage,\n",
                  "the output alphabet of the first argument must be the ",
                  "input alphabet\nof the second argument,");
  fi;

  for x in States(tdcr1) do
    for y in States(tdcr2) do
      Add(newstates, [x, y]);
      Add(ntfun, []);
      Add(nofun, []);
    od;
  od;

  n := 0;
  for q in newstates do
    n := n + 1;
    for x in InputAlphabet(tdcr1) do
      word := OutputFunction(tdcr1)[q[1]][x + 1];
      tducerf := TransducerFunction(tdcr2, word, q[2]);
      newstate := [TransitionFunction(tdcr1)[q[1]][x + 1], tducerf[2]];
      ntfun[n][x + 1] := Position(newstates, newstate);
      nofun[n][x + 1] := tducerf[1];
    od;
  od;

  return Transducer(NrInputSymbols(tdcr1), NrOutputSymbols(tdcr2), ntfun,
  nofun);
end);

InstallMethod(\*, "for two transducers",
[IsTransducer, IsTransducer],
TransducerProduct);

InstallMethod(\^, "for a transducer and a positive integer",
[IsTransducer, IsInt],
function(T, n)
  local flag, tducer, x;
  flag := false;
  if n = 0 then
    if InputAlphabet(T) = OutputAlphabet(T) then
       return IdentityTransducer(Size(InputAlphabet(T)));
    fi;
  fi;
  if n<0 then
    if not IsBijectiveTransducer(T) then 
      return fail;
    fi;
    flag := true;
  fi;
  tducer := CopyTransducerWithInitialState(T, 1);

  for x in [1 .. n - 1] do
    tducer := tducer * T;
  od;
  if flag then
    tducer := InverseTransducer(tducer);
  fi;
  return tducer;
end);

InstallMethod(\=, "for two transducers",
[IsTransducer, IsTransducer], OmegaEquivalentTransducers);

InstallMethod(IsSynchronousTransducer, "for a transducer", [IsTransducer],
function(T)
  return ForAll(Concatenation(OutputFunction(T)), x -> (Size(x)=1));
end);

#causes infinite loop if transducer has a state which can only write one infinite word so I added a check for this but check can be very slow.
InstallMethod(RemoveStatesWithIncompleteResponse, "for a transducer",
[IsTransducer],
function(T)
  local i, check, s, output, outputs, word, ntfunc, nofunc, n, x;
  if IsDegenerateTransducer(T) then
	return fail;
  fi;
  if Size(OutputAlphabet(T)) = 1 and Size(States(T)=1) then 
    return fail;
  fi;
  word := [1 .. Size(States(T))];
  for s in States(T) do
    check := true;
    outputs := [];
    for word in IteratorOfTuples(InputAlphabet(T),Size(States(T))) do
      output := TransducerFunction(T, word, s)[1];
      if Size(outputs) = 0 then
        Add(outputs, output);
      else
        if not IsPrefix(output, outputs[1]) and not IsPrefix(outputs[1],output) then
          check := false;
          break;
        fi;
      fi;
    od;
    if check then 
	return fail;
    fi;
  od;
  ntfunc := [];
  nofunc := [];
  for x in [1 .. NrStates(T) + 1] do
    Add(ntfunc, []);
    Add(nofunc, []);
  od;
  for x in InputAlphabet(T) do
    ntfunc[1][x + 1] := TransducerFunction(T, [x], 1)[2] + 1;
    nofunc[1][x + 1] := ImageConeLongestPrefix([x], 1, T);
  od;
  for n in [2 .. NrStates(T) + 1] do
    for x in InputAlphabet(T) do
      nofunc[n][x + 1] := Minus(ImageConeLongestPrefix([x], n - 1, T),
                                ImageConeLongestPrefix([], n - 1, T));
      ntfunc[n][x + 1] := TransducerFunction(T, [x], n - 1)[2] + 1;
      od;
    od;

  return Transducer(NrInputSymbols(T), NrOutputSymbols(T), ntfunc, nofunc);
end);

InstallMethod(RemoveInaccessibleStates, "for a transducer",
[IsTransducer],
function(T)
  local states, newq, newl, new, q, n, x;

  states := [1];
  newq := [[]];
  newl := [[]];
  n := 0;

  for q in states do
    n := n + 1;
    for x in InputAlphabet(T) do
      new := TransducerFunction(T, [x], q);

      if not new[2] in states then
        Add(states, new[2]);
        Add(newq, []);
        Add(newl, []);
      fi;

      newq[n][x + 1] := Position(states, new[2]);
      newl[n][x + 1] := new[1];
    od;
  od;

  return Transducer(NrInputSymbols(T), NrOutputSymbols(T), newq, newl);
end);

InstallMethod(CopyTransducerWithInitialState,
"for a transducer and a positive integer",
[IsTransducer, IsPosInt],
function(T, i)
  local new, newq, newl, q, states, x, n;
  states := ShallowCopy(States(T));

  if not i in states then
    ErrorNoReturn("aaa: ChangeInitialState: usage,\n",
                  "the second argument is not a state of the first argument,");
  fi;

  newq := [];
  newl := [];
  n := 0;

  for x in states do
    Add(newq, []);
    Add(newl, []);
  od;

  Sort(states, function(x, y)
                 return x = i;
               end);

  for q in states do
    n := n + 1;
    for x in InputAlphabet(T) do
      new := TransducerFunction(T, [x], q);
      newq[n][x + 1] := Position(states, new[2]);
      newl[n][x + 1] := new[1];
    od;
  od;

  return Transducer(NrInputSymbols(T), NrOutputSymbols(T), newq, newl);
end);

InstallMethod(IsSynchronizingTransducer, "for a transducer",
[IsTransducer],
function(T)
  return TransducerSynchronizingLength(T)<infinity;
end);

InstallMethod(IsBisynchronizingTransducer, "for a transducer",
[IsTransducer],
function(T)
  return IsBijectiveTransducer(T) and IsSynchronizingTransducer(T) and IsSynchronizingTransducer(InverseTransducer(T));
end);

InstallMethod(TransducerCore, "for a transducer",
[IsTransducer],
function(T)
  local SLen;
  SLen := TransducerSynchronizingLength(T);
  if SLen = infinity then
    return fail;
  fi;
  return RemoveInaccessibleStates(CopyTransducerWithInitialState(T,TransducerFunction(T,ListWithIdenticalEntries(0,SLen),1)[2]));
end);

InstallMethod(RemoveEquivalentStates, "for a transducer",
[IsTransducer],
function(T)
  local states, n, Eq, Reps, q, p, Eqclass, new, newq, newl, x, seen, dmy, nsr,
        ns;
  ns := NrStates(T);
  nsr := 0;
  dmy := CopyTransducerWithInitialState(T, 1);
  Eqclass := function(y)
               local class;
                 for class in Eq do
                   if y in class then
                     return Minimum(class);
                   fi;
                 od;
             end;

  while nsr < ns do
    ns := NrStates(dmy);
    states := [1 .. ns];
    n := 0;
    Eq := [];
    Reps := [];
    newq := [];
    newl := [];
    seen := [];
    for q in states do
      if not q in seen then
        n := n + 1;
        Add(Eq, []);
        Add(Reps, q);
        for p in states do
          if not p in seen then
            if TransitionFunction(dmy)[q] = TransitionFunction(dmy)[p] and
                OutputFunction(dmy)[q] = OutputFunction(dmy)[p] then
              Add(Eq[n], p);
              Add(seen, p);
            fi;
          fi;
        od;
      fi;
    od;
    n := 0;
    for q in Reps do
      n := n + 1;
      Add(newq, []);
      Add(newl, []);

      for x in InputAlphabet(dmy) do
        new := TransducerFunction(dmy, [x], q);
        newl[n][x + 1] := new[1];
        newq[n][x + 1] := Position(Reps, Eqclass(new[2]));
      od;
    od;
    dmy := Transducer(NrInputSymbols(dmy), NrOutputSymbols(dmy), newq, newl);
    nsr := NrStates(dmy);
  od;
  return dmy;
end);

InstallMethod(IsInjectiveTransducer, "for a transducer",
[IsTransducer],
function(T)
  local BadAut, S, CompDetAutIntersect, A, StatesNr, Alph, TMat, s, out, i, j, x, TMat2, NewNrStates, Languages;
  if IsDegenerateTransducer(T) then
  	return fail;
  fi;
  
  CompDetAutIntersect := function(D1,D2)
    local DC1, DC2, D3, S1, S2, S3;
    S1 := [1 .. NumberStatesOfAutomaton(D1)];
    S2 := [1 .. NumberStatesOfAutomaton(D2)];
    SubtractSet(S1, FinalStatesOfAutomaton(D1));
    SubtractSet(S2, FinalStatesOfAutomaton(D2));
    DC1 := CopyAutomaton(D1);
    DC2 := CopyAutomaton(D2);
    SetFinalStatesOfAutomaton(DC1,S1);
    SetFinalStatesOfAutomaton(DC2,S2); 
    D3 := NullCompletionAutomaton(RatExpToAut(FAtoRatExp(UnionAutomata(DC1,DC2))));
    S3 := [1 .. NumberStatesOfAutomaton(D3)];
    SubtractSet(S3, FinalStatesOfAutomaton(D3));
    SetFinalStatesOfAutomaton(D3,S3);
    return D3;
 end;

  A:= ImageAutomaton(T);
  StatesNr := NumberStatesOfAutomaton(A);
  Alph     := AlphabetOfAutomatonAsList(A);
  TMat     := TransitionMatrixOfAutomaton(A);
 
  for s in States(T) do
    Languages := [];
    for x in InputAlphabet(T) do
      out := TransducerFunction(T,[x],s)[1];
      NewNrStates := StatesNr + 1 + Size(out);
      TMat2 := StructuralCopy(TMat);
      for j in [1 .. Size(out)-1] do 
        TMat2[out[j]+1][StatesNr + j] := [StatesNr + j + 1];
      od;
      if Size(out) > 0 then
        TMat2[out[Size(out)]+1][StatesNr + Size(out)] := [TransducerFunction(T,[x],s)[2]];
        TMat2[Size(Alph)][NewNrStates] := [StatesNr + 1];
      else;
        TMat2[Size(Alph)][NewNrStates] := [TransducerFunction(T,[x],s)[2]];
      fi;
     BadAut := Automaton("epsilon",NewNrStates, Alph, TMat2, [NewNrStates],[1 .. NewNrStates]);
     Add(Languages,NullCompletionAutomaton(RatExpToAut(FAtoRatExp(BadAut))));    
    od;
    for i in [1 .. Size(InputAlphabet(T))] do
      for j in [i+1 .. Size(InputAlphabet(T))] do
        if not IsFiniteRegularLanguage(CompDetAutIntersect(Languages[i],Languages[j])) then
          SetIsInjectiveTransducer(T,false);
          return false;
        fi;
      od;
    od;
  od;

  SetIsInjectiveTransducer(T, true);
  return true;
end);

InstallMethod(EqualImagePrefixes, "for a transducer",
[IsTransducer],
function(T)
  local tducer;

  if HasEqualImagePrefixes(T) then
    return EqualImagePrefixes(T);
  elif HasIsInjectiveTransducer(T) then
    if IsInjectiveTransducer(T) = false then
      tducer := CopyTransducerWithInitialState(T, 1);
      IsInjectiveTransducer(tducer);
      SetEqualImagePrefixes(T, EqualImagePrefixes(tducer));
      return EqualImagePrefixes(T);
    fi;
  elif IsInjectiveTransducer(T) = false then
    return EqualImagePrefixes(T);
  else
    return fail;
  fi;
  return fail;
end);

QuotientTransducer := function(T,EqR) 
  local Classes, class, i, Pi, Lambda, initialclass; 
  Classes:=ShallowCopy(EquivalenceRelationPartition(EquivalenceRelationByPairs(Domain(States(T)),EqR)));
  
  class := function(q)
        local j;
        for j in [1 .. Length(Classes)] do
                if q in Classes[j] then
                        return j;
                fi;
        od;
	return fail;
  end;
  for i in States(T) do
  	if class(i)=fail then
		Add(Classes,[i]);
	fi;
  od;
  for i in Classes do
  	if 1 in i then
          initialclass := i;
        fi;
  od;
  Remove(Classes,Position(Classes,initialclass));
  Classes := Concatenation([initialclass],Classes);
  Pi:= ShallowCopy(Classes);
  Lambda := ShallowCopy(Classes);
  Apply(Pi,x -> TransitionFunction(T)[x[1]]);
  Apply(Lambda, x-> OutputFunction(T)[x[1]]);
  for i in Pi do
        Apply(i,class);
  od;
  return Transducer(Length(InputAlphabet(T)),Length(OutputAlphabet(T)),Pi,Lambda
);
end;

InstallMethod(TransducerSynchronizingLength, "for a transducer", [IsTransducer],
function(T)
	local count, CopyT, TempT, flag;
	flag := true;
        CopyT := CopyTransducerWithInitialState(T,1);
	count := -1;
	while flag do
		count := count + 1;
		TempT := QuotientTransducer(CopyT,Filtered(Cartesian(States(CopyT), States(CopyT)),x-> TransitionFunction(CopyT)[x[1]]=TransitionFunction(CopyT)[x[2]]));
		flag := (States(CopyT) <> States(TempT));
		CopyT := TempT;
	od;
	if States(CopyT) = [1] then 
		return count;
	fi;
	return infinity;
end);

InstallMethod(ImageAutomaton, "for a transducer", 
[IsTransducer],
function(T)
  local numberofstates, i, transitiontable, currentnewstate, j, k;
  numberofstates := Size(States(T));
  for i in Concatenation(OutputFunction(T)) do
    if not Size(i)=0 then
      numberofstates := numberofstates + Size(i) - 1;
    fi;
  od;
  transitiontable := List([1 .. Size(OutputAlphabet(T))+1], x -> List([1 .. numberofstates], y-> []));
  currentnewstate := Size(States(T)) + 1;
  for i in States(T) do
    for j in InputAlphabet(T) do
      if Size(OutputFunction(T)[i][j+1]) > 1 then
         Add(transitiontable[OutputFunction(T)[i][j+1][1]+1][i],currentnewstate);
         for k in [2 .. Size(OutputFunction(T)[i][j+1])-1] do
           AddSet(transitiontable[OutputFunction(T)[i][j+1][k]+1][currentnewstate],currentnewstate + 1);
           currentnewstate := currentnewstate + 1;
         od;
           AddSet(transitiontable[OutputFunction(T)[i][j+1][Size(OutputFunction(T)[i][j+1])]+1][currentnewstate],TransducerFunction(T,[j],i)[2]);
           currentnewstate := currentnewstate + 1;
      fi;
      if Size(OutputFunction(T)[i][j+1]) = 1 then 
          AddSet(transitiontable[OutputFunction(T)[i][j+1][1]+1][i],TransducerFunction(T,[j],i)[2]);
      fi;
      if Size(OutputFunction(T)[i][j+1]) < 1 then 
          AddSet(transitiontable[Size(OutputAlphabet(T))+1][i],TransducerFunction(T,[j],i)[2]);
      fi;
    od;
  od;
  return Automaton("epsilon", numberofstates, Concatenation(List(OutputAlphabet(T),x->String(x)[1]),"@"), transitiontable, [1], [1 .. numberofstates]);
end);

InstallMethod(IsDegenerateTransducer, "for a transducer",
[IsTransducer],
function(T)
	local Out, D, OutNeigh;
	Out := States(T);
	OutNeigh := function(s)
		local Output, i;
		Output := [];
		for i in InputAlphabet(T) do
			if TransducerFunction(T,[i],s)[1] = [] then
				Add(Output,TransducerFunction(T,[i],s)[2]);
			fi;
		od;
		return Output;
	end;
	Apply(Out, OutNeigh);
	D := Digraph(Out);
	return DigraphHasLoops(D) or DigraphGirth(D) < infinity;
end);

InstallMethod(CombineOmegaEquivalentStates, "for a transducer",
 [IsTransducer],
function(T)
  local  x, Bad, EqRelation, i, tuple, NewTuple, b, flag;
  EqRelation:= Cartesian(States(T),States(T));
  Bad:= [];
  for i in InputAlphabet(T) do
        for tuple in EqRelation do
                if TransducerFunction(T,[i],tuple[1])[1] <> TransducerFunction(T,[i],tuple[2])[1] then
                        if not tuple in Bad then
				Add(Bad,tuple);
			fi;
                fi;
        od;
  od;
  for b in Bad do
        Remove(EqRelation,Position(EqRelation,b));
  od;
  flag := true;
  while flag do
        flag := false;
        for tuple in EqRelation do
                for i in InputAlphabet(T) do
                        NewTuple := [TransducerFunction(T,[i],tuple[1])[2],TransducerFunction(T,[i],tuple[2])[2]];
                        if not NewTuple in EqRelation then
                                Remove(EqRelation,Position(EqRelation,tuple));
                                Remove(EqRelation,Position(EqRelation,[tuple[2],tuple[1]]));
                                flag:=true;
                                break;
                        fi;
                od;
        od;
  od;
  return QuotientTransducer(T,EqRelation);
end);

InstallMethod(IsomorphicInitialTransducers, "for a pair of transducer",
[IsTransducer, IsTransducer],
function(T1,T2)
  local D1, D2, perm, Dtemp, i;
  if not States(T1) = States(T2) then
    return false;
  fi;
  if not InputAlphabet(T1)=InputAlphabet(T2) then
    return false;
  fi;
  if not OutputAlphabet(T1)= OutputAlphabet(T2) then
    return false;
  fi;
  D1 := List([1 .. Size(States(T1))], x -> [OutputFunction(T1)[x], TransitionFunction(T1)[x]]);
  D2 := List([1 .. Size(States(T2))], x -> [OutputFunction(T2)[x], TransitionFunction(T2)[x]]);
  for perm in SymmetricGroup(Size(States(T1))) do
     if 1^perm = 1 then
       Dtemp := StructuralCopy(List([1 .. Size(States(T1))],x-> D1[x^perm]));
       for i in [1 .. Size(Dtemp)] do
         Apply(Dtemp[i][2], x -> x^perm);
       od;
       if Dtemp = D2 then
         return true;
       fi;
     fi;
  od;
  return false;
end);

InstallMethod(IsMinimalTransducer, "for a Transducer",
[IsTransducer],
function(T)
  local min;
  min := MinimiseTransducer(T);
  if min = fail then
    return fail;
  fi;
  return IsomorphicInitialTransducers(T,min);
end);

InstallMethod(IsBijectiveTransducer, "for a transducer",
[IsTransducer],
function(T)
  return not IsDegenerateTransducer(T) and IsInjectiveTransducer(T) and IsSurjectiveTransducer(T);
end);

InstallMethod(EqualTransducers, "for a pair of transducers",
[IsTransducer, IsTransducer],
function(T1,T2)
return OutputFunction(T1)=OutputFunction(T2) and TransitionFunction(T1)=TransitionFunction(T2);
end);

InstallMethod(Order, "for a transducers", 
[IsTransducer],
function(T)
  local p, id;
  p:= 1;
  if not InputAlphabet(T)= OutputAlphabet(T) then
    return fail;
  fi;
  id := IdentityTransducer(Size(InputAlphabet(T)));
  while not T^p=id do
    p:= p+1;
  od;
  return p;
end);

InstallMethod(OmegaEquivalentTransducers, "for a pair of transducers",
[IsTransducer,IsTransducer],
function(T1,T2)
  local M1,M2;
  M1:= MinimiseTransducer(T1);
  M2:= MinimiseTransducer(T2);
  if M1 = fail or M2 = fail then
    return fail;
  fi;
  return IsomorphicInitialTransducers(M1,M2);
end);

InstallMethod(MinimiseTransducer, "for a transducer",
[IsTransducer],
function(T)
  local T2;
  if IsDegenerateTransducer(T) then
	return fail;
  fi;
  T2:=RemoveStatesWithIncompleteResponse(RemoveInaccessibleStates(T));
  if T2=fail then 
    return T2;
  fi;
  return CombineOmegaEquivalentStates(T2);
end);

InstallMethod(IsSurjectiveTransducer, "for a transducer",
[IsTransducer],
function(T)
  local usefulstates, prefixcodes, imagetrees, completeblocks, finalimagetree,
  currentblocks, containsantichain, currentword, x, flag, y, minwords, tyx,
  pos, keys, subtree, check, pos2, prefix, block, state, imagekeys,
  minword, answer;
  if IsDegenerateTransducer(T) then
	return fail;
  fi;
  imagetrees := States(T);
  completeblocks := [];
  usefulstates := [1];
  prefixcodes := [];

  containsantichain := function(list, n)
    local currentword, minwords, x, y, check, maxword, children, i;
    if IsEmpty(list) then
      return false;
    fi;

    currentword := [];
    minwords := [];
    check := false;
    for x in list do
      for y in [1 .. Size(minwords)] do
        if IsPrefix(x, minwords[y]) then
          minwords[y] := StructuralCopy(x);
          break;
        elif IsPrefix(minwords[y], x) then
          check := true;
          break;
        fi;
      od;
      if not check then
        Add(minwords, StructuralCopy(x));
      fi;
      check := false;
    od;
    while not minwords = [[]] do
      Sort(minwords, function(x, y)
                       return Size(x) > Size(y);
                     end);
      maxword := StructuralCopy(minwords[1]);
      Remove(maxword);
      children := [];
      minwords := Set(minwords);
      for i in [0 .. n - 1] do
        Add(children, Concatenation(maxword, [i]));
      od;
      if not IsSubset(minwords, children) then
        return false;
      else
        for i in children do
          RemoveSet(minwords, i);
        od;
        AddSet(minwords, maxword);
      fi;
    od;
    return true;
  end;

  for x in usefulstates do
    flag := true;
    Add(prefixcodes, []);
    currentword := [];
    while flag do
      while IsEmpty(TransducerFunction(T, currentword, x)[1]) do
        Add(currentword, 0);
      od;
      Add(prefixcodes[Position(usefulstates, x)], StructuralCopy(currentword));
      while currentword[Size(currentword)] = NrInputSymbols(T) - 1 do
        Remove(currentword);
        if IsEmpty(currentword) then
          break;
        fi;
      od;

      if not IsEmpty(currentword) then
        currentword[Size(currentword)] := currentword[Size(currentword)] + 1;
      else
        flag := false;
      fi;
    od;
    for y in prefixcodes[Size(prefixcodes)] do;
      tyx := TransducerFunction(T, y, x);
      if not tyx[2] in usefulstates then
         Add(usefulstates, tyx[2]);
      fi;
    od;
    imagetrees[x] := [];
    for y in prefixcodes[Size(prefixcodes)] do
      tyx := TransducerFunction(T, y, x);
      pos := Position(List(imagetrees[x], y -> y[1]), tyx[1]);
      if not pos = fail then
        AddSet(imagetrees[x][pos][2], tyx[2]);
      else
        AddSet(imagetrees[x], [tyx[1], [tyx[2]]]);
      fi;
    od;
  od;
 
  finalimagetree := [[[], [1]]];
  currentblocks := [[[], [[[], [1]]]]];
  keys := [[]];
  while (not IsSubset(completeblocks, List(currentblocks, x -> x[2]))) and
      containsantichain(keys, NrInputSymbols(T)) do
    for block in currentblocks do
      if not block[2] in completeblocks then
        break;
      fi;
    od;
    keys := List(finalimagetree, x -> x[1]);
    for state in finalimagetree[Position(keys, block[1])][2] do
      imagekeys := StructuralCopy(List(imagetrees[state], x -> x[1]));
      for prefix in imagekeys do
        keys := List(finalimagetree, x -> x[1]);
        pos := Position(keys, Concatenation(block[1], prefix));
        if not pos = fail then
          pos2 := Position(imagekeys, prefix);
          Append(finalimagetree[pos][2], imagetrees[state][pos2][2]);
          finalimagetree[pos][2] := Set(finalimagetree[pos][2]);
        else
          pos2 := Position(imagekeys, prefix);
          Add(finalimagetree, [StructuralCopy(Concatenation(block[1], prefix)),
              StructuralCopy(imagetrees[state][pos2][2])]);
        fi;
      od;
    od;
    keys := List(finalimagetree, x -> x[1]);
    pos := Position(keys, block[1]);
    Remove(finalimagetree, pos);
    Remove(currentblocks, Position(currentblocks, block));
    AddSet(completeblocks, block[2]);
    minwords := [];
    check := false;
    keys := List(finalimagetree, x -> x[1]);
    prefix := StructuralCopy(block[1]);
    for x in keys do
      if IsPrefix(x, prefix) then
        for y in [1 .. Size(minwords)] do
          if IsPrefix(minwords[y], x) then
            minwords[y] := StructuralCopy(x);
          elif IsPrefix(x, minwords[y]) then
            check := true;
          fi;
        od;

        if not check then
          Add(minwords, StructuralCopy(x));
        fi;
        check := false;
      fi;
      minwords := Set(minwords);
    od;
    for minword in minwords do
      subtree := [];
      for x in [1 .. Size(keys)] do
         if IsPrefix(keys[x], minword) then
           Add(subtree, [Minus(keys[x], minword),
               StructuralCopy(finalimagetree[x][2])]);
         fi;
      od;
      Add(currentblocks, [ShallowCopy(minword), StructuralCopy(subtree)]);
    od;
    keys := List(finalimagetree, x -> x[1]);
  od;

  answer := containsantichain(keys, NrInputSymbols(T));
  SetIsSurjectiveTransducer(T, answer);
  return answer;
end);


InstallMethod(IsLipschitzTransducer, "for a transducer",
[IsTransducer],
function(T)
  local s, i, j, output;
  for s in States(T) do;
    for i in Tuples(InputAlphabet(T),Size(States(T))) do
      for j in [1 .. Size(i)] do
        output := TransducerFunction(T,i{[1 .. j]},s);
        if output[2] = s then 
             if Size(output[1]) <> j then
               return false;
             fi;
             break;
        fi;
      od;
    od;
  od;
  return true;
end);


InstallMethod(IsInLB, "for a transducer",
[IsTransducer],
function(T)
   local tinverse;
   if not IsBijectiveTransducer(T) then
     return false;
   fi;
   tinverse := InverseTransducer(T);
   return IsSynchronizingTransducer(T) and IsLipschitzTransducer(T) and IsSynchronizingTransducer(tinverse) and IsLipschitzTransducer(tinverse);
end);
