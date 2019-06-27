#############################################################################
##
#W  toperations.gd
#Y  Copyright (C) 2017                               Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains the declaration of operations that relate to transducers.

DeclareOperation("InverseTransducer", [IsTransducer]);
DeclareOperation("TransducerProduct", [IsTransducer, IsTransducer]);
DeclareOperation("RemoveStatesWithIncompleteResponse", [IsTransducer]);
DeclareOperation("RemoveInaccessibleStates", [IsTransducer]);
DeclareOperation("CopyTransducerWithInitialState", [IsTransducer, IsPosInt]);
DeclareOperation("RemoveEquivalentStates", [IsTransducer]);
DeclareAttribute("IsInjectiveTransducer", IsTransducer);
DeclareAttribute("IsSurjectiveTransducer", IsTransducer);
DeclareAttribute("IsBijectiveTransducer", IsTransducer);
DeclareAttribute("EqualImagePrefixes", IsTransducer);
DeclareAttribute("TransducerSynchronizingLength", IsTransducer);
DeclareAttribute("IsDegenerateTransducer", IsTransducer);
DeclareAttribute("IsMinimalTransducer", IsTransducer);
DeclareAttribute("IsSynchronousTransducer", IsTransducer);
DeclareAttribute("IsLipschitzTransducer", IsTransducer);
DeclareAttribute("IsSynchronizingTransducer", IsTransducer);
DeclareAttribute("IsBisynchronizingTransducer", IsTransducer);
DeclareAttribute("IsInLB", IsTransducer);
DeclareAttribute("TransducerCore", IsTransducer);
DeclareOperation("IsomorphicInitialTransducers",[IsTransducer, IsTransducer]);
DeclareOperation("IsomorphicTransducers", [IsTransducer, IsTransducer]);
DeclareOperation("EqualTransducers",[IsTransducer, IsTransducer]);
DeclareOperation("OmegaEquivalentTransducers",[IsTransducer, IsTransducer]);
DeclareOperation("CoreProduct", [IsTransducer, IsTransducer]);
DeclareAttribute("IsInV", IsTransducer);
DeclareAttribute("CombineEquivalentStates", IsTransducer);
DeclareAttribute("Order", IsTransducer);
DeclareAttribute("ImageAutomaton",IsTransducer);
DeclareAttribute("MinimiseTransducer", IsTransducer);
DeclareAttribute("HasClopenImage", IsTransducer);
DeclareOperation("\*", [IsTransducer, IsTransducer]);
DeclareOperation("\^", [IsTransducer, IsInt]);
DeclareOperation("\^", [IsTransducer, IsTransducer]);
