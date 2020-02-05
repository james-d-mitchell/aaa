#!/usr/bin/env bash

# If a command fails, exit this script with an error code
set -e

if [ "$SUITE" != "test" ] && [ "$SUITE" != "coverage" ] && [ "$SUITE" != "lint" ]; then
  echo -e "\nError, unrecognised Travis suite: $SUITE"
  exit 1
fi

mv ../aaa $HOME/aaa

################################################################################
# Install software necessary for tests and coverage: GAP and packages
################################################################################ 

################################################################################
# Install GAP
echo -e "\nInstalling GAP..."
if [ "$GAP" == "required" ]; then
  GAP=v`grep "GAPVERS" $HOME/aaa/PackageInfo.g | awk -F'"' '{print $2}'`
fi
GAPROOT="$HOME/gap"
echo -e "\nInstalling GAP $GAP into $GAPROOT..."
git clone -b $GAP --depth=1 https://github.com/gap-system/gap.git $GAPROOT
cd $GAPROOT
./autogen.sh
./configure --with-gmp=system $GAP_FLAGS
make -j4
mkdir pkg

################################################################################
# Copy Aaa to its proper location
mv $HOME/aaa $GAPROOT/pkg/aaa

# Common curl settings
CURL="curl --connect-timeout 5 --max-time 10 --retry 5 --retry-delay 0 --retry-max-time 40 -L"

################################################################################
# Install grape, io, orb, and profiling
PKGS=( "io" "orb" "grape" )
if [ "$SUITE" == "coverage" ]; then
  PKGS+=( "profiling" )
fi

for PKG in "${PKGS[@]}"; do
  cd $GAPROOT/pkg

  # Get the relevant version number
  if [ "$PACKAGES" == "latest" ] || [ "$PKG" == "profiling" ]; then
    VERSION=`$CURL -s "https://github.com/gap-packages/$PKG/releases/latest" | grep \<title\>Release | awk -F' ' '{print $2}'`
  else
    VERSION=`grep "\"$PKG\"" $GAPROOT/pkg/aaa/PackageInfo.g | awk -F'"' '{print $4}' | cut -c3-`
  fi

  URL="https://github.com/gap-packages/$PKG/releases/download/v$VERSION/$PKG-$VERSION.tar.gz"
  echo -e "\nDownloading $PKG-$VERSION ($PACKAGES version), from URL:\n$URL"
  $CURL "$URL" -o $PKG-$VERSION.tar.gz
  tar xf $PKG-$VERSION.tar.gz && rm $PKG-$VERSION.tar.gz

  if [ -f $PKG-$VERSION/configure ]; then
    if [ "$PKG" == "orb" ] || [ "$PKG" == "grape" ]; then
      cd $PKG-$VERSION && ./configure && make # orb/grape don't accept flags
    else
      cd $PKG-$VERSION && ./configure $PKG_FLAGS && make
    fi
  fi
done

################################################################################
# Install required GAP packages
cd $GAPROOT/pkg
echo -e "\nGetting the required GAP packages (smallgrp, transgrp, primgrp)..."
$CURL -O "https://www.gap-system.org/pub/gap/gap4pkgs/packages-required-master.tar.gz"
tar xf packages-required-master.tar.gz
rm packages-required-master.tar.gz

################################################################################
## Install NautyTracesInterface in Travis
if [ "$SETUP" == "travis" ]; then
  echo -e "\nGetting master version of NautyTracesInterface"
  git clone -b master --depth=1 https://github.com/sebasguts/NautyTracesInterface.git $GAPROOT/pkg/nautytraces
  cd $GAPROOT/pkg/nautytraces/nauty2*r* && ./configure $PKG_FLAGS && make
  cd $GAPROOT/pkg/nautytraces && ./autogen.sh && ./configure $PKG_FLAGS && make
fi
