#!/bin/bash

set -e

CXXFLAGS="-I. -fPIC -O3 -DNDEBUG -DHAVE_ZLIB -DKENLM_MAX_ORDER=6 -lz $CXXFLAGS"

if [ ! -d "kenlm" ]
then
	if [ ! -f "kenlm.tar.gz" ]
	then
		echo -n "Downloading... "
		wget http://kheafield.com/code/kenlm.tar.gz 2> /dev/null
		echo "Done."
	fi
	echo -n "Extracting... "
	tar xfv kenlm.tar.gz > /dev/null
	echo "Done."
fi

echo -n "Building"
cp kenlm.cc kenlm/.
cd kenlm/

#Grab all cc files in these directories except those ending in test.cc or main.cc
objects=""
for i in util/double-conversion/*.cc util/*.cc lm/*.cc; do
	if [ "${i%test.cc}" == "$i" ] && [ "${i%main.cc}" == "$i" ]; then
		g++ $CXXFLAGS -c $i -o ${i%.cc}.o
		objects="$objects ${i%.cc}.o"
		echo -n "."
	fi
done

g++ $CXXFLAGS -Wall -c kenlm.cc -o kenlm.o
objects="$objects kenlm.o"
echo -n "."
g++ $CXXFLAGS -Wall -shared -o libkenlm.so $objects
echo -n "."

cp libkenlm.so ../.
cd ..
echo " Done."
