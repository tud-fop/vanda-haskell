#!/bin/bash

set -e

CXXFLAGS="-I. -fPIC -O3 -DNDEBUG -DHAVE_ZLIB -DKENLM_MAX_ORDER=6 $CXXFLAGS"

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

for i in util/{bit_packing,ersatz_progress,exception,file,file_piece,murmur_hash,mmap,pool,read_compressed,string_piece,usage} lm/{bhiksha,binary_format,config,lm_exception,model,quantize,read_arpa,search_hashed,search_trie,trie,trie_sort,value_build,virtual_interface,vocab}
do
	g++ $CXXFLAGS -c $i.cc -o $i.o
	echo -n "."
done

g++ $CXXFLAGS -Wall -c kenlm.cc -o kenlm.o
echo -n "."
g++ $CXXFLAGS -Wall -shared -o kenlm.so {lm,util,.}/*.o
echo -n "."

cp kenlm.so ../.
cd ..
echo " Done."
