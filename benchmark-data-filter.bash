#!/bin/bash

# Copyright (c) 2010, Toni Dietze

set -e -u -C
#set -u -C



function filter {
  declare -a cnt
  declare -i i=0
  while read -a l
  do
    if [ ${#l[@]} -eq 14 ]
    then
      len=$((l[5] + 1))
      cnt[$len]=$((cnt[$len] + 0))
      if ( [ $((len / 2)) -eq $(((len+1) / 2)) ] || [ $len -ge 35 ] )  \
        && ( [ $i -lt 205 ] && [ ${cnt[$len]} -lt 1 ] || [ $i -ge 205 ] && [ ${cnt[$len]} -lt 2 ] )
      then
        cnt[$len]=$((cnt[$len] + 1))
        echo "${l[@]}"
      fi
    fi
    i+=1
  done
}


for f in *.csv
do
  filter <"${f}" >|"filtered/${f##*/}"
done
