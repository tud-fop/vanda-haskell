#!/bin/bash

# Copyright (c) 2011, Toni Dietze

# set -e -u -C

function speedtest() {
  echo "${@}" >&2

  for I in $(seq -4 0)
  do
    echo -n -e "${I}\t"
    BEGIN="$(date +%s)"
    command time -f '%S\t%U\t%e' ./BenchmarkTDBH print "${1}" +RTS -M3.5G 2>&1 >/dev/null |  \
      ( IFS='' read -r TIME; echo -n "${TIME}"; )
    END="$(date +%s)"
    echo -e "\t$((END - BEGIN))\t0\t0\t0\t0\t0\t0\t0\t0\t0"
  done

  declare -i I=1
  while IFS='' read -r
  do
    echo -n -e "${I}\t"
    BEGIN="$(date +%s)"
    command time -f '%S\t%U\t%e' ./BenchmarkTDBH tdbh "${1}" "${REPLY}" +RTS -M3.5G 2>&1 >/dev/null |  \
      ( IFS='' read -r TIME; echo -n "${TIME}"; )
    END="$(date +%s)"
    echo -n -e "\t$((END - BEGIN))\t"
    ./BenchmarkTDBH tdbhStats "${1}" "${REPLY}" +RTS -M3.5G |  \
      (
        read x L1
        read x L2
        read x L3
        read x L4
        read x L5
        echo -e "${L1}\t${L2}\t${L3}\t${L4}\t${L5}"
      )
    I+=1
  done <"${2}"
}


G='hgs_tiger_release_aug07_notable_2000_utf-8/IntVertices'
Y='hgs_tiger_release_aug07_notable_2000_utf-8'
O='speedtest'

#speedtest  \
#  "${G}/hg_200_3.txt"  \
#  "${Y}/yields_5empty_1-200_401-600.txt"  \
#  > "${O}/200_3.txt"
#speedtest  \
#  "${G}/hg_200_3_reverse.txt"  \
#  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
#  > "${O}/200_3_reverse.txt"
#speedtest  \
#  "${G}/hg_200_3_binarized.txt"  \
#  "${Y}/yields_5empty_1-200_401-600.txt"  \
#  > "${O}/200_3_binarized.txt"
#speedtest  \
#  "${G}/hg_200_3_reverse_binarized.txt"  \
#  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
#  > "${O}/200_3_reverse_binarized.txt"
#
#speedtest  \
#  "${G}/hg_400_3.txt"  \
#  "${Y}/yields_5empty_1-200_401-600.txt"  \
#  > "${O}/400_3.txt"
#speedtest  \
#  "${G}/hg_400_3_reverse.txt"  \
#  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
#  > "${O}/400_3_reverse.txt"
#speedtest  \
#  "${G}/hg_400_3_binarized.txt"  \
#  "${Y}/yields_5empty_1-200_401-600.txt"  \
#  > "${O}/400_3_binarized.txt"
#speedtest  \
#  "${G}/hg_400_3_reverse_binarized.txt"  \
#  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
#  > "${O}/400_3_reverse_binarized.txt"
#
#
#speedtest  \
#  "${G}/hg_200_2.txt"  \
#  "${Y}/yields_5empty_1-200_401-600.txt"  \
#  > "${O}/200_2.txt"
#speedtest  \
#  "${G}/hg_200_2_reverse.txt"  \
#  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
#  > "${O}/200_2_reverse.txt"
#speedtest  \
#  "${G}/hg_200_2_binarized.txt"  \
#  "${Y}/yields_5empty_1-200_401-600.txt"  \
#  > "${O}/200_2_binarized.txt"
#speedtest  \
#  "${G}/hg_200_2_reverse_binarized.txt"  \
#  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
#  > "${O}/200_2_reverse_binarized.txt"
#
#speedtest  \
#  "${G}/hg_400_2.txt"  \
#  "${Y}/yields_5empty_1-200_401-600.txt"  \
#  > "${O}/400_2.txt"
#speedtest  \
#  "${G}/hg_400_2_reverse.txt"  \
#  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
#  > "${O}/400_2_reverse.txt"
#speedtest  \
#  "${G}/hg_400_2_binarized.txt"  \
#  "${Y}/yields_5empty_1-200_401-600.txt"  \
#  > "${O}/400_2_binarized.txt"
#speedtest  \
#  "${G}/hg_400_2_reverse_binarized.txt"  \
#  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
#  > "${O}/400_2_reverse_binarized.txt"
#
#
#speedtest  \
#  "${G}/hg_200_0.txt"  \
#  "${Y}/yields_5empty_1-200_401-600.txt"  \
#  > "${O}/200_0.txt"
#speedtest  \
#  "${G}/hg_200_0_reverse.txt"  \
#  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
#  > "${O}/200_0_reverse.txt"
#speedtest  \
#  "${G}/hg_200_0_binarized.txt"  \
#  "${Y}/yields_5empty_1-200_401-600.txt"  \
#  > "${O}/200_0_binarized.txt"
#speedtest  \
#  "${G}/hg_200_0_reverse_binarized.txt"  \
#  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
#  > "${O}/200_0_reverse_binarized.txt"
#
#speedtest  \
#  "${G}/hg_400_0.txt"  \
#  "${Y}/yields_5empty_1-200_401-600.txt"  \
#  > "${O}/400_0.txt"
speedtest  \
  "${G}/hg_400_0_reverse.txt"  \
  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
  > "${O}/400_0_reverse.txt"
speedtest  \
  "${G}/hg_400_0_binarized.txt"  \
  "${Y}/yields_5empty_1-200_401-600.txt"  \
  > "${O}/400_0_binarized.txt"
speedtest  \
  "${G}/hg_400_0_reverse_binarized.txt"  \
  "${Y}/yields_reversed_5empty_1-200_401-600.txt"  \
  > "${O}/400_0_reverse_binarized.txt"

