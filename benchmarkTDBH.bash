#!/bin/bash


# { date; ./BenchmarkTDBH +RTS -s -M1.5G -RTS train 99 2000; date; } 2>&1 | tee log.txt
# ../BenchmarkTDBH preprocess hg_2000_0.txt
# 
# ../BenchmarkTDBH test hg_2000_0_IntVertices.txt 0 True
# 
# ./BenchmarkTDBH printYields
# 
# for a in bha bhb bhc; do for b in False True; do ./BenchmarkTDBH synthBench 4 4 $b stats $a +RTS -s; done; done
# 
# for i in $(seq 2001 3000); do ../BenchmarkTDBH benchmark hg_2000_3_IntVertices.txt yields.txt $i bha pretend +RTS -s -M200M; done


# set -e -u -C
set -u -C
set -o pipefail

RTS="-M3.5G"

filename='benchmarkTDBH_synthBench'

declare -i n m

declare -i stop=0
trap 'stop=1; echo >&2; echo "Aborting safely." >&2' SIGINT

nl=$'\n'

{
echo -n "["
separator=""
for (( n=2; n <= 10; n+=1 ))
do
  for (( m=1; m <= 32 - n; m+=1 ))
  do
    echo "m = $m; n = $n" >&2
    out="${separator}${nl}"
    out+="[ (\"a_\", [(\"m\", \"${m}\"), (\"n\", \"${n}\")])${nl}"
    (( ${stop} )) && break
    for b in False True
    do
      o="$(./BenchmarkTDBH synthBench $m $n $b pretend bha +RTS -t --machine-readable ${RTS} 2>&1 >/dev/null | tail -n+2)" || o="[]"
      out+=", (\"pretend_${b}_\", ${o})${nl}"
      (( ${stop} )) && break
      for a in bha bhb # bhc
      do
        o="$(./BenchmarkTDBH synthBench $m $n $b bench $a +RTS -t --machine-readable ${RTS} 2>&1 >/dev/null | tail -n+2)" || o="[]"
        out+=", (\"bench_${b}_${a}_\", ${o})${nl}"
        (( ${stop} )) && break
        o="$(./BenchmarkTDBH synthBench $m $n $b stats $a +RTS -t --machine-readable ${RTS} 2>/dev/null)" || o="[]"
        out+=", (\"stats_${b}_${a}_\", ${o})${nl}"
        (( ${stop} )) && break
      done
      (( ${stop} )) && break
    done
    (( ${stop} )) && break
    out+="]"
    separator=","
    echo "${out}"
  done
  (( ${stop} )) && break
done
echo "]"
} >"${filename}.data"

./BenchmarkdataToCSV <"${filename}.data" >"${filename}.csv" || true



filename='benchmarkTDBH_synthBench2'
{
echo -n "["
separator=""
for (( n=1; n <= 200; n+=1 ))
do
  out="${separator}${nl}"
  out+="[ (\"a_\", [(\"n\", \"${n}\")])${nl}"
  (( ${stop} )) && break
  for b in False True
  do
    o="$(./BenchmarkTDBH synthBench2 $n $b pretend bha +RTS -t --machine-readable ${RTS} 2>&1 >/dev/null | tail -n+2)" || o="[]"
    out+=", (\"pretend_${b}_\", ${o})${nl}"
    (( ${stop} )) && break
    for a in bha bhb # bhc
    do
      echo "n = $n; $b; $a" >&2
      o="$(./BenchmarkTDBH synthBench2 $n $b bench $a +RTS -t --machine-readable ${RTS} 2>&1 >/dev/null | tail -n+2)" || o="[]"
      out+=", (\"bench_${b}_${a}_\", ${o})${nl}"
      (( ${stop} )) && break
      o="$(./BenchmarkTDBH synthBench2 $n $b stats $a +RTS -t --machine-readable ${RTS} 2>/dev/null)" || o="[]"
      out+=", (\"stats_${b}_${a}_\", ${o})${nl}"
      (( ${stop} )) && break
    done
    (( ${stop} )) && break
  done
  (( ${stop} )) && break
  out+="]"
  separator=","
  echo "${out}"
done
echo "]"
} >"${filename}.data"

./BenchmarkdataToCSV <"${filename}.data" >"${filename}.csv" || true



filename='benchmarkTDBH_benchmark_yieldsRandom_binarized_'
{
echo -n "["
separator=""
for (( n=0; n <= 113; n+=1 ))
do
  out="${separator}${nl}"
  out+="[ (\"a_\", [(\"n\", \"${n}\")])${nl}"
  (( ${stop} )) && break
  for g in hg_2000_0_IntVertices_binarized_.txt hg_2000_3_IntVertices_binarized_.txt
  do
    echo "n = $n; $g; pretend" >&2
    o="$(./BenchmarkTDBH benchmark "${g}" yieldsRandom.txt "${n}" pretend bha +RTS -t --machine-readable ${RTS} 2>&1 >/dev/null | tail -n+2)" || o="[]"
    out+=", (\"pretend_${g}_\", ${o})${nl}"
    (( ${stop} )) && break
    for a in bha # bhb # bhc
    do
      echo "n = $n; $g; $a benchmark" >&2
      o="$(./BenchmarkTDBH benchmark "${g}" yieldsRandom.txt "${n}" bench ${a} +RTS -t --machine-readable ${RTS} 2>&1 >/dev/null | tail -n+2)" || o="[]"
      out+=", (\"bench_${g}_${a}_\", ${o})${nl}"
      (( ${stop} )) && break
      echo "n = $n; $g; $a stats" >&2
      o="$(./BenchmarkTDBH benchmark "${g}" yieldsRandom.txt "${n}" stats ${a} +RTS -t --machine-readable ${RTS} 2>/dev/null)" || o="[]"
      out+=", (\"stats_${g}_${a}_\", ${o})${nl}"
      (( ${stop} )) && break
    done
    (( ${stop} )) && break
  done
  (( ${stop} )) && break
  out+="]"
  separator=","
  echo "${out}"
done
echo "]"
} >"${filename}.data"

./BenchmarkdataToCSV <"${filename}.data" >"${filename}.csv" || true


filename='benchmarkTDBH_benchmark_yieldsRandom'
{
echo -n "["
separator=""
for (( n=0; n <= 98; n+=1 ))
do
  out="${separator}${nl}"
  out+="[ (\"a_\", [(\"n\", \"${n}\")])${nl}"
  (( ${stop} )) && break
  for g in hg_2000_0_IntVertices.txt hg_2000_0_IntVertices_binarized_.txt hg_2000_3_IntVertices.txt hg_2000_3_IntVertices_binarized_.txt
  do
    echo "n = $n; $g; pretend" >&2
    o="$(./BenchmarkTDBH benchmark "${g}" yieldsRandom.txt "${n}" pretend bha +RTS -t --machine-readable ${RTS} 2>&1 >/dev/null | tail -n+2)" || o="[]"
    out+=", (\"pretend_${g}_\", ${o})${nl}"
    (( ${stop} )) && break
    for a in bha bhb bhc
    do
      echo "n = $n; $g; $a benchmark" >&2
      o="$(./BenchmarkTDBH benchmark "${g}" yieldsRandom.txt "${n}" bench ${a} +RTS -t --machine-readable ${RTS} 2>&1 >/dev/null | tail -n+2)" || o="[]"
      out+=", (\"bench_${g}_${a}_\", ${o})${nl}"
      (( ${stop} )) && break
      echo "n = $n; $g; $a stats" >&2
      o="$(./BenchmarkTDBH benchmark "${g}" yieldsRandom.txt "${n}" stats ${a} +RTS -t --machine-readable ${RTS} 2>/dev/null)" || o="[]"
      out+=", (\"stats_${g}_${a}_\", ${o})${nl}"
      (( ${stop} )) && break
    done
    (( ${stop} )) && break
  done
  (( ${stop} )) && break
  out+="]"
  separator=","
  echo "${out}"
done
echo "]"
} >"${filename}.data"

./BenchmarkdataToCSV <"${filename}.data" >"${filename}.csv" || true
