#!/bin/bash

# Copyright (c) 2011, Toni Dietze

# set -e -u -C


function speedtestCore() {
	echo "${@}" >&2

	for I in $(seq -4 0)
	do
		echo -n -e "${I}\t"
		BEGIN="$(date +%s)"
		command time -f '%S\t%U\t%e' ./"${1}" readWTA "${2}" +RTS -M3.5G 2>&1 >/dev/null |  \
			( IFS='' read -r TIME; echo -n "${TIME}"; )
		END="$(date +%s)"
		echo -e "\t$((END - BEGIN))\t-1\t-1\t-1\t-1\t-1\t-1\t-1\t-1\t-1"
	done

	declare -i I=1
	S='[""]'
	declare -i E=$((30 * 60 + $(date +%s)))
	while [ "$(date +%s)" -le "${E}" ]
	do
		echo "${I}, ${S}" >&2
		{
			command time -f '%S\t%U\t%e' ./"${1}" tdbh "${2}" "${S}" +RTS -M3.5G 2>&1 >/dev/null |  \
				( IFS='' read -r TIME; echo -e -n "${I}\t${TIME}"; )
			killall sleep
		} &
		sleep 10m && killall -KILL "${1}" && break
		echo -n -e "\t-1\t"
		./"${1}" tdbhStats "${2}" "${S}" +RTS -M3.5G |  \
			(
				read x L1
				read x L2
				read x L3
				read x L4
				read x L5
				echo -e "${L1}\t${L2}\t${L3}\t${L4}\t${L5}"
			)
		I+=1
		S="${S%]},\"\"]"
	done
}


function speedtest() {
	speedtestCore "${@}" > "speedtestNew/${1}_${2##*/}"
}


G='hgs_tiger_release_aug07_notable_2000_utf-8/IntVertices'
Y='hgs_tiger_release_aug07_notable_2000_utf-8'


speedtest  \
	'BenchmarkComplete'  \
	"${G}/hg_400_3.txt"  \
	"${Y}/yields_5empty_1-200_401-600.txt"
speedtest  \
	'BenchmarkComplete'  \
	"${G}/hg_400_3_binarized.txt"  \
	"${Y}/yields_5empty_1-200_401-600.txt"

speedtest  \
	'BenchmarkComplete'  \
	"${G}/hg_400_0.txt"  \
	"${Y}/yields_5empty_1-200_401-600.txt"
speedtest  \
	'BenchmarkComplete'  \
	"${G}/hg_400_0_binarized.txt"  \
	"${Y}/yields_5empty_1-200_401-600.txt"
