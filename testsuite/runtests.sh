#!/bin/bash

set -Ceu

cd "${0%/*}"            ; declare -r scriptDir="${PWD}"
cd "${scriptDir}/../src"; declare -r srcDir="${PWD}"
cd "${scriptDir}/tests" ; declare -r testsDir="${PWD}"

declare -r tempDir="${scriptDir}/build"
mkdir -p "${tempDir}"
# declare -r tempDir="$(mktemp --directory --suffix=".${0##*/}")"
# trap 'rm -rf "${tempDir}"; trap - ERR EXIT' ERR EXIT

function runTest() {
	cd "${tempDir}"
	rm -f *.tix
	# ↱ workaround “Warning: Couldn't figure out linker information!”
	LANG=C ghc  \
		-Wall  \
		-fwarn-tabs  \
		-fwarn-incomplete-record-updates  \
		-fwarn-unused-do-bind  \
		-outputdir "${tempDir}"  \
		-i"${srcDir}"  \
		-i"${testsDir}"  \
		-fhpc  \
		-v0  \
		-main-is "${1}"  \
		-o tests  \
		--make "${@}"
	echo
	./tests || true
	echo
	hpc report tests
	echo
	hpc markup --hpcdir="../build/.hpc" --srcdir="${srcDir}" --srcdir="${testsDir}" tests >/dev/null  \
		&& echo 'You can find detailed html pages about code coverage in the following directory:'  \
		&& echo "${tempDir}"
}

runTest HUnitTests "${@}"
