#!/bin/bash

set -ue

build=false
fail=false

# Parse options
while [[ "$1" == "-"* ]]; do
	case "$1" in
	  "--fail")
		fail=true
		shift
		;;
	  "--build")
		build=true
		shift
		;;
	esac
done

DIR=$1
shift
FLAGS="$@"

if ! [ -d "${DIR}" ]; then
	# Just quietly bail out
	exit 0
fi

if ! [ -x ./cmt ]; then
	echo "cmt not found. You need to run 'make' first." >&2
	exit 1
fi

if $build; then
	msg="BUILDING"
else
	msg="Running "
fi

if $fail; then
	goodbad="bad "
else
	goodbad="good"
fi

printf "%-40s" "${msg} ${goodbad} tests with <${FLAGS}>:"

for i in ${DIR}/*.cmt; do
	if ! [ -f "$i" ] ; then continue ; fi

	if ./cmt $FLAGS "$i" &>/dev/null; then
		res=true
	else
		res=false
	fi
	# Poor man's XOR
	if (! $res && ! $fail) || ($res && $fail); then
		echo
		echo "TEST $i FAILED!"
		echo "Command was: ./cmt \"$FLAGS\" \"$i\""
		exit 1
	fi

	if $build; then
		indent -kr -i8 "${i/.cmt/.c}"
		rm -f "${i/.cmt/.c~}"
		gcc -c "${i/.cmt/.c}" -o /dev/null
	fi
	echo -n '.'
done

echo " OK"
