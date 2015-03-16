#!/bin/bash

set -ue

GOOD_DIR=$1
BAD_DIR=$2

shift 2
FLAGS="$@"

if ! [ -x ./cmt ]; then
	echo "cmt not found. You need to run 'make' first." >&2
	exit 1
fi

printf "%-30s" "Running tests with <$FLAGS>:"

for i in ${GOOD_DIR}/*.cmt; do
	if ! [ -f "$i" ] ; then continue ; fi

	if ! ./cmt $FLAGS "$i" &>/dev/null ; then
		echo
		echo "TEST $i FAILED!"
		echo "Command was: ./cmt \"$FLAGS\" \"$i\""
		exit 1
	fi
	echo -n '.'
done

echo -n ' '

for i in ${BAD_DIR}/*.cmt; do
	if ! [ -f "$i" ] ; then continue ; fi

	if ./cmt $FLAGS "$i" &>/dev/null ; then
		echo
		echo "TEST $i FAILED!"
		echo "Command was: ./cmt \"$FLAGS\" \"$i\""
		exit 1
	fi
	echo -n '.'
done

echo " OK"
