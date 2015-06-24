#!/bin/bash

set -ue

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

printf "%-40s" "Running LIVE tests with <${FLAGS}>:"

for i in ${DIR}/*; do
	if ! [ -d "$i" ] ; then continue ; fi

	if ! ./cmt $FLAGS $i/code.cmt &>/dev/null; then
		echo
		echo "TEST $i FAILED TO BE TRANSLATED!"
		exit 1
	fi

	cat $i/code.c $i/driver.c > $i/full.c
	if ! gcc $i/full.c -o $i/full; then
		echo
		echo "TEST $i FAILED TO COMPILE!"
		exit 1
	fi

	if ! ./$i/full; then
		echo
		echo "TEST $i FAILED WHEN RUNNING!"
		exit 1
	fi

	echo -n '.'
done

echo " OK"
