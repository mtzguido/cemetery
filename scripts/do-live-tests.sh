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
	R=.

	if [ $R == . ] && ! ./cmt $FLAGS $i/code.cmt &>/dev/null; then
		R=t
		goto
	fi

	cat $i/code.c $i/driver.c > $i/full.c
	if [ $R == . ] && ! gcc -Wsign-compare $i/full.c -o $i/full; then
		R=c
	fi

	if [ $R == . ] && ! ./$i/full; then
		R=r
	fi
	if [ $R == . ] && ! valgrind --error-exitcode=1 --leak-check=full ./$i/full &>/dev/null; then
		R=l
	fi

	echo -n $R
done

echo " OK"
