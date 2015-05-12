#!/bin/bash

set -e

GOOD_DIR=$1
BAD_DIR=$2 # (optional)
build=

shift 2

if [ "$1" == '--build' ]; then
	build=1
	shift
fi

FLAGS="$@"

if ! [ -x ./cmt ]; then
	echo "cmt not found. You need to run 'make' first." >&2
	exit 1
fi

if [ "$build" != "" ]; then
	printf "%-30s" "BUILDING tests with <$FLAGS>:"
else
	printf "%-30s" "Running tests with <$FLAGS>:"
fi

for i in ${GOOD_DIR}/*.cmt; do
	if ! [ -f "$i" ] ; then continue ; fi

	if ! ./cmt $FLAGS "$i" &>/dev/null ; then
		echo
		echo "TEST $i FAILED!"
		echo "Command was: ./cmt \"$FLAGS\" \"$i\""
		exit 1
	fi

	if [ "$build" != "" ]; then
		indent -kr -i8 "${i/.cmt/.c}"
		gcc -c "${i/.cmt/.c}" -o /dev/null
	fi
	echo -n '.'
done

echo -n ' '

if [ "${BAD_DIR}" != "" ]; then
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
fi

echo " OK"
