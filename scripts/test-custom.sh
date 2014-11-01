#!/bin/bash

set -ue

FLAGS="$@"

if ! [ -x ./cmt ]; then
	echo "cmt not found. You need to run 'make' first." >&2
	exit 1
fi

echo -n "Running tests with <$FLAGS>: "

for i in tests/*.cmt; do
	if ! ./cmt $FLAGS "$i" >/dev/null ; then
		echo
		echo "TEST $i FAILED!"
		echo "Command was: ./cmt \"$FLAGS\" \"$i\""
		exit 1
	fi
	echo -n '.'
done

echo " OK"
