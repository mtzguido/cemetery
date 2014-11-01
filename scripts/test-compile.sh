#!/bin/bash

# This one is trickier, generate all source code and try to turn them
# into a .o. This will syntax and type check. We could have undefined
# symbols but hey, it's better than nothing.

set -ue

if ! [ -x ./cmt ]; then
	echo "cmt not found. You need to run 'make' first." >&2
	exit 1
fi

echo -n "Running compile test"

for i in tests/*.cmt; do
	if ! ./cmt "$i" >/dev/null ; then
		echo
		echo "TEST $i FAILED!"
		echo "Command was: ./cmt \"$i\""
		exit 1
	fi

	if ! gcc -c "${i/.cmt/.c}" -o /dev/null ; then
		echo
		echo "TEST $i FAILED TO COMPILE"
		exit 1
	fi

	echo -n '.'
done

echo " OK"
