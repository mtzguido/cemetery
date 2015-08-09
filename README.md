# The Cemetery Language

## What is it?

The idea is to have a yacc-like tool to generate cryptographic algorithms
from a language on which it's easy to think about them. Handling the buffers
when writing a cryptographic algorithm in C is very cumbersome, even if the
algorithm is a very simple one.

Cemetery tries to be a solution to that problem by allowing you to write
the algorithms in a clear language, which can then be compiled to C code.

Example (not actually Cemetery-generated):

Cemetery source:

	fun coll (x : int) : int
		if x == 1
			return 1
		else if even(x)
			return coll(x/2)
		else
			return coll(3*x +1)

Output C code:

	int coll(int x)
	{
		if (x == 1)
			return 1;
		else if (__cmt_even(x))
			return coll(x / 2);
		else
			return coll(3 * x + 1);
	}
