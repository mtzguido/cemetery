# The Cemetery Language

## What is it?

The idea is to have a yacc-like tool to generate cryptographic algorithms
from a language on which it's easy to think about them. Handling the buffers
when writing a cryptographic algorithm in C is very cumbersome, even if the
algorithm is a very simple one.

Cemetery tries to be a solution to that problem by allowing you to write
the algorithms in a clear language, which can then be compiled to C code.

Example:

Cemetery source:

	static fun pad(m : bits) : bits
		var l = length(m)
		var k = (447 - l) % 512
		var r : bits

		r = m ||| <1 :: 1> ||| <0 :: k> ||| tobits(l, 64)

		return r

Output C code:

	static cmt_bits_t pad(cmt_bits_t m)
	{
		int l, k;
		cmt_bits_t r, t0, t1, t2, t3, t4, t5;

		l = cmt_length(m);
		k = __cmt_mod(447 - l, 512);
		t0 = __cmt_init(&__cmt_buf_literal_0, 1);
		t1 = __cmt_bconcat(m, t0);
		cmt_free(t0);
		t2 = __cmt_zero(k);
		t3 = __cmt_bconcat(t1, t2);
		cmt_free(t1);
		cmt_free(t2);
		t4 = __cmt_tobits(l, 64);
		t5 = __cmt_bconcat(t3, t4);
		cmt_free(t3);
		cmt_free(t4);
		r = t5;
		return r;
	}
