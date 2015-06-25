/* Cemetery prologue */

#include <stddef.h>

#define W  ((int)sizeof(unsigned long))
#define WB (W * 8)
#define bit(i) ((unsigned long)1 << (i))

struct cmt_bits {
	int length;
	int size;
	unsigned long data[];
};

struct cmt_init {
	int length;
	unsigned char data[];
};

typedef struct cmt_bits *cmt_bits_t;

static int __cmt_even(int x)
{
	return !(x&1);
}

static inline int max(int a, int b)
{
	return a > b ? a : b;
}

static int __cmt_mod(int a, int b)
{
	if (a == -1)
		return b - 1;

	if (a < 0)
		a += b * (-a/b + 1);

	return a % b;
}

static inline __cmt_error(char *s)
{
	fprintf(stderr, "Cemetery error: %s\n", s);
	abort();
}

static cmt_bits_t __cmt_alloc(int length)
{
	cmt_bits_t ret;
	int size;

	size = (length + WB - 1) / WB;

	ret = calloc(1, offsetof(struct cmt_bits, data[size]));

	if (!ret)
		__cmt_error("allocation failed\n");

	ret->length = length;
	ret->size = size;

	return ret;
}

static void __cmt_fixup(cmt_bits_t b)
{
	unsigned long m;

	if (b->length % WB) {
		m = -1;
		m <<= b->length % WB;
		b->data[b->size - 1] &= ~m;
	}
}

static bool get_bit(cmt_bits_t b, int o)
{
	if (o > b->length)
		return false;

	return b->data[o / WB] & bit(o % WB);
}

static void set_bit(cmt_bits_t b, int o)
{
	if (o > b->length)
		__cmt_error("wat");

	b->data[o / WB] |= bit(o % WB);
}

static unsigned long get_word(cmt_bits_t b, int wi)
{
	if (wi < 0)
		__cmt_error("fuck 1");

	/*
	 * Allowed for now since it simplies the binops
	 * but, up for discussion wether we should fail
	 * hard here.
	 */
	if (wi >= b->size)
		return 0UL;

	return b->data[wi];
}

static void set_word(cmt_bits_t b, int wi, unsigned long w)
{
	if (wi < 0)
		__cmt_error("fuck 2");

	if (wi >= (b->length + WB - 1)/ WB)
		__cmt_error("fuck 3");

	b->data[wi] = w;
}

static cmt_bits_t __cmt_band(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	int i;

	for (i = 0; i < ret->size; i++)
		set_word(ret, i, get_word(l, i) & get_word(r, i));

	return ret;
}

static cmt_bits_t __cmt_bor(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	int i;

	for (i = 0; i < ret->size; i++)
		set_word(ret, i, get_word(l, i) | get_word(r, i));

	return ret;
}

static cmt_bits_t __cmt_xor(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	int i;

	for (i = 0; i < ret->size; i++)
		set_word(ret, i, get_word(l, i) ^ get_word(r, i));

	return ret;
}

static void __cmt_copy(cmt_bits_t to, int offset, cmt_bits_t from,
		       int start_bit, int len)
{
	int i;

	/* FIXME, TERRIBILY SLOW! */
	for (i = 0; i < len; i++) {
		if (get_bit(from, start_bit + i))
			set_bit(to, offset + i);
	}
}

static cmt_bits_t __cmt_bconcat(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(l->length + r->length);

	__cmt_copy(ret, 0,         r, 0, r->length);
	__cmt_copy(ret, r->length, l, 0, l->length);

	return ret;
}

cmt_bits_t __cmt_slice(cmt_bits_t l, int from, int to)
{
	int tob;
	cmt_bits_t ret = __cmt_alloc(to - from + 1);

	/*
	 * Slice takes bits counted from the left (MSB),
	 * so turn them into an index from the right (LSB)
	 */
	tob = l->length - 1 - to;

	__cmt_copy(ret, 0, l, tob, ret->length);

	return ret;
}

cmt_bits_t __cmt_bnot(cmt_bits_t e)
{
	cmt_bits_t ret = __cmt_alloc(e->length);
	int i;

	for (i = 0; i < ret->size; i++)
		set_word(ret, i, ~get_word(e, i));

	/* Fixup the trailing bits, they should always be 0 */
	__cmt_fixup(ret);

	return ret;
}

cmt_bits_t __cmt_permute(cmt_bits_t e, int perm[])
{
	cmt_bits_t ret = __cmt_alloc(64); /* FIXME! */
	int i;

	for (i = 0; i < ret->length; i++) {
		if (get_bit(e, perm[i]))
			set_bit(ret, i);
	}

	return ret;
}

cmt_bits_t __cmt_permute_inv(cmt_bits_t e, int perm[])
{
	cmt_bits_t ret = __cmt_alloc(64); /* FIXME! */
	int i;

	for (i = 0; i < ret->length; i++) {
		if (get_bit(e, i))
			set_bit(ret, perm[i]);
	}

	return ret;
}

cmt_bits_t __cmt_tobits(int x, int len)
{
	cmt_bits_t ret = __cmt_alloc(len);
	int i = 0;

	for (i = 0; i < len; i++) {
		if (!x)
			break;

		if (x & 1)
			set_bit(ret, i);

		x >>= 1;
	}

	return ret;
}

int __cmt_toint(cmt_bits_t b)
{
	int ret = 0;
	int i;

	for (i = b->length - 1; i >= 0; i--) {
		ret <<= 1;

		if (get_bit(b, i))
			ret += 1;
	}

	return ret;
}

cmt_bits_t __cmt_shiftl(cmt_bits_t b, int s)
{
	if (s < 0)
		abort();

	cmt_bits_t ret = __cmt_alloc(b->length + s);
	__cmt_copy(ret, s, b, 0, b->length);
	return ret;
}

cmt_bits_t __cmt_shiftr(cmt_bits_t b, int s)
{
	if (s < 0)
		abort;

	if (s > b->length)
		return __cmt_alloc(0);

	cmt_bits_t ret = __cmt_alloc(b->length - s);
	__cmt_copy(ret, 0, b, s, b->length - s);

	return ret;
}

cmt_bits_t __cmt_rotl(cmt_bits_t b, int s)
{
	cmt_bits_t ret = __cmt_alloc(b->length);
	s = __cmt_mod(s, b->length);

	__cmt_copy(ret, 0, b, b->length - s, s);
	__cmt_copy(ret, s, b, 0, b->length - s);

	return ret;
}

cmt_bits_t __cmt_rotr(cmt_bits_t b, int s)
{
	return __cmt_rotl(b, b->length - s);
}

int __cmt_length(cmt_bits_t b)
{
	return b->length;
}

cmt_bits_t __cmt_init(unsigned char *data, int length)
{
	cmt_bits_t ret = __cmt_alloc(length);
	memcpy(ret->data, data, (length + 7) / 8);
	return ret;
}

static cmt_bits_t __cmt_modplus(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	int i;
	unsigned long c = 0, cc;

	for (i = 0; i < ret->size; i++) {
		cc = c;
		c = c + get_word(l, i) + get_word(r, i);

		set_word(ret, i, c);

		if (c < cc)
			c = 1;
	}

	__cmt_fixup(ret);

	return ret;
}

cmt_bits_t __cmt_zero(int l)
{
	return __cmt_alloc(l);
}

#undef W
#undef WB

/* / Cemetery prologue */
