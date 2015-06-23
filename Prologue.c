/* Cemetery prologue */

#include <stddef.h>

struct cmt_bits {
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
	while (a < 0)
		a += b;

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

	ret = calloc(1, offsetof(struct cmt_bits, data[length / 8]));

	if (!ret)
		__cmt_error("allocation failed\n");

	ret->length = length;

	return ret;
}

static bool get(cmt_bits_t b, int o)
{
	if (o > b->length)
		__cmt_error("wat");

	return b->data[o/8] & (1 << (o % 8));
}

static void set(cmt_bits_t b, int o, int v)
{
	if (o > b->length)
		__cmt_error("wat");

	if (v)
		b->data[o/8] |= (1 << (o % 8));
	else
		b->data[o/8] &= ~(1 << (o % 8));
}

static void __cmt_copy(cmt_bits_t to, int offset, cmt_bits_t from)
{
	int i;

	for (i = 0; i < from->length; i++) {
		if (get(from, i))
			set(to, offset + i, 1);
	}
}

static cmt_bits_t __cmt_band(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	int i;

	for (i = 0; i < ret->length; i++) {
		if (i > l->length || i > r->length)
			break;

		set(ret, i, get(l, i) && get(r, i));
	}

	return ret;
}

static cmt_bits_t __cmt_bor(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	int i;

	for (i = 0; i < ret->length; i++) {
		if (i > l->length || i > r->length)
			break;

		set(ret, i, get(l, i) || get(r, i));
	}

	return ret;
}

static cmt_bits_t __cmt_xor(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	int i;

	for (i = 0; i < ret->length; i++) {
		if (i > l->length || i > r->length)
			break;

		set(ret, i, get(l, i) != get(r, i));
	}

	return ret;
}

static cmt_bits_t __cmt_bconcat(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(l->length + r->length);
	__cmt_copy(ret, 0, l);
	__cmt_copy(ret, l->length, r);

	return ret;
}

cmt_bits_t __cmt_slice(cmt_bits_t l, int from, int to)
{
	cmt_bits_t ret = __cmt_alloc(to - from + 1);
	int i;

	for (i = 0; i < ret->length; i++) {
		if (get(l, i + from))
			set(ret, i, 1);
	}

	return ret;
}

cmt_bits_t __cmt_bnot(cmt_bits_t e)
{
	cmt_bits_t ret = __cmt_alloc(e->length);
	int i;

	for (i = 0; i < e->length; i++) {
		if (!get(e, i))
			set(ret, i, 1);
	}

	return ret;
}

cmt_bits_t __cmt_permute(cmt_bits_t e, int perm[])
{
	cmt_bits_t ret = __cmt_alloc(64); /* FIXME! */
	int i;

	for (i = 0; i < ret->length; i++) {
		if (get(e, perm[i]))
			set(ret, i, 1);
	}

	return ret;
}

cmt_bits_t __cmt_permute_inv(cmt_bits_t e, int perm[])
{
	cmt_bits_t ret = __cmt_alloc(64); /* FIXME! */
	int i;

	for (i = 0; i < ret->length; i++) {
		if (get(e, i))
			set(ret, perm[i], 1);
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
			set(ret, i, 1);

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

		if (get(b, i))
			ret += 1;
	}

	return ret;
}

cmt_bits_t __cmt_shiftl(cmt_bits_t b, int s)
{
	cmt_bits_t ret = __cmt_alloc(b->length + s);
	int i;

	for (i = s; i < ret->length; i++)
		if (get(b, i - s))
			set(ret, i, 1);

	return ret;
}

cmt_bits_t __cmt_shiftr(cmt_bits_t b, int s)
{
	cmt_bits_t ret = __cmt_alloc(b->length - s);
	int i;

	for (i = 0; i < ret->length; i++)
		if (get(b, i + s))
			set(ret, i, 1);

	return ret;
}

cmt_bits_t __cmt_rotl(cmt_bits_t b, int s)
{
	cmt_bits_t ret = __cmt_alloc(b->length);
	int i;

	for (i = 0; i < ret->length; i++)
		if (get(b, __cmt_mod(i - s,  b->length)))
			set(ret, i, 1);

	return ret;
}

cmt_bits_t __cmt_rotr(cmt_bits_t b, int s)
{
	cmt_bits_t ret = __cmt_alloc(b->length);
	int i;

	for (i = 0; i < ret->length; i++)
		if (get(b, __cmt_mod(i + s, b->length)))
			set(ret, i, 1);

	return ret;
}

int __cmt_length(cmt_bits_t b)
{
	return b->length;
}

cmt_bits_t __cmt_init(struct cmt_bits *t)
{
	cmt_bits_t ret = __cmt_alloc(t->length);
	__cmt_copy(ret, 0, t);
	return ret;
}

static cmt_bits_t __cmt_modplus(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	int i;
	int c = 0;

	for (i = 0; i < ret->length; i++) {
		c /= 2;

		if (get(l, i))
			c++;
		if (get(r, i))
			c++;

		if (c & 1)
			set(ret, i, 1);
	}

	return ret;
}

cmt_bits_t __cmt_zero(int l)
{
	return __cmt_alloc(l);
}

/* / Cemetery prologue */
