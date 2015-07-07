/* Cemetery prologue */

typedef unsigned long word_t;

#define W	((int)sizeof(word_t))
#define WB	(W * 8)
#define bit(i)	((word_t)1 << (i))

#define mask_l(i)	(bit(i) - 1)
#define mask_m(i)	(~(mask_l(WB - i)))

struct cmt_bits {
	int length;
	int size;
	word_t data[];
};

struct cmt_init {
	int length;
	unsigned char data[];
};

typedef struct cmt_bits *cmt_bits_t;

static inline void __cmt_error(char *s)
{
	fprintf(stderr, "Cemetery error: %s\n", s);
	abort();
}

#define __cmt_assert(c)						\
	do {							\
		if (!(c))					\
			__cmt_error("ASSERT FAILED: " #c);	\
	} while(0);

static inline int max(int a, int b)
{
	return a > b ? a : b;
}

static int __cmt_mod(int a, int b)
{
	__cmt_assert(b > 0);

	/* Special case for powers of 2 */
	if (b & (b - 1) == 0)
		return a & (b - 1);

	return (a % b) + (a < 0 ? b : 0);
}

cmt_bits_t __cmt_alloc(int length)
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

void __cmt_fixup(cmt_bits_t b)
{
	word_t m;

	if (b->length % WB) {
		m = -1;
		m <<= b->length % WB;
		b->data[b->size - 1] &= ~m;
	}
}

bool get_bit(cmt_bits_t b, int o)
{
	if (o > b->length)
		return false;

	return b->data[o / WB] & bit(o % WB);
}

void set_bit(cmt_bits_t b, int o)
{
	if (o > b->length)
		__cmt_error("wat");

	b->data[o / WB] |= bit(o % WB);
}

word_t get_word(cmt_bits_t b, int wi)
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

void set_word(cmt_bits_t b, int wi, word_t w)
{
	__cmt_assert(wi >= 0);
	__cmt_assert(wi < (b->length + WB - 1)/ WB);

	b->data[wi] = w;
}

cmt_bits_t __cmt_band(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	int i;

	for (i = 0; i < ret->size; i++)
		set_word(ret, i, get_word(l, i) & get_word(r, i));

	return ret;
}

cmt_bits_t __cmt_bor(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	int i;

	for (i = 0; i < ret->size; i++)
		set_word(ret, i, get_word(l, i) | get_word(r, i));

	return ret;
}

cmt_bits_t __cmt_xor(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	int i;

	for (i = 0; i < ret->size; i++)
		set_word(ret, i, get_word(l, i) ^ get_word(r, i));

	return ret;
}

static inline word_t mask_set(word_t v, word_t on, word_t off)
{
	return (v & ~off) | on;
}

static void __cmt_bitcopy_iter(cmt_bits_t to, int offset,
			       cmt_bits_t from, int start_bit, int len)
{
	int i;

	if (!len)
		return;

	if (start_bit / WB == (start_bit + len - 1) / WB &&
	    offset / WB == (offset + len - 1) / WB) {
		word_t t;
		word_t w;
		word_t m;
		int s;


		w = from->data[start_bit / WB];

		t = to->data[offset / WB];

		w >>= start_bit % WB;

		m = mask_l(len);
		w &= m;

		w <<= offset % WB;
		m <<= offset % WB;

		t = mask_set(t, w, m);

		to->data[offset / WB] = t;
		return;
	}

	for (i = 0; i < len; i++) {
		if (get_bit(from, start_bit + i))
			set_bit(to, offset + i);
	}
}

static void __cmt_bitcopy(cmt_bits_t to, int offset,
			  cmt_bits_t from, int start_bit, int len)
{
	int fw, lw;
	int w;
	int lskip, rskip;
	word_t *T, *F;
	int L, R;
	int n;

	if (!len)
		return;

	fw = (offset + WB - 1) / WB;
	lw = (offset + len) / WB;
	lskip = fw * WB - offset;
	rskip = offset +len - lw * WB;

	T = &to->data[fw];
	F = &from->data[(start_bit + lskip) / WB];

	L = (start_bit + lskip) % WB;
	R = WB - L;

	n = lw - fw;

	if (n <= 0) {
		__cmt_bitcopy_iter(to, offset, from, start_bit, len);
		return;
	}

	if (L == 0) {
		for (w = 0; w < n; w++) {
			word_t t = T[w];
			t = mask_set(t, F[w], (word_t)(-1));
			T[w] = t;
		}
	} else {
		for (w = 0; w < n; w++) {
			word_t t = T[w];
			t = mask_set(t, F[w] >> L, mask_l(R));
			t = mask_set(t, F[w +1] << R, mask_m(L));
			T[w] = t;
		}
	}

	__cmt_bitcopy_iter(to, offset, from, start_bit, lskip);
	__cmt_bitcopy_iter(to, offset + len - rskip, from,
			   start_bit + len - rskip, rskip);
}

cmt_bits_t __cmt_bconcat(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(l->length + r->length);

	__cmt_bitcopy(ret, 0,         r, 0, r->length);
	__cmt_bitcopy(ret, r->length, l, 0, l->length);

	return ret;
}

cmt_bits_t __cmt_slice(cmt_bits_t l, int from, int to)
{
	int tob;
	cmt_bits_t ret = __cmt_alloc(to - from + 1);

	__cmt_assert(from >= 0);
	__cmt_assert(from < l->length);
	__cmt_assert(to >= 0);
	__cmt_assert(to < l->length);

	/*
	 * Slice takes bits counted from the left (MSB),
	 * so turn them into an index from the right (LSB)
	 */
	tob = l->length - 1 - to;

	__cmt_bitcopy(ret, 0, l, tob, ret->length);

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

cmt_bits_t __cmt_permute(cmt_bits_t e, int perm[], int len)
{
	cmt_bits_t ret = __cmt_alloc(len);
	int i;

	for (i = 0; i < len; i++) {
		if (get_bit(e, e->length - perm[i]))
			set_bit(ret, len - 1 - i);
	}

	return ret;
}

cmt_bits_t __cmt_tobits(int x, int len)
{
	cmt_bits_t ret = __cmt_alloc(len);
	int i = 0;

	ret->data[0] = x;

	return ret;
}

int __cmt_toint(cmt_bits_t b)
{
	return b->data[0];
}

cmt_bits_t __cmt_shiftl(cmt_bits_t b, int s)
{
	__cmt_assert(s >= 0);

	cmt_bits_t ret = __cmt_alloc(b->length + s);
	__cmt_bitcopy(ret, s, b, 0, b->length);

	return ret;
}

cmt_bits_t __cmt_shiftr(cmt_bits_t b, int s)
{
	__cmt_assert(s >= 0);

	if (s > b->length)
		return __cmt_alloc(0);

	cmt_bits_t ret = __cmt_alloc(b->length - s);
	__cmt_bitcopy(ret, 0, b, s, b->length - s);

	return ret;
}

cmt_bits_t __cmt_rotl(cmt_bits_t b, int s)
{
	cmt_bits_t ret = __cmt_alloc(b->length);
	s = __cmt_mod(s, b->length);

	__cmt_bitcopy(ret, 0, b, b->length - s, s);
	__cmt_bitcopy(ret, s, b, 0, b->length - s);

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

cmt_bits_t __cmt_modplus(cmt_bits_t l, cmt_bits_t r)
{
	cmt_bits_t ret = __cmt_alloc(max(l->length, r->length));
	word_t c = 0, cc;
	int i;

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

bool __cmt_eq(cmt_bits_t l, cmt_bits_t r)
{
	if (l->length != r->length)
		return false;

	__cmt_assert(l->size == r->size);
	return !memcmp(l->data, r->data, l->size);
}

cmt_bits_t __cmt_zero(int l)
{
	return __cmt_alloc(l);
}

cmt_bits_t __cmt_copy(cmt_bits_t b)
{
	cmt_bits_t ret = __cmt_alloc(b->length);
	__cmt_bitcopy(ret, 0, b, 0, b->length);

	return ret;
}

cmt_bits_t cmt_mkbuf(const char *d, int bitlen)
{
	cmt_bits_t ret = __cmt_alloc(bitlen);
	char *t = (char*)ret->data;
	int i;

	__cmt_assert(bitlen % 8 == 0);

	for (i = 0; i < bitlen / 8; i++)
		t[bitlen/8 - 1 - i] = d[i];

	return ret;
}

void cmt_copy(char *dest, cmt_bits_t b)
{
	char *f = (char*)b->data;
	int i;
	int l = b->length / 8;

	__cmt_assert(b->length % 8 == 0);

	for (i = 0; i < l; i++)
		dest[i] = f[l - 1 - i];
}

void __cmt_free(cmt_bits_t b)
{
	free(b);
}

#undef W
#undef WB
#undef bit

#undef mask_l
#undef mask_m

#undef __cmt_assert

/* / Cemetery prologue */
