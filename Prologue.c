/* Cemetery prologue */

struct cmt_bits {
	int length;
	unsigned char data[];
};

typedef struct cmt_bits *cmt_bits_t;

int __cmt_even(int x)
{
	return !(x&1);
}

cmt_bits_t __cmt_band(cmt_bits_t l, cmt_bits_t r);
cmt_bits_t __cmt_bor(cmt_bits_t l, cmt_bits_t r);
cmt_bits_t __cmt_bconcat(cmt_bits_t l, cmt_bits_t r);
cmt_bits_t __cmt_xor(cmt_bits_t l, cmt_bits_t r);
cmt_bits_t __cmt_slice(cmt_bits_t l, int, int);
cmt_bits_t __cmt_bnot(cmt_bits_t e);

cmt_bits_t __cmt_permute(cmt_bits_t e, int[]);
cmt_bits_t __cmt_permute_inv(cmt_bits_t e, int[]);

cmt_bits_t __cmt_tobits(int, int);
int __cmt_toint(cmt_bits_t);

cmt_bits_t __cmt_shiftl(cmt_bits_t, int);
cmt_bits_t __cmt_shiftr(cmt_bits_t, int);
cmt_bits_t __cmt_rotl(cmt_bits_t, int);
cmt_bits_t __cmt_rotr(cmt_bits_t, int);

int __cmt_length(cmt_bits_t);

cmt_bits_t __cmt_init(struct cmt_bits*);

/* / Cemetery prologue */
