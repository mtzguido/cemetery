/* Cemetery prologue */

struct cmt_bits;
typedef struct cmt_bits *cmt_bits_t;

int __cmt_even(int x)
{
	return !(x&1);
}

cmt_bits_t __cmt_band(cmt_bits_t l, cmt_bits_t r);
cmt_bits_t __cmt_bor(cmt_bits_t l, cmt_bits_t r);
cmt_bits_t __cmt_bnot(cmt_bits_t e);

/* / Cemetery prologue */
