typedef unsigned long word_t;

struct cmt_bits;
struct cmt_init;

typedef struct cmt_bits *cmt_bits_t;

cmt_bits_t cmt_mkbuf(const char *d, int bitlen);
void cmt_frombuf(char *dest, cmt_bits_t b);

void cmt_copy(char *dest, cmt_bits_t b);
void cmt_free(cmt_bits_t b);
