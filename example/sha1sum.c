#include <stdio.h>
#include <stdlib.h>

struct cmt_bits;
typedef struct cmt_bits *cmt_bits_t;

cmt_bits_t sha1(cmt_bits_t);
cmt_bits_t cmt_mkbuf(const char *, int);
void cmt_copy(char *, cmt_bits_t);

int main(int argc, char **argv)
{
	cmt_bits_t b, h;
	FILE *f;
	char *buf;
	int size;
	int r, t;
	int i;
	unsigned char hash[20];

	if (argc != 2) {
		fprintf(stderr, "usage: %s <filename>\n", argv[0]);
		exit(1);
	}

	f = fopen(argv[1], "rb");

	fseek(f, 0, SEEK_END);
	size = ftell(f);
	fseek(f, 0, SEEK_SET);

	buf = malloc(size);
	if (!buf) {
		fprintf(stderr, "OOM\n");
		exit(1);
	}

	r = 0;
	while (r < size) {
		t = fread(buf + r, 1, size - r, f);
		if (t < 0) {
			perror("read");
			exit(1);
		}
		r += t;
	}

	b = cmt_mkbuf(buf, 8 * size);
	h = sha1(b);
	cmt_copy(hash, h);

	for (i = 0; i < 20; i++)
		printf("%02x", hash[i]);
	printf("\n");

	cmt_free(h);
	cmt_free(b);
	free(buf);
	fclose(f);

	return 0;
}
