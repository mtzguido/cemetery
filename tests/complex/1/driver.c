#include <stdio.h>

char *msg = "Hey, this is my secret message";
char *key = "dulce de leche";

int main() {
	int i;
	cmt_buf_t p = cmt_mkbuf(msg, strlen(msg));
	cmt_buf_t k = cmt_mkbuf(key, strlen(key));
	cmt_buf_t c = repkeyxor(p, k);

	for (i = 0; i < cmt_length(c); i++)
		printf("%02x", c->data[i]);
	printf("\n");

	return 0;
}
