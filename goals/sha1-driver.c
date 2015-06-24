struct cmt_bits empty = {.data = {}, .length = 0};

int main() {
	int i;

	cmt_bits_t crap = __cmt_init(&empty);
	cmt_bits_t hash = sha1(crap);


	printf("length: %i\n", hash->length);
	printf("data: ");
	for (i = 0; i < hash->length; i++)
		printf("%i", get(hash, i) ? 1 : 0);

	printf("\n");
}
