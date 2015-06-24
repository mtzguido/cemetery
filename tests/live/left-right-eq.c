void dump(cmt_bits_t b)
{
	int i;

	fprintf(stderr, "Length: %i\n", b->length);

	for (i = b->length - 1; i >= 0; i--)
		fprintf(stderr, "%c", get(b, i) ? '1' : '0');

	fprintf(stderr, "\n");
}

int main()
{
	cmt_bits_t l = left();
	cmt_bits_t r = right();
	int i;

	if (l->length != r->length) {
		fprintf(stderr, "Length mismatch\n");
		dump(l);
		dump(r);
		return 1;
	}

	for (i = 0; i < l->length; i++) {
		if (get(l, i) != get(r, i)) {
			fprintf(stderr, "Bit sequences differ\n");
			dump(l);
			dump(r);
			return 1;
		}
	}

	return 0;
}
