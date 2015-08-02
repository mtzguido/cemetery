void dump(cmt_bits_t b)
{
	int i;

	fprintf(stderr, "Length: %i\n", b->length);

	for (i = (b->length + 3) / 4; i > 0; i--) {
		int t = 0;
		t = 2 * t + get_bit(b, 4 * i - 1);
		t = 2 * t + get_bit(b, 4 * i - 2);
		t = 2 * t + get_bit(b, 4 * i - 3);
		t = 2 * t + get_bit(b, 4 * i - 4);
		fprintf(stderr, "%x", t);
	}

	fprintf(stderr, "\n");
}

int main()
{
	cmt_bits_t l = left();
	cmt_bits_t r = right();
	int ret;
	int i;

	if (l->length != r->length) {
		fprintf(stderr, "\nLength mismatch\n");
		dump(l);
		dump(r);
		ret = 1;
		goto out;
	}

	for (i = 0; i < l->length; i++) {
		if (get_bit(l, i) != get_bit(r, i)) {
			fprintf(stderr, "\nBit sequences differ\n");
			dump(l);
			dump(r);
			ret = 1;
			goto out;
		}
	}

	ret = 0;
out:

	cmt_free(l);
	cmt_free(r);
	return ret;
}
