/*
 * Code autogenerated by the Cemetery compiler
 *
 * Complains go to:
 *   Guido Martínez
 *   https://github.com/mtzguido/cemetery
 *   mtzguido@gmail.com
 */

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

struct __cmt_buf {
	ssize_t length;
	uint8_t data[];
};

#define __cmt_min(x, y) ({			\
	typeof(x) _min1 = (x);			\
	typeof(y) _min2 = (y);			\
	(void) (&_min1 == &_min2);		\
	_min1 < _min2 ? _min1 : _min2; })

typedef struct __cmt_buf *cmt_buf_t;

static inline ssize_t __cmt_length(cmt_buf_t buf)
{
	return buf->length;
}

cmt_buf_t cmt_alloc(ssize_t length)
{
	cmt_buf_t ret;

	ret = malloc(sizeof *ret + length);
	ret->length = length;

	return ret;
}

static inline cmt_buf_t cmt_mkbuf(void *data, ssize_t length)
{
	cmt_buf_t ret = cmt_alloc(length);

	memcpy(ret->data, data, length);

	return ret;
}

static inline cmt_buf_t cmt_realloc(cmt_buf_t buf, ssize_t length)
{
	cmt_buf_t ret;

	ret = realloc(buf, length);
	assert(ret);

	return ret;
}

cmt_buf_t __cmt_xor(cmt_buf_t l, cmt_buf_t r)
{
	cmt_buf_t ret;
	ssize_t length;
	int i;

	length = __cmt_min(__cmt_length(l), __cmt_length(r));
	ret = cmt_alloc(length);

	for (i = 0; i < length; i++)
		ret->data[i] = l->data[i] ^ r->data[i];

	return ret;
}

cmt_buf_t __cmt_trunc(cmt_buf_t buf, ssize_t length)
{
	ssize_t old_length = __cmt_length(buf);

	cmt_realloc(buf, sizeof *buf + length);

	if (length > old_length)
		memset(buf->data + old_length, 0, length - old_length);

	return buf;
}

cmt_buf_t __cmt_repeat(cmt_buf_t buf, ssize_t length)
{
	ssize_t old_length = __cmt_length(buf);
	ssize_t off;

	cmt_realloc(buf, sizeof *buf + length);

	length -= old_length;
	off = old_length;

	while (length > 0) {
		memcpy(buf->data + off, buf->data,
				__cmt_min(length, old_length));
		length -= old_length;
		off += old_length;
	}

	return buf;
}

/* End of autogenerated code */
/* ------------------------- */

