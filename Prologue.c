#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

struct __cmt_buf {
	size_t length;
	uint8_t data[];
};

typedef struct __cmt_buf *cmt_buf_t;
