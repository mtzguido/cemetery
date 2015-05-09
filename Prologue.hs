module Prologue where

hprologue = ""
cprologue = unlines [
    "#include <stdbool.h>",
    "#include <stdint.h>",
    "#include <stddef.h>",
    "",
    "struct __cmt_buf {",
    "\tsize_t length;",
    "\tuint8_t data[];",
    "};",
    "",
    "typedef struct __cmt_buf *cmt_buf_t;"
 ]
