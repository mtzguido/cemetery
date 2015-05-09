module Prologue where

hprologue = ""
cprologue = unlines [
    "#include <stdbool.h>",
    "#include <stdint.h>",
    "",
    "typedef uint8_t cmt_buf_t[];"
 ]
