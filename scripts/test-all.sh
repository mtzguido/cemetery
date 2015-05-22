#!/bin/bash

set -ue

./scripts/do-test.sh --fail	tests/bad-lex	"--lex"
./scripts/do-test.sh --fail	tests/bad-ast	"--ast"
./scripts/do-test.sh --fail	tests/bad-type	"--trans"
./scripts/do-test.sh		tests/good	""
