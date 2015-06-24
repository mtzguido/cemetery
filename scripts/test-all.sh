#!/bin/bash

set -ue

./scripts/do-test.sh --fail	tests/bad-lex	"--lex"
./scripts/do-test.sh		tests/bad-ast	"--lex"
./scripts/do-test.sh --fail	tests/bad-ast	"--ast"
./scripts/do-test.sh		tests/bad-type	"--ast"
./scripts/do-test.sh --fail	tests/bad-type	"--trans"
./scripts/do-test.sh		tests/good	""
./scripts/do-test.sh --build	tests/good	""
./scripts/do-live-tests.sh	tests/live
