#!/bin/bash

set -ue

./scripts/test-custom.sh tests/good tests/lexer-bad "--lexer"
