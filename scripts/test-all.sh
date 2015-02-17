#!/bin/bash

set -ue

./scripts/test-lexer.sh
./scripts/test-parse.sh
./scripts/test-full.sh
