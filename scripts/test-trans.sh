#!/bin/bash

set -ue

./scripts/test-custom.sh tests/good tests/trans-bad "--trans"
