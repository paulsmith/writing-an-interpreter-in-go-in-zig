#!/bin/bash

set -e

cd $(dirname ${BASH_SOURCE[0]})
timestamp=$(date +%Y-%m-%dT%H:%M)
name=interp-${timestamp//:/_}.md
cat > $name <<EOF
---
title: "\"Writing an Interpreter in Go\" in Zig, part XX"
date: $timestamp
---

# "Writing an Interpreter in Go" in Zig, part XX


EOF
