#!/bin/bash

if [ -z "$1" ]; then
    echo "Usage: $0 [number]"
    echo "Example: $0 1.17"
    exit 1
fi

NAME="$1"
FILENAME="${NAME}.rkt"

cat > "$FILENAME" <<EOF
#lang sicp
(#%require "util.rkt")

;; exercise $NAME
(display "exercise $NAME\n")

EOF

echo "Exercise $FILENAME created"

