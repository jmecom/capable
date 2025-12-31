#!/bin/bash
set -e

cd "$(dirname "$0")/../.."

PROG="examples/sort/sort.cap"
TESTS="examples/sort/tests"
PASS=0
FAIL=0

echo "Building compiler..."
cargo build --bin capc -q

echo "Running sort tests..."
echo

for input in "$TESTS"/*.in; do
    name=$(basename "$input" .in)
    expected="$TESTS/$name.expected"

    if [[ ! -f "$expected" ]]; then
        echo "SKIP $name (no .expected file)"
        continue
    fi

    actual=$(cargo run --bin capc -q -- run "$PROG" < "$input" 2>&1 | grep -v "^[[:space:]]*Finished\|^[[:space:]]*Compiling\|^[[:space:]]*Running")
    expected_content=$(cat "$expected")

    if [[ "$actual" == "$expected_content" ]]; then
        echo "PASS $name"
        ((PASS++))
    else
        echo "FAIL $name"
        echo "  expected:"
        echo "$expected_content" | sed 's/^/    /'
        echo "  actual:"
        echo "$actual" | sed 's/^/    /'
        ((FAIL++))
    fi
done

echo
echo "Results: $PASS passed, $FAIL failed"

if [[ $FAIL -gt 0 ]]; then
    exit 1
fi
