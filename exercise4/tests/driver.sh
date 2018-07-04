#!/bin/sh

set -eu

CMD=../lab4

SCRIPT=$(readlink -f "$0")
SCRIPTPATH=$(dirname "$SCRIPT")
cd "$SCRIPTPATH"

echo "Starting tests..."

for filename in ./test_**.cc; do
	echo -n "Running test $filename..."
	
	clang -Wno-macro-redefined "$filename" -o "$filename.clang.bin"
	if "$filename.clang.bin"; then
		ref_val=0
	else
		ref_val=$?
	fi
	echo -n "  $ref_val"

	"$CMD" "$filename" > "$filename.custom.ll"
	llc "$filename.custom.ll" -o "$filename.custom.s"
	clang "$filename.custom.s" -o "$filename.custom.bin"
	if "$filename.custom.bin"; then
		act_val=0
	else
		act_val=$?
	fi

	if [ $act_val -ne $ref_val ]; then
		echo "  failure: custom $act_val vs clang $ref_val"
		exit 1;
	fi

	echo "  done"
	rm "$filename.clang.bin" "$filename.custom.ll" "$filename.custom.bin" "$filename.custom.s"

done

echo "Finished tests"
