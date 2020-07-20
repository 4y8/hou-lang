#!/bin/sh

test() {
    ./a.out "$(cat "$1")" > out.asm &&
        nasm -felf64 out.asm -o out.o &&
        musl-gcc out.o -static -o out
    ./out
    [ "$?" != "$2" ] &&
        echo "Test $1 failed!" &&
        exit 1
    rm ./out.o ./out ./out.asm
}

[ -z "$CC" ] && CC="cc"
"$CC" ../hou.c
for file in *; do
    [ -e "$file" ] || [ -L "$file" ] || continue
    [ "$file" != "test.sh" ] && [ "$file" != "a.out" ] &&
        test "$file" "$(echo "$file" | cut -b $((${#file} - 4)))" &&
        echo "Test $file passed!"
done
echo "All test passed!"
rm a.out
