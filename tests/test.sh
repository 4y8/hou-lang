#!/bin/sh

test() {
    cat ../stdlib.hou "$1" > temp.hou
    ./a.out temp.hou &&
	cat out.asm ../stdlib.asm > o.asm &&
        nasm -felf64 o.asm -o out.o &&
        musl-gcc out.o -static -o out
    ./out
    [ "$?" != "$2" ] &&
        echo "Test $1 failed!" &&
        exit 1
    rm ./out.o ./out ./out.asm ./o.asm ./temp.hou
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
