make buildc

./maeelc $1 > $1.asm
nasm -f elf64 $1.asm
gcc -nostartfiles -no-pie -o $1.out $1.o -lc
# ld -o $1.out $1.o
