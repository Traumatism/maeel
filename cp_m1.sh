make buildc

./maeelc $1 > $1.asm
nasm -f elf64 $1.asm


gcc -g -Wall -Wextra -pedantic -Werror -target x86_64-apple-darwin -nostartfiles -no-pie -o $1.out $1.o -lc
