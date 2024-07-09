/bin/echo -n "[*] Building maeelc(ompiler)... "
rustc maeelc.rs -Copt-level=3 -o maeelc 2>/dev/null
echo "done"

/bin/echo -n "[*] Compiling '$1' to assembly... "
./maeelc $1 > $1.asm
echo "done"

/bin/echo -n "[*] Compiling to machine code and linking... "
nasm -f elf64 $1.asm
gcc -nostartfiles -no-pie -o $1.out $1.o -lc
echo "done"

echo ""
echo "[+] You may now run: ./$1.out"
