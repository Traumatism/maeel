echo "[*] Building maeelc(ompiler)... \c"
rustc maeelc.rs -Copt-level=3 -o maeelc 2>/dev/null
echo "done"

echo "[*] Compiling '$1' to assembly... \c"
./maeelc $1 > $1.asm
echo "done"

echo "[*] Compiling to machine code and linking... \c"
nasm -f elf64 $1.asm
gcc -nostartfiles -no-pie -o $1.out $1.o -lc
echo "done"

echo ""
echo "[+] You may now run: ./$1.out"
