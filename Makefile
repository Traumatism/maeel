build:
	rustc maeel.rs -Copt-level=3 -o maeel -L dependency=enums.rs -L dependency=types.rs

vscode:
	rm -rf ~/.vscode/extensions/maeel-syntax-highlighting 2>/dev/null && cp -r ide ~/.vscode/extensions/maeel-syntax-highlighting

test:
	rustc maeel.rs -o maeel && ./maeel stdlib/tests.maeel

bench:
	rustc maeel.rs -o maeel && hyperfine --runs 1000 "./maeel stdlib/tests.maeel"
