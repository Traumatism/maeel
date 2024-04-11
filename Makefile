build:
	rustc maeel.rs -Copt-level=3 -o maeel

nvim:
	rm ~/.config/nvim/syntax/maeel.vim && cp maeel.vim ~/.config/nvim/syntax

vscode:
	rm -rf ~/.vscode/extensions/maeel-syntax-highlighting 2>/dev/null && cp -r ide ~/.vscode/extensions/maeel-syntax-highlighting

test:
	rustc maeel.rs -o maeel && ./maeel tests.maeel

bench:
	rustc maeel.rs -o maeel && hyperfine --runs 1000 "./maeel tests.maeel"
