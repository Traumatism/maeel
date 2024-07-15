all: build

build:
	rustc src/maeel.rs -Copt-level=3 -o maeel
nvim:
	rm ~/.config/nvim/syntax/maeel.vim && cp editor_impls/vim/maeel.vim ~/.config/nvim/syntax

vscode:
	rm -rf ~/.vscode/extensions/maeel-syntax-highlighting 2>/dev/null && cp -r editor_impls/vscode ~/.vscode/extensions/maeel-syntax-highlighting

test:
	rustc src/maeel.rs -o maeel && ./maeel run tests.maeel

bench:
	rustc src/maeel.rs -o maeel && hyperfine --runs 1000 "./maeel run tests.maeel"
