all: build buildc buildlex

build:
	rustc maeel.rs -Copt-level=3 -o maeel

buildc:
	rustc maeelc.rs -o maeelc

buildlex:
	rustc maeellex.rs -o maeellex

nvim:
	rm ~/.config/nvim/syntax/maeel.vim && cp editor_impls/vim/maeel.vim ~/.config/nvim/syntax

vscode:
	rm -rf ~/.vscode/extensions/maeel-syntax-highlighting 2>/dev/null && cp -r editor_impls/vscode ~/.vscode/extensions/maeel-syntax-highlighting

test:
	rustc maeel.rs -o maeel && ./maeel tests.maeel

bench:
	rustc maeel.rs -o maeel && hyperfine --runs 1000 "./maeel tests.maeel"

clean:
	rm maeellex maeel maeelc
