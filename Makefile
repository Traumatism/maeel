CC=rustc
CCARGS=-Copt-level=3 -Cstrip=symbols -v
SRC=src/maeel.rs
TESTS=tests.maeel
EXE=maeel


all: build

fmt:
	rustfmt $(SRC)

fast:
	$(CC) $(SRC) -o $(EXE)

build:
	$(CC) $(CCARGS) $(SRC) -o $(EXE)

nvim:
	rm -f ~/.config/nvim/syntax/maeel.vim
	cp editor_impls/vim/maeel.vim ~/.config/nvim/syntax

vscode:
	rm -rf ~/.vscode/extensions/maeel-syntax-highlighting 2>/dev/null
	cp -r editor_impls/vscode ~/.vscode/extensions/maeel-syntax-highlighting

test:	build
	./$(EXE) $(TESTS) 

bench:	build
	hyperfine --runs 1000 "./$(EXE) $(TESTS)"

