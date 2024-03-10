build:
	rustc maeel.rs -Copt-level=3 -o maeel

vscode:
	rm -rf ~/.vscode/extensions/maeel-syntax-highlighting 2>/dev/null && cp -r ide ~/.vscode/extensions/maeel-syntax-highlighting

test:
	rustc maeel.rs -o maeel && ./maeel stdlib/tests.maeel

clean: 
	rm ./maeel ./maeel.tmp Cargo.lock || rm -r target

bench:
	rustc maeel.rs -o maeel && hyperfine --runs 1000 "./maeel stdlib/tests.maeel"
