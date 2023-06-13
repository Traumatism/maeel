build:
	cargo build -q --release && mv target/release/maeel ./maeel

install:
	cargo install --path .

vscode:
	rm -rf ~/.vscode/extensions/maeel-syntax-highlighting 2>/dev/null && cp -r ide ~/.vscode/extensions/maeel-syntax-highlighting

test:
	cargo build -q --release && mv target/release/maeel ./maeel.tmp && ./maeel.tmp stdlib/tests.maeel || rm maeel.tmp

clean: 
	rm ./maeel ./maeel.tmp Cargo.lock || rm -r target

bench:
	RUSTFLAGS='--cfg maeelvm="ikuyo"' cargo build -q --release && mv target/release/maeel ./maeel.tmp && hyperfine "./maeel.tmp stdlib/tests.maeel" || rm maeel.tmp
