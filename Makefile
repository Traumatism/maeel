build:
	cargo build -q --release && mv target/release/maeel ./maeel

install:
	cargo install --path maeel_main

vscode:
	rm -rf ~/.vscode/extensions/maeel-syntax-highlighting 2>/dev/null && \
	cp -r maeel_syntax_highlighting ~/.vscode/extensions/maeel-syntax-highlighting

test:
	cargo build -q && mv target/debug/maeel ./maeel.tmp && \
	./maeel.tmp tests.maeel || \
	rm maeel.tmp
