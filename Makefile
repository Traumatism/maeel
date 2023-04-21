build:
	cargo build -v --release && mv target/release/maeel ./maeel

install:
	cargo install --path maeel_main -v

vscode:
	rm -rf ~/.vscode/extensions/maeel-syntax-highlighting 2>/dev/null && \
	cp -r maeel_syntax_highlighting ~/.vscode/extensions/maeel-syntax-highlighting