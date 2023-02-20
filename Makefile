build:
	cargo build -v --release && mv target/release/maeel .

install:
	cargo install --path . -v

vscode:
	rm -rf ~/.vscode/extensions/maeel-syntax-highlighting 2>/dev/null && \
	cp -r maeel-syntax-highlighting ~/.vscode/extensions/maeel-syntax-highlighting