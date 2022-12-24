build:
	cargo build -v --release && mv target/release/maeel .

vscode:
	rm -rf ~/.vscode/extensions/maeel && cp -r maeel-language ~/.vscode/extensions/