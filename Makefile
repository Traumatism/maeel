build:
	cargo build -v --release && mv target/release/maeel .

install:
	cargo install --path . -v