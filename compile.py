import argparse
import os


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="maeel compiler")

    parser.add_argument(
        "-m",
        "--mael-src",
        default="src/main.rs",
        help="maeel.rs src file",
        metavar="<path>",
        dest="maeel",
    )

    parser.add_argument(
        "file",
        help="target file to compile",
        metavar="<path>",
    )

    args = parser.parse_args()

    content = ""
    with open(args.maeel, "r") as f:
        for line in f.readlines():
            if line.startswith("fn main()"):
                break

            content += line

    target_content = open(args.file, "r").read()

    content += (
        """
fn main() {
    let content = r#"%s"#;

    let mut tokens = lex_into_tokens(&content);
    let mut instructions = Stack::default();
    let mut current_instruction = Stack::default();

    while let Some(next) = tokens.pop() {
        match next {
            Token::Separator => {
                instructions.push(current_instruction);
                current_instruction = Stack::default()
            }
            _ => current_instruction.push(next),
        }
    }

    instructions.push(current_instruction);

    Parser::new(instructions, VM::default()).parse();
}
    """
        % target_content
    )

    output_file = args.file.replace(".", "_")
    with open(output_file + ".rs", "w") as f:
        f.write(content)

    os.system(f"rustc -C opt-level=3 {output_file}.rs")
    os.remove(f"{output_file}.rs")
