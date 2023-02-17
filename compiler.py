import os
import sys


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("python3 compiler.py <file.maeel>")
        sys.exit()

    program_content = open(
        sys.argv[1],
        "r",
        encoding="utf-8",
    ).read()

    program_content = program_content.replace("\n", " ")

    lexer_content = open(
        "src/lexer.rs",
        "r",
        encoding="utf-8",
    ).read()

    interpreter_content = open(
        "src/interpreter.rs",
        "r",
        encoding="utf-8",
    ).read()

    output = ""
    output += lexer_content
    output += "\n"
    output += interpreter_content
    output += "\n"

    output += (
        """
fn main() {
    let content = \"%s\";
    let mut interpreter = Interpreter::default();

    extract_instructions(extract_blocks(&lex_into_tokens(&content)))
        .iter()
        .for_each(|instruction| {
            interpreter.handle_instruction(&mut instruction.iter())
        });
}    
    """
        % program_content
    )

    output_file_name = "%s_out.rs" % sys.argv[1].split(os.path.sep)[-1].split(".")[0]

    output_file = open(
        output_file_name,
        "w+",
        encoding="utf-8",
    )

    for line in output.splitlines():
        if line.startswith("use") and not line.startswith("use std::"):
            continue

        output_file.write(line)
        output_file.write("\n")

    output_file.close()

    os.system(f"rustc {output_file_name} 2>/dev/null")
    os.remove(f"{output_file_name}")
