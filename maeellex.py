import sys

from typing import List, NamedTuple, TypeVar, Generic

T = TypeVar("T")

class Token(NamedTuple, Generic[T]):
    """ https://en.wikipedia.org/wiki/Lexical_analysis#Tokenization """
    value: T
    file: str
    line: int

Tokens = List[Token]
""" List of token """

class String(Token[str]):     """ String(str) token """
class Name(Token[str]):       """ Name(str) token """
class Integer(Token[int]):    """ Integer(int) token """
class Float(Token[float]):    """ Float(float) token """
class Symbol(Token[str]):     """ Symbol(str) token """
class Comment(Token[str]):    """ User comment(str) token """
class Annotation(Token[str]): """ Type annotation(str) token """
class Block(Token[Tokens]):   """ Code block([]token) token """


def lex_into_tokens(code: str, file: str) -> Tokens:
    line                = 1
    chars               = iter(code)
    stack: List[Tokens] = list()
    tokens: Tokens      = list()
    output: Tokens      = list()
    temp_tokens: Tokens = list()

    while char := next(chars, None):
        if char == "\n":
            line += 1
        elif char in (" ", "\t"):
            pass
        elif char == "#":
            comment = char
            while (next_char := next(chars)) != '\n':
                comment += next_char
            line += 1
            tokens.append(Comment(comment, file, line))
        elif char in ("(", ")"):
            tokens.append(Symbol(char, file, line))
        elif char == "[":
            annotation = str()
            while (next_char := next(chars)) != "]":
                annotation += next_char
            tokens.append(Annotation(annotation, file, line))
        elif char == "\"":
            content = str()
            while (next_char := next(chars)) != "\"":
                if next_char == "\\":
                    match next(chars, None):
                        case "n": content += "\n"
                        case "t": content += "\t"
                        case "\\": content += "\n"
                        case "\"": content += "\""
                        case other: raise Exception(f"\n{file}:{line} invalid escape sequence: \\{other}")
                    continue
                content += next_char
            tokens.append(String(content, file, line))
        elif (name_pred := lambda char: char.isalpha() or char == "_")(char):
            name = char
            while name_pred(next_char := next(chars)):
                name += next_char
            chars = iter(next_char + str().join(chars))
            tokens.append(Name(name, file, line))
        elif char.isdigit():
            content = char
            while (next_char := next(chars)).isdigit():
                content += next_char
            chars = iter(next_char + str().join(chars))
            cls, fun = (Float, float) if "." in content else (Integer, int)
            tokens.append(cls(fun(content), file, line))  # type: ignore
        else:
            tokens.append(Symbol(char, file, line))
        char = next(chars, None)

    for token in tokens:
        if not isinstance(token, Symbol):
            temp_tokens.append(token)
            continue
        if token.value == "(":
            stack.append(temp_tokens)
            temp_tokens = list()
            continue
        elif token.value == ")":
            nested_tokens = temp_tokens.copy()
            if stack:
                temp_tokens = stack.pop()
                temp_tokens.append(Block(nested_tokens, file, line))
                continue
            nested_tokens.reverse()
            output.append(Block(nested_tokens, file, line))
            continue
        temp_tokens.append(token)
    output.extend(temp_tokens)
    return output


if __name__ == "__main__":
    with open(file := sys.argv[1], "r") as f:
        for token in lex_into_tokens(f.read() + "\n", file):
            print(token)
