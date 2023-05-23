CODE = "1 1 + 3 * 4 f"

Token = tuple[str, int | None]
tokens: list[Token] = []

state = 0
state1_buffer = ""

for char in CODE:

    if char == " " and state == 0:  # ignore whitespaces
        continue

    elif char == " " and state == 1:
        tokens.append(("INT", int(state1_buffer)))
        state1_buffer = ""
        state = 0

    elif char == "f" and state == 0:
        tokens.append(("FUNCTION", ("f", 2)))

    elif char == "+" and state == 0:
        tokens.append(("BINOP_ADD", None))

    elif char == "*" and state == 0:
        tokens.append(("BINOP_MUL", None))

    elif char.isdigit() and state == 0:
        state = 1
        state1_buffer += char

    elif state == 1:
        state1_buffer += char

stack = []

for token, value in tokens:
    if token == "INT":
        stack.append(value)

    elif token == "BINOP_ADD":
        lhs, rhs = stack.pop(), stack.pop()
        stack.append(f"({lhs} + {rhs})")

    elif token == "BINOP_MUL":
        lhs, rhs = stack.pop(), stack.pop()
        stack.append(f"({lhs} * {rhs})")

    elif token == "FUNCTION":
        function_name, arguments_count = value

        exprs = (
            str(stack.pop()) for _ in range(arguments_count)
        )

        stack.append(f"({function_name}({', '.join(exprs)}))")

assert len(stack) == 1

print(f"Input: {CODE}")
print(f"Output: {stack[0]}")
