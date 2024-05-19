from typing import Iterable, TypedDict

from toolz.curried import pipe, map, filter

Line = str
LabelName = str
VarName = str
BinaryString = str
BinaryChar = str


class AInstruction(TypedDict):
    num: int


class CInstruction(TypedDict):
    dest: BinaryString
    comp: BinaryString
    jump: BinaryString


Instruction = AInstruction | CInstruction


def asm_to_hack(asm_source: str) -> str:
    # remove whitespace and comments
    lines = pipe(
        asm_source.split("\n"),
        map(lambda s: s.replace(" ", "")),  # remove whitespace
        filter(lambda s: not s.startswith("//")),  # remove comments
        filter(lambda s: s),
        list,
    )

    label_map = build_label_map(lines)

    for line in lines:
        print(line)

    return ""


def build_label_map(lines: Iterable[Line]) -> dict[LabelName, int]:
    # pc means "program counter"
    pc: int = 0
    ret: dict[LabelName, int] = {}
    for line in lines:
        if is_label(line):
            label_name = parse_label_name(line)
            ret[label_name] = pc
        else:
            pc += 1
    return ret


def build_var_map(
    label_map: dict[LabelName, int],
    lines: Iterable[Line],
) -> dict[VarName, int]:
    ret: dict[VarName, int] = {}
    var_counter: int = 16

    lines = pipe(lines, filter(is_a_instruction))
    for line in lines:
        text = parse_a_instruction_text(line)
        ret[text] = var_counter
        var_counter += 1
    return ret


def is_label(line: Line) -> bool:
    return line.startswith("(") and line.endswith(")") and len(line) > 2


def parse_label_name(line: Line) -> LabelName:
    return line[1:-1]


def is_a_instruction(line: Line) -> bool:
    return line.startswith("@")


def parse_a_instruction_text(line: Line) -> str:
    return line[1:]


def parse_a_instruction(
    label_map: dict[LabelName, int],
    var_map: dict[VarName, int],
    line: Line,
) -> AInstruction:
    text = parse_a_instruction_text(line)
    if text.isdigit():
        return {"num": int(text)}
    elif text in label_map:
        return {"num": label_map[text]}
    elif text in var_map:
        return {"num": var_map[text]}
    else:
        raise Exception("foo")


def parse_instruction(
    label_map: dict[LabelName, int],
    var_map: dict[VarName, int],
    line: Line,
) -> Instruction:
    text = parse_a_instruction_text(line)
    if text.isdigit():
        return {"num": int(text)}
    elif text in label_map:
        return {"num": label_map[text]}
    elif text in var_map:
        return {"num": var_map[text]}
    else:
        raise Exception("foo")


# def parse_instructions(
#     label_map: dict[LabelName, int],
#     lines: Iterable[Line],
# ) -> list[Instruction]:
#     ret: list[Instruction] = []
#     var_map: dict[VarName, int] = {}
#     var_counter: int = 16
#
#     for line in lines:
#         _, is_label = parse_label_name(line)
#         if is_label:
#             continue
#         # Parse A-instruction
#         elif line.startswith("@"):
#             text = line[1:]
#             a_num: int
#             if text.isdigit():
#                 a_num = int(text)
#             elif text in label_map:
#                 a_num = label_map[text]
#             elif text in var_map:
#                 a_num = var_map[text]
#             else:
#                 var_map[text] = var_counter
#                 var_counter += 1
#                 a_num = var_counter
#     return []
