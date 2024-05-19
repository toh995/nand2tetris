import sys
import pathlib

from foo import asm_to_hack


class ValidationException(Exception):
    pass


# The first entry is the script/filename
file_path = pathlib.Path(sys.argv[1])

# Validate the inputs
if len(sys.argv) != 2:
    raise ValidationException(
        f"Expected exactly one argument, instead got: {sys.argv[1:]}"
    )
if file_path.suffix != ".asm":
    raise ValidationException(
        f"Expected a filepath with extension '.asm', instead got: {file_path}"
    )

# Read the file
asm_source: str
with open(file_path) as f:
    asm_source = f.read()

asm_to_hack(asm_source)

# Write to the new file
new_file_path = file_path.with_suffix(".hack")
with open(new_file_path, "w") as f:
    f.write("asdf")
