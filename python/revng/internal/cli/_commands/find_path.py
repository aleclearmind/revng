#
# This file is distributed under the MIT License. See LICENSE.md for details.
#

import sys
from pathlib import Path
from typing import Optional

from revng.internal.cli.commands_registry import Command, CommandsRegistry, Options


class FindPathCommand(Command):
    def __init__(self):
        super().__init__(
            ("internal", "find-path"),
            "Resolve a file under one of revng's search prefixes",
        )

    def register_arguments(self, parser):
        parser.description = (
            "Walk revng's search prefixes (REVNG_RESOURCES env var, "
            "--prefix flags, and the running install root) and print the "
            "absolute path of the first prefix where the given relative "
            "file exists. Exits non-zero if no prefix contains it."
        )
        parser.add_argument(
            "relpath",
            help="Relative path to resolve, e.g. share/revng/prototypes.sqlite",
        )

    def run(self, options: Options) -> Optional[int]:
        relpath = options.parsed_args.relpath
        if Path(relpath).is_absolute():
            sys.stderr.write(f"find-path: argument must be relative: {relpath}\n")
            return 1
        for prefix in options.search_prefixes:
            candidate = Path(prefix) / relpath
            if candidate.exists():
                sys.stdout.write(str(candidate) + "\n")
                return 0
        sys.stderr.write(
            f"find-path: {relpath!r} not found under any of:\n  "
            + "\n  ".join(options.search_prefixes)
            + "\n"
        )
        return 1


def setup(commands_registry: CommandsRegistry):
    commands_registry.register_command(FindPathCommand())
