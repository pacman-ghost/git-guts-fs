A small library that examines the internal files of a git repo.

It can handle:
- objects, stored loose or in packs (including ref and offset deltas)
- the staging index
- refs
- reflogs

Compile the `cli` project (a CLI wrapper around the main library), then run it with `--help` to get help.
