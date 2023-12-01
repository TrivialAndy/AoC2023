# AoC2023

My solutions to the Advent of code 2023

All code is written in Fortran and built using fpm.
A reminder for myself of how to run it is below!

## Repo structure

- `app` has the main entry point for the code. This is a cli that takes the day, part, and the index of the dataset as 3 args. After running it will print out a runtime too.
- `data` Nothing is commited to this but the code expects a folder called `data` with each days problem inputs in. e.g. `data/day01_0.txt`
- `src` contains code specific to each day. If any of the code ends up being reusable it can be split out into modules that end up in this folder too.
- `fpm.toml` the build file.

## Running the code

```
fpm run -- <day> <part> <dataset>
```

For improved timings try it with the `--profile release` args.
