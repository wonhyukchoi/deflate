# deflate
A simplified implementation of the [DEFLATE algorithm](https://en.wikipedia.org/wiki/Deflate).

## Usage
### Flags
* `-i`, `--input`
* `-o`, `--output`, or if none specified, `stdout`
* `-e`, `--compress`
* `-d`, `--decompress`
* `-c`, `--char`, do compression at the character granularity
* `-w`, `--word`, do compression at the word (stream of characters delimited by whitespace (space, tab, newline)) granularity
