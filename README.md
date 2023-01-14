# deflate
A pure Haskell file compressor / decompressor using the [DEFLATE algorithm](https://en.wikipedia.org/wiki/Deflate).

## Usage
### Flags
* `-i`, `--input`
* `-o`, `--output`, or if none specified, `stdout`
* `-e`, `--compress`
* `-d`, `--decompress`
* `-c`, `--char`, do compression at the character granularity. This is the default.
* `-w`, `--word`, do compression at the word (stream of characters delimited by whitespace (space, tab, newline)) granularity
* `-p`, `--profile`, calculates time and size of compression / decompression
* `-g=[int]`, `--greed`
    * Greediness factor 0-10 (default 5).
    * Higher greed results in longer compression times, but smaller files. 
    * Greediness of 0 results in an uncompressed file. Greed of 10 for big files may not terminate.

## Benchmarks
Probably populated here after the tool is built.
