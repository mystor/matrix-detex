# Matrix Detex

De-LaTeXifies a given matrix, transforming it into a format more appropriate for using on WolframAlpha and similar.

## Building

Install the Haskell Platform and run:

```bash
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

The `matrix-detex` binary should now be in the `dist/build/matrix-detex` directory.

## Usage

Copy a LaTeX style matrix to the clipboard
```latex
\begin{bmatrix}
  1 & 0 & 0\\
  0 & 1 & 0\\
  0 & 0 & 1
\end{bmatrix}
```

Run `matrix-detex`.

The result will be put on your clipboard!
```
{{1, 0, 0},
 {0, 1, 0},
 {0, 0, 1}}
```
