> Produce an Anki deck from an org file

# Installation and usage

``` shell 
stack build .
stack install .
pandoc-anki  # for help
```

# Prerequisites

-   Anki 2.0.47
-   Anki Addon: Crowd Anki Importer

# Optional prerequisites

If you need correct tikz output (e.g., tikz commutative diagrams), use
the Anki Addon "Edit Latex Build process", with the following build
process:

``` python
newLaTeX = [
  ["pdflatex", "-interaction=nonstopmode", "--shell-escape", "tmp.tex"]
]


# make the changes
import anki.latex
anki.latex.latexCmds = newLaTeX
```

This needs imagemagick with the pdf convert delegate, otherwise you get
a "convert: no decode delegate for this image format \`PDF'" error:

    brew remove imagemagick 
    brew install imagemagick --build-from-source
