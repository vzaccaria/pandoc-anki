

# Prerequisites

- Anki 2.0.47 
- Anki Addon: Crowd Anki Importer 
- Anki Addon: Edit Latex Build process 

Use the following build process:

```python
newLaTeX = \
[
  ["pdflatex", "-interaction=nonstopmode", "--shell-escape", "tmp.tex"]
]


# make the changes
import anki.latex
anki.latex.latexCmds = newLaTeX
```

# Possible fixes to errors:

## convert: no decode delegate for this image format `PDF'

Try with:

```
brew remove imagemagick 
brew install imagemagick --build-from-source
```
