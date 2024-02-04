#!/bin/bash

# Convert Markdown to LaTeX
md2tex() {
  local input_file="$1"
  local output_file="$2"

  awk -F'|' 'NR>3 && NF { 
      namer=gensub(/^ *| *$/, "", "g", $2);
      gsub(/_/,"\\_", namer); # Replace underscores with "\_"
      name=gensub(/^ *| *$/, "", "g", $2);
      family=gensub(/^ *| *$/, "", "g", $3); 
      gradient=gensub(/^ *| *$/, "", "g", $4); 
      palette=gensub(/^ *| *$/, "", "g", $5); 
      levels=gensub(/^ *| *$/, "", "g", $6); 
      colorbar=gensub(/^ *| *$/, "", "g", $7); 
      print namer " & " gradient " & " palette " & " levels " &"; 
      print "\\includegraphics[width=\\linewidth]{../png/" tolower(name) "_colorbar.png}\\\\ \\hline" 
  }' "$input_file" > "$output_file"
}
md2tex "../COLORMAPS_LIST_CAT.md" "contents/categorical.tex"
md2tex "../COLORMAPS_LIST_CYC.md" "contents/cyclic.tex"
md2tex "../COLORMAPS_LIST_DIV.md" "contents/diverging.tex"
md2tex "../COLORMAPS_LIST_MSQ.md" "contents/multi_sequential.tex"
md2tex "../COLORMAPS_LIST_SEQ.md" "contents/sequential.tex"


# Build the document
pdflatex -shell-escape forcolormap.tex
bibtex forcolormap
pdflatex -shell-escape forcolormap.tex
pdflatex -shell-escape forcolormap.tex

mv forcolormap.pdf ../ForColormap.pdf

# Clean up
rm -f *.log *.aux *.lot *.lof *.toc *.out *.bbl *.blg *.fdb_latexmk *.fls
rm -rf svg-inkscape