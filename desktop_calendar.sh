#!/usr/bin/bash

echo "output directory = "$1
./desktop_calendar > $1/pages.tex
cp output/b1.tex output/desktop_calendar.tex

echo "\\input{$1/pages.tex}" >> output/desktop_calendar.tex
echo "\\end{document}" >> output/desktop_calendar.tex

pdflatex -output-directory=$1 output/desktop_calendar.tex
