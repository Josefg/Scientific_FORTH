@echo off
echo --- Step 1 of 4: Running pdfLaTeX (Pass 1) to find all references... ---
pdflatex %1

echo --- Step 2 of 4: Running MakeIndex to build the index... ---
makeindex -s index_style.ist "%~n1.idx"

echo --- Step 3 of 4: Running pdfLaTeX (Pass 2) to add the index and update references... ---
pdflatex %1

echo --- Step 4 of 4: Running pdfLaTeX (Pass 3) to finalize all page numbers... ---
pdflatex %1

echo --- Build complete. Now cleaning up auxiliary files... ---

REM --- Cleanup Section ---
del "*.aux" "*.log" "*.idx" "*.ind" "*.ilg" "*.toc" "*.out" "*.ptc"

echo --- Cleanup complete. ---