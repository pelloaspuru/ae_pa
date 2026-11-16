# Builds w1_graphs.pdf, the figures of the "Graphs" section of the slides.
# Runs w1_code.R and stores every plot it draws as one page, in order of
# appearance: adding or removing a plot in w1_code.R shifts the page numbers
# used by \includegraphics[page=...] in AE_w01_intro.tex.
# Run from this folder: Rscript w1_figures.R

pdf("w1_graphs.pdf", width = 4, height = 2.5)
source("w1_code.R", print.eval = TRUE)
dev.off()
