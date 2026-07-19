# Microbiome relative-abundance composition

This folder contains a reproducible R Markdown reconstruction of a sample-level microbiome composition plot. Synthetic data are generated with `set.seed(1031)`, each stacked bar sums to 100%, and enlarged gaps separate the twelve study groups.

## Files

- `microbiome_relative_abundance.Rmd`: data generation and plotting code.
- `microbiome_relative_abundance.html`: rendered R Markdown report.
- `microbiome_relative_abundance.png`: high-resolution figure.
- `synthetic_microbiome_abundance.csv`: generated sample-by-family abundance data.

## Reproduce

Render the R Markdown document from this directory:

```r
rmarkdown::render("microbiome_relative_abundance.Rmd")
```
