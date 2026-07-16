# Annotated correlation bubble heatmap

This folder contains an R Markdown reproduction of a Cell-style annotated upper/lower correlation heatmap. All values are synthetic and reproducible with `set.seed(1031)`.

## Files

- `correlation_bubble_heatmap.Rmd` — simulation, correlation analysis, and plotting code.
- `correlation_bubble_heatmap.html` — self-contained compiled report.
- `correlation_bubble_heatmap.png` — high-resolution output figure.
- `synthetic_signature_scores.csv` — synthetic signature scores, PSA, and Ki67.
- `signature_metadata.csv` — biological-category, origin, and cell-cycle annotations.
- `spearman_correlation_matrix.csv` — calculated Spearman correlation matrix.

## Rebuild

```r
rmarkdown::render("correlation_bubble_heatmap.Rmd")
```

Required packages: `ggplot2`, `dplyr`, `tidyr`, `patchwork`, `rmarkdown`, and `knitr`.
