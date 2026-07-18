# Antibody uptake tissue dot plot

R Markdown reproduction of a biodistribution dot plot comparing control IgG click, pre-click, and site-specific click conditions across tumour and other tissues.

All data are synthetic and reproducible with `set.seed(1031)`.

## Files

- `antibody_uptake_tissue_dotplot.Rmd` — data construction and plotting code.
- `antibody_uptake_tissue_dotplot.html` — self-contained compiled report.
- `antibody_uptake_tissue_dotplot.png` — high-resolution figure.
- `synthetic_antibody_uptake.csv` — replicate-level observations.
- `antibody_uptake_summary.csv` — means and standard deviations.

## Rebuild

```r
rmarkdown::render("antibody_uptake_tissue_dotplot.Rmd")
```

Required packages: `ggplot2`, `dplyr`, `tidyr`, `rmarkdown`, and `knitr`.
