# Two-dimensional Gibbs energy landscape

R Markdown reproduction of panel G: a two-dimensional Gibbs/free-energy heatmap with two connected metastable basins.

All data are synthetic and reproducible with `set.seed(1031)`.

## Files

- `gibbs_energy_landscape_2d.Rmd` — simulation and plotting code.
- `gibbs_energy_landscape_2d.html` — self-contained compiled report.
- `gibbs_energy_landscape_2d.png` — high-resolution figure.
- `gibbs_energy_grid_2d.csv` — calculated energy grid.
- `synthetic_state_parameters.csv` — parameters used to create the basins.

## Rebuild

```r
rmarkdown::render("gibbs_energy_landscape_2d.Rmd")
```

Required packages: `ggplot2`, `dplyr`, `tidyr`, `rmarkdown`, and `knitr`.
