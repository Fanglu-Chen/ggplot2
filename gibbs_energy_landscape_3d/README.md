# Three-dimensional Gibbs energy landscape

R Markdown reproduction of panel H: a three-dimensional Gibbs/free-energy surface with a coloured projection on the lower plane.

All data are synthetic and reproducible with `set.seed(1031)`.

## Files

- `gibbs_energy_landscape_3d.Rmd` — simulation and plotting code.
- `gibbs_energy_landscape_3d.html` — self-contained compiled report.
- `gibbs_energy_landscape_3d.png` — high-resolution figure.
- `gibbs_energy_grid_3d.csv` — calculated energy grid.
- `synthetic_state_parameters.csv` — parameters used to create the basins.

## Rebuild

```r
rmarkdown::render("gibbs_energy_landscape_3d.Rmd")
```

Required packages: `dplyr`, `tidyr`, `plot3D`, `rmarkdown`, and `knitr`.
