# Global wetland cover loss map

R Markdown reproduction of a global wetland-cover-loss map. The output uses a rectangular longitude–latitude extent and intentionally omits the panel letter.

All wetland-loss values are synthetic and reproducible with `set.seed(1031)`.

## Files

- `wetland_cover_loss_map.Rmd` — spatial simulation and plotting code.
- `wetland_cover_loss_map.html` — self-contained compiled report.
- `wetland_cover_loss_map.png` — high-resolution figure.
- `synthetic_wetland_loss_grid.csv` — synthetic grid-cell values.
- `wetland_region_parameters.csv` — parameters controlling spatial wetland occurrence.
- `wetland_loss_pressure_parameters.csv` — parameters controlling regional loss intensity.

## Rebuild

```r
rmarkdown::render("wetland_cover_loss_map.Rmd")
```

Required packages: `ggplot2`, `dplyr`, `tidyr`, `sf`, `rnaturalearth`, `rmarkdown`, and `knitr`.
