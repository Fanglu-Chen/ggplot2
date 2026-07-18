# Wetland-loss bubble scatter plot

R Markdown reproduction of a regional wetland-loss validation bubble plot. Bubble area represents regional area and colour represents continent.

The project contains 121 synthetic observations and is reproducible with `set.seed(1031)`.

## Files

- `wetland_loss_bubble_scatter.Rmd` — simulation, regression, and plotting code.
- `wetland_loss_bubble_scatter.html` — self-contained compiled report.
- `wetland_loss_bubble_scatter.png` — high-resolution figure.
- `synthetic_wetland_regions.csv` — synthetic regional observations.
- `regression_statistics.csv` — model coefficients and fit statistics.

## Rebuild

```r
rmarkdown::render("wetland_loss_bubble_scatter.Rmd")
```

Required packages: `ggplot2`, `dplyr`, `tidyr`, `ggrepel`, `rmarkdown`, and `knitr`.
