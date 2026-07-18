# AUC and mean-difference estimation plot

R Markdown reproduction of a paired estimation plot with AUC quasirandom point distributions on the left and bootstrap mean-difference distributions on the right.

All data are synthetic and reproducible with `set.seed(1031)`.

## Files

- `auc_mean_difference_estimation_plot.Rmd` — simulation, bootstrap, and plotting code.
- `auc_mean_difference_estimation_plot.html` — self-contained compiled report.
- `auc_mean_difference_estimation_plot.png` — high-resolution figure.
- `synthetic_auc_observations.csv` — paired AUC observations.
- `bootstrap_mean_differences.csv` — bootstrap mean-difference values.
- `mean_difference_summary.csv` — estimates and confidence intervals.

## Rebuild

```r
rmarkdown::render("auc_mean_difference_estimation_plot.Rmd")
```

Required packages: `ggplot2`, `dplyr`, `tidyr`, `ggbeeswarm`, `patchwork`, `rmarkdown`, and `knitr`.
