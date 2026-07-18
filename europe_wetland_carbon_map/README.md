# European wetland carbon storage map

This folder contains an R Markdown reconstruction of the supplied European
wetland-carbon figure. It keeps the country-level choropleth, country codes and
wetland-class pie charts, while omitting the upper-left panel letter and the
large summary pie chart from panel b.

## Files

- `europe_wetland_carbon_map.Rmd`: reproducible source document.
- `europe_wetland_carbon_map.html`: self-contained compiled report.
- `europe_wetland_carbon_map.png`: high-resolution figure.
- `synthetic_country_wetland_data.csv`: synthetic country estimates and class shares.

The synthetic data are reproducible with `set.seed(1031)`.

