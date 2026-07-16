# Split violin plot with significance annotations

使用模拟数据和 `ggplot2` 复现的左右分裂小提琴图，包含七类细胞、两种 epigenomic compartment、四分位区间、中位数和显著性括号。

## 文件

- `split_violin_plot.Rmd`：数据生成、自定义分裂小提琴几何对象和绘图代码
- `split_violin_plot.html`：编译后的自包含 HTML
- `split_violin_plot.png`：320 dpi 图片
- `synthetic_at_content.csv`：模拟 A/T content 数据
- `significance_results.csv`：显著性检验结果

## 编译

```r
rmarkdown::render("split_violin_plot.Rmd")
```

需要的 R 包：`rmarkdown`、`knitr`、`ggplot2`、`dplyr` 和 `scales`。
