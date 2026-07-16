# Circular multi-trait lollipop plot

使用模拟数据和 `circlize` 复现的环形多疾病棒棒糖图，包括十二个疾病分区、径向 selection score、外圈基因标签和中心跨疾病弦线。

> 数据仅用于演示绘图方法，不代表真实医学结论。

## 文件

- `circular_lollipop.Rmd`：数据生成、布局计算和绘图代码
- `circular_lollipop.html`：编译后的自包含 HTML
- `circular_lollipop.png`：320 dpi 图片
- `synthetic_gene_scores.csv`：模拟基因评分
- `synthetic_connections.csv`：模拟跨疾病连接

## 编译

```r
rmarkdown::render("circular_lollipop.Rmd")
```

需要的 R 包：`rmarkdown`、`knitr`、`circlize` 和 `dplyr`。
