# Multistage categorical alluvial plot

这是一个使用自编模拟数据和 `ggplot2` 复现的三阶段分类冲积图。项目采用 R Markdown 编译，不依赖专用冲积图扩展包。

## 文件

- `multistage_alluvial.Rmd`：完整数据生成、布局计算和绘图代码
- `multistage_alluvial.html`：编译后的自包含 HTML 文档
- `multistage_alluvial.png`：300 dpi 输出图片
- `synthetic_flow_data.csv`：可重复生成的模拟汇总数据

## 编译

在项目目录运行：

```r
rmarkdown::render("multistage_alluvial.Rmd")
```

需要的 R 包：`rmarkdown`、`knitr`、`ggplot2`、`dplyr` 和 `scales`。
