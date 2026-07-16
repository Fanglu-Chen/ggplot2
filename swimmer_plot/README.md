# Swimmer plot

使用模拟数据和 `ggplot2` 复现的临床治疗泳道图，包含患者治疗持续时间、最佳疗效、疾病进展、换药事件和五列患者特征。

> 数据仅用于演示绘图方法，不代表真实临床研究结果。

## 文件

- `swimmer_plot.Rmd`：数据生成和绘图代码
- `swimmer_plot.html`：编译后的自包含 HTML
- `swimmer_plot.png`：320 dpi 图片
- `synthetic_patient_summary.csv`：患者特征和随访数据
- `synthetic_treatment_intervals.csv`：治疗区间数据
- `synthetic_clinical_markers.csv`：疗效和事件标记

## 编译

```r
rmarkdown::render("swimmer_plot.Rmd")
```

需要的 R 包：`rmarkdown`、`knitr`、`ggplot2` 和 `dplyr`。
