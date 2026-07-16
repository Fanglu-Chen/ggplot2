library(ggplot2)
library(ggrepel)
library(tidyverse)

df <- as.data.frame(tibble(
  disease = as.factor(c("心绞痛", "心脏性猝死", "心肌梗死", "脑卒中")),
  count = c(811, 29, 1643, 10812)
))

df <- df %>% mutate(fraction = round(count / sum(count),4),
                                    ymax = cumsum(fraction),
                                    ymin = c(0, head(ymax, n=-1)),
                                    labelPosition = (ymax + ymin)/2,
                                    label = paste0(disease,"\n",fraction*100, "%"))

ggplot(df, aes(fill = disease)) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_text_repel(aes(x = 4, y = labelPosition, label = label),
                   size = 4, hjust = .5,
                   nudge_x = 0.7, direction = "x"
  ) +
  coord_polar(theta = "y", clip = "off") +
  xlim(c(2, 5)) +
  scale_fill_brewer(palette = "OrRd") +
  theme(legend.position = "none") +
  theme_void() +
  guides(
    fill = "none"
  )

g <- grid::grid.text(label = "江西省", x = 0.5, y = 0.5, 
                     just = c("center", "center"), 
                     gp = grid::gpar(col = "black", fontsize = 15, fontface = "bold"), vp = NULL)


grid.draw(g)
