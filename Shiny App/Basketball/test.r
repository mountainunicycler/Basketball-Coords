library(tidyverse)
library(plotly)

bears = read_csv("Bears.csv")

names(bears)

ggplotly(ggplot(bears, aes(event_coord_x, event_coord_y)) +
	geom_hex())
