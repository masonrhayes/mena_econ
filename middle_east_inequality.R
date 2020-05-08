library(tidyverse)
library(readxl)
library(ggthemes)

middle_east_inequality <- read_csv("middle_east_inequality.csv")
colnames(middle_east_inequality) <- c("year", "top1percent", "bottom50percent", "top10percent")
view(middle_east_inequality)

inequality %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = top10percent, color = "Top 10%")) +
  geom_line(aes(y = bottom50percent, color = "Bottom 50%")) +
  theme_economist() +
  scale_color_economist() +
  labs(title = "Pre-Tax Income Share in ME", x = "Year", y = "Income Share")
