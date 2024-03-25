install.packages("tidyverse")
library("tidyverse")
library("nflfastR")

playByPlay <- load_pbp(2022)

playByPlayPass =
  playByPlay |>
    filter(play_type == 'pass' & !is.na(air_yards));

playByPlayPass |>
  group_by(passer_id, passer) |>
  summarize(n =n(), adot=mean(air_yards)) |>
  filter(n >= 100 & !is.na(passer)) |>
  arrange(-adot) |>
  print(n= Inf)



