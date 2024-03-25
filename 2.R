library("tidyverse")
library("nflfastR")
library("ggthemes")
playByPlay = load_pbp(2016:2023);
passingPlays =
  playByPlay |>
    filter(play_type== "pass" & !is.na(air_yards))
passingPlays =
  passingPlays |>
    mutate(
      pass_length_air_yards = ifelse(air_yards >= 20, "long", "short"),
      passing_yards = ifelse(is.na(passing_yards), 0, passing_yards)
    )
passingPlays |>
  pull(passing_yards) |>
  summary()

passingPlays |>
  filter(pass_length_air_yards == "short") |>
  pull(passing_yards) |>
  summary()


passingPlays |>
  filter(pass_length_air_yards == "long") |>
  pull(passing_yards) |>
  summary()

passingPlays |>
  filter(pass_length_air_yards == "short") |>
  pull(epa) |>
  summary()

passingPlays |>
  filter(pass_length_air_yards == "long") |>
  pull(epa) |>
  summary()

ggplot(playByPlay, aes(x = passing_yards)) +
  geom_histogram()

passingPlays |>
  filter(pass_length_air_yards == "long") |>
  ggplot(aes(passing_yards)) +
  geom_histogram(bin_width = 1) +
  ylab("Count") +
  xlab("Yards gained or lost during passing plays on long passes") +
  theme_bw()

ggplot(passingPlays, aes(x = pass_length_air_yards, y = passing_yards)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Pass Length in yards") +
  ylab("Yards gained or loss")

quarterbacks =
  passingPlays |>
  group_by(passer_player_name, passer_player_id, season) |>
    summarize(
      ypa = mean(passing_yards, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

quarterbacks |>
  arrange(-ypa) |>
  print()

quarterbacks =
  passingPlays |>
    group_by(passer_id, passer, season) |>
    summarize(
      n=n(), ypa=mean(passing_yards),
      .groups = "drop"
    ) |>
    filter(n>=100) |>
    arrange(-ypa)

quarterbacks |>
  print(n = 20)


airYards =
  passingPlays |>
    select(passer_id, passer, season, pass_length_air_yards, passing_yards) |>
    arrange(passer_id, season, pass_length_air_yards) |>
    group_by(passer_id, passer, pass_length_air_yards, season) |>
    summarize(n=n(),
              ypa = mean(passing_yards),
              .groups = "drop") |>
    filter((n >= 100 & pass_length_air_yards == "short") |
             (n>=30 & pass_length_air_yards == "long")) |>
    select(-n)

airYardsLag =
  airYards |>
    mutate(season = season + 1) |>
    rename(ypa_last = ypa)

passers =
  airYards |>
    inner_join(airYardsLag,
               by = c("passer_id", "pass_length_air_yards", "season", "passer"))

passers |>
  filter(passer %in% .c("T.Brady", "A.Rodgers")) |>
  print(n = Inf)


passers |>
  glimpse()

passers |>
  distinct(passer_id) |>
  nrow()

scatterYPA =
  ggplot(passers, aes0x = ypa_last, y = ypa)





