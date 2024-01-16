
``` r
opening_day = as_date("2023-11-06")
today = Sys.Date()
season_dates = as.character(seq.Date(from = opening_day, to = today, by = 1))
no_game_dates = c(as_date("2023-12-25"), as_date("2023-12-26"))
season_dates = season_dates[!season_dates %in% no_game_dates]

# res = read_csv("data/end_games.csv", show_col_types = F)

res = data.frame()

for (date in season_dates) {
  new = get_master_schedule(date)
  res = rbind(res, new)
}

# write_csv(res, "data/end_games.csv")

end = res |>
  filter(!is.na(home_score)) |>
  mutate(win_team = ifelse(home_score > away_score, home, away),
         lose_team = ifelse(home_score > away_score, away, home),
         win_score = ifelse(home_score > away_score, home_score, away_score),
         lose_score = ifelse(home_score > away_score, away_score, home_score))

all_teams = sort(unique(c(unique(end$win_team), unique(end$lose_team))))

team_records = data.frame(team = all_teams) |>
  left_join(end |>
  count(win_team) |>
  setNames(c("team", "wins")), by = "team") |>
  left_join(end |>
  count(lose_team) |>
  setNames(c("team", "losses")), by = "team") |>
  mutate(wins = replace_na(wins, 0),
         losses = replace_na(losses, 0),
         games = wins + losses,
         win_pct = round(wins / games, 3)) |>
  arrange(desc(win_pct), desc(wins))
```

``` r
get_team_off_ppg = function(team) {
  home = end |> filter(home == team) |> pull(home_score)
  away = end |> filter(away == team) |> pull(away_score)
  return(round(mean(c(home, away)), 3))
}

get_team_def_ppg = function(team) {
  home = end |> filter(home == team) |> pull(away_score)
  away = end |> filter(away == team) |> pull(home_score)
  return(round(mean(c(home, away)), 3))
}

team_ppg = data.frame(team = all_teams) |>
  mutate(off_ppg = sapply(team, get_team_off_ppg),
         def_ppg = sapply(team, get_team_def_ppg),
         diff = off_ppg - def_ppg)

eligible_teams = team_records |>
  filter(games >= 3) |>
  pull(team)

top3 = team_ppg |>
  filter(team %in% eligible_teams) |>
  slice_max(diff, n = 5, with_ties = F) |>
  pull(team)

team_ppg |>
  mutate(top_lab = ifelse(team %in% top3, team, "")) |>
  ggplot(aes(off_ppg, def_ppg)) +
  geom_point(aes(col = diff), show.legend = F) +
  ggrepel::geom_text_repel(aes(label = top_lab), size = 3, fontface = "italic", max.overlaps = 100) +
  geom_abline(linetype = "dashed", alpha = 0.25) +
  scale_color_gradient(low = "indianred3", high = "springgreen4") +
  labs(x = "Offensive PPG", y = "Defensive PPG",
       title = "Scatterplot of Offensive and Defensive PPG",
       subtitle = "Labeled teams have a top-five differential")
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
get_half_scores = function(game_id) {
  data = get_pbp_game(game_id) |>
    mutate(prev_home_score = lag(home_score),
           prev_away_score = lag(away_score),
           home_score_diff = home_score - prev_home_score,
           away_score_diff = away_score - prev_away_score)
  
  home = data$home[1]
  away = data$away[1]
    
  return(data |>
    group_by(half) |>
    summarise(home_pts = sum(home_score_diff, na.rm = T),
    away_pts = sum(away_score_diff, na.rm = T)) |>
    transmute(game_id = game_id, half, home_pts, away_pts) |>
    pivot_wider(!game_id, names_from = half, values_from = c("home_pts", "away_pts")) |>
    transmute(game_id, home, away, home_pts_1, home_pts_2, away_pts_1, away_pts_2))
}

uni_unc = 401580922
get_half_scores(uni_unc)
```

    ## # A tibble: 1 × 7
    ##     game_id home          away       home_pts_1 home_pts_2 away_pts_1 away_pts_2
    ##       <dbl> <chr>         <chr>           <dbl>      <dbl>      <dbl>      <dbl>
    ## 1 401580922 Northern Iowa North Car…         35         56         41         28

``` r
cpr_raw = end |>
  left_join(team_ppg, by = c("away" = "team")) |>
  rename(away_off_ppg = off_ppg, away_def_ppg = def_ppg) |>
  select(-diff) |>
  left_join(team_ppg, by = c("home" = "team")) |>
  rename(home_off_ppg = off_ppg, home_def_ppg = def_ppg) |>
  select(-diff) |>
  mutate(home_exp = (home_off_ppg + away_def_ppg) / 2,
         away_exp = (home_def_ppg + away_off_ppg) / 2,
         home_off_cpr = home_score - home_exp,
         home_def_cpr = away_exp - away_score,
         away_off_cpr = away_score - away_exp,
         away_def_cpr = home_exp - home_score)

get_off_cpr = function(team) {
  home = cpr_raw |> filter(home == team) |> pull(home_off_cpr)
  away = cpr_raw |> filter(away == team) |> pull(away_off_cpr)
  return(round(mean(c(home, away)), 3))
}

get_def_cpr = function(team) {
  home = cpr_raw |> filter(home == team) |> pull(home_def_cpr)
  away = cpr_raw |> filter(away == team) |> pull(away_def_cpr)
  return(round(mean(c(home, away)), 3))
}

team_cpr = data.frame(team = all_teams) |>
  mutate(off_cpr = sapply(team, get_off_cpr),
         def_cpr = sapply(team, get_def_cpr),
         total_cpr = off_cpr + def_cpr)
```

``` r
# best overall teams
team_cpr |>
  slice_max(total_cpr, n = 25, with_ties = F) |>
  mutate(rank = rank(-total_cpr),
         team = paste0(team, " (", rank, ")")) |>
  ggplot(aes(reorder(team, total_cpr), total_cpr)) +
  geom_col(aes(fill = team), show.legend = F) +
  geom_text(aes(label = total_cpr), size = 3, hjust = -0.25) +
  coord_flip(ylim = c(0, max(team_cpr$total_cpr) * 1.05)) +
  labs(x = NULL, y = "Total CPR", title = "Best Overall Teams (CPR)")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# best offensive teams
team_cpr |>
  slice_max(off_cpr, n = 25, with_ties = F) |>
  mutate(rank = rank(-off_cpr),
         team = paste0(team, " (", rank, ")")) |>
  ggplot(aes(reorder(team, off_cpr), off_cpr)) +
  geom_col(aes(fill = team), show.legend = F) +
  geom_text(aes(label = off_cpr), size = 3, hjust = -0.25) +
  coord_flip(ylim = c(0, max(team_cpr$off_cpr) * 1.05)) +
  labs(x = NULL, y = "Offensive CPR", title = "Best Offensive Teams (CPR)")
```

![](README_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
# best defensive teams
team_cpr |>
  slice_max(def_cpr, n = 25, with_ties = F) |>
  mutate(rank = rank(-def_cpr),
         team = paste0(team, " (", rank, ")")) |>
  ggplot(aes(reorder(team, def_cpr), def_cpr)) +
  geom_col(aes(fill = team), show.legend = F) +
  geom_text(aes(label = def_cpr), size = 3, hjust = -0.25) +
  coord_flip(ylim = c(0, max(team_cpr$def_cpr) * 1.05)) +
  labs(x = NULL, y = "Defensive CPR", title = "Best Defensive Teams (CPR)")
```

![](README_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
sprintf("Correlation between offensive and total CPR: %s", round(cor(team_cpr$off_cpr, team_cpr$total_cpr), 3))
```

    ## [1] "Correlation between offensive and total CPR: 0.838"

``` r
sprintf("Correlation between defensive and total CPR: %s", round(cor(team_cpr$def_cpr, team_cpr$total_cpr), 3))
```

    ## [1] "Correlation between defensive and total CPR: 0.909"

``` r
off = lm(total_cpr ~ off_cpr, data = team_cpr)
def = lm(total_cpr ~ def_cpr, data = team_cpr)

print("-------------------------------------------------------------------")
```

    ## [1] "-------------------------------------------------------------------"

``` r
sprintf("RMSE for model predicting total CPR with only offensive CPR: %s", round(sqrt(mean(off$residuals ^ 2)), 3))
```

    ## [1] "RMSE for model predicting total CPR with only offensive CPR: 6.166"

``` r
sprintf("RMSE for model predicting total CPR with only defensive CPR: %s", round(sqrt(mean(def$residuals ^ 2)), 3))
```

    ## [1] "RMSE for model predicting total CPR with only defensive CPR: 4.694"

``` r
get_team_color = function(team) {
  try1 = ncaa_colors |> filter(ncaa_name == team)
  if (nrow(try1) > 0) return(pull(try1, primary_color))
  try2 = ncaa_colors |> filter(espn_name == team)
  if (nrow(try2) > 0) return(pull(try2, primary_color))
  if (nrow(try1) == 0 & nrow(try2) == 0) return("not found")
}

team_records |>
  filter(games >= 10) |>
  select(team) |>
  arrange(team) |>
  mutate(color = sapply(team, get_team_color)) |>
  filter(color == "not found")
```

    ##                        team     color
    ## 1       American University not found
    ## 2         Appalachian State not found
    ## 3       Arkansas-Pine Bluff not found
    ## 4         Boston University not found
    ## 5     Cal State Bakersfield not found
    ## 6       Cal State Fullerton not found
    ## 7      Cal State Northridge not found
    ## 8          Central Arkansas not found
    ## 9       Central Connecticut not found
    ## 10         Central Michigan not found
    ## 11      Charleston Southern not found
    ## 12     East Tennessee State not found
    ## 13         Eastern Illinois not found
    ## 14         Eastern Kentucky not found
    ## 15         Eastern Michigan not found
    ## 16       Eastern Washington not found
    ## 17         Florida Atlantic not found
    ## 18       Florida Gulf Coast not found
    ## 19    Florida International not found
    ## 20         Georgia Southern not found
    ## 21       Jacksonville State not found
    ## 22         Long Beach State not found
    ## 23   Long Island University not found
    ## 24         Loyola Marymount not found
    ## 25   Maryland-Eastern Shore not found
    ## 26         Middle Tennessee not found
    ## 27        Mississippi State not found
    ## 28 Mississippi Valley State not found
    ## 29         New Mexico State not found
    ## 30       North Carolina A&T not found
    ## 31   North Carolina Central not found
    ## 32       North Dakota State not found
    ## 33         Northern Arizona not found
    ## 34        Northern Colorado not found
    ## 35        Northern Illinois not found
    ## 36        Northern Kentucky not found
    ## 37       Northwestern State not found
    ## 38             Pennsylvania not found
    ## 39         Prairie View A&M not found
    ## 40         SIU Edwardsville not found
    ## 41         Sacramento State not found
    ## 42           San José State not found
    ## 43     South Carolina State not found
    ## 44   South Carolina Upstate not found
    ## 45       South Dakota State not found
    ## 46            South Florida not found
    ## 47 Southeast Missouri State not found
    ## 48        Southern Illinois not found
    ## 49         St. Francis (PA) not found
    ## 50               St. John's not found
    ## 51     St. Thomas-Minnesota not found
    ## 52        Stephen F. Austin not found
    ## 53           Tarleton State not found
    ## 54          Tennessee State not found
    ## 55 Texas A&M-Corpus Christi not found
    ## 56     UT Rio Grande Valley not found
    ## 57         Washington State not found
    ## 58         Western Carolina not found
    ## 59         Western Illinois not found
    ## 60         Western Kentucky not found
    ## 61         Western Michigan not found
    ## 62         Youngstown State not found

``` r
score_counts = end |>
  mutate(score = paste0(win_score, "-", lose_score)) |>
  count(score) |>
  rename(score_count = n)

end |>
  mutate(score = paste0(win_score, "-", lose_score)) |>
  left_join(score_counts, by = "score") |>
  ggplot(aes(win_score, lose_score)) +
  geom_point(shape = "square", col = "#89A483")
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
my_teams = c("North Carolina", "Tennessee")

team_cpr |>
  mutate(off_rank = rank(-off_cpr, ties.method = "average"),
         def_rank = rank(-def_cpr, ties.method = "average"),
         ovr_rank = rank(-total_cpr, ties.method = "average")) |>
  filter(team %in% my_teams)
```

    ##             team off_cpr def_cpr total_cpr off_rank def_rank ovr_rank
    ## 1 North Carolina   5.717   3.110     8.827       18       86       17
    ## 2      Tennessee   4.463   5.177     9.640       33       17       10

``` r
# build a model predicting win team based on team CPR metrics and see which are most important

cpr_ppg = team_ppg |>
  left_join(team_cpr, by = "team") |>
  select(team, off_ppg, def_ppg, off_cpr, def_cpr)

winners_with_metrics = end |>
  mutate(home_win = ifelse(win_team == home, 1, 0)) |>
  left_join(cpr_ppg, by = c("home" = "team")) |>
  rename(home_off_ppg = off_ppg,
         home_def_ppg = def_ppg,
         home_off_cpr = off_cpr,
         home_def_cpr = def_cpr) |>
  left_join(cpr_ppg, by = c("away" = "team")) |>
  rename(away_off_ppg = off_ppg,
         away_def_ppg = def_ppg,
         away_off_cpr = off_cpr,
         away_def_cpr = def_cpr) |>
  select(home_win, home_off_ppg:away_def_cpr)

winners_with_metrics |> colnames()
```

    ## [1] "home_win"     "home_off_ppg" "home_def_ppg" "home_off_cpr" "home_def_cpr"
    ## [6] "away_off_ppg" "away_def_ppg" "away_off_cpr" "away_def_cpr"

``` r
set.seed(1251)
trainIndex = createDataPartition(winners_with_metrics$home_win, p = 0.7, list = F, times = 1)
train_data = winners_with_metrics[trainIndex, ]
test_data = winners_with_metrics[-trainIndex, ]

model = glm(home_win ~ ., data = train_data, family = "binomial")
predictions = predict(model, newdata = test_data, type = "response")
binary_predictions = ifelse(predictions > 0.5, 1, 0)

conf_matrix = confusionMatrix(data = as.factor(binary_predictions), reference = as.factor(test_data$home_win))
accuracy = paste0(round(conf_matrix$overall["Accuracy"] * 100, 2), "%")
f1_score = paste0(round(conf_matrix$byClass["F1"] * 100, 2), "%")

sprintf("model accuracy: %s", accuracy)
```

    ## [1] "model accuracy: 80.26%"

``` r
sprintf("model f1 score: %s", f1_score)
```

    ## [1] "model f1 score: 60.45%"

``` r
generate_matchup_metrics = function(home, away) {
  return(data.frame(home = home, away = away) |>
    left_join(cpr_ppg, by = c("home" = "team")) |>
    rename(home_off_ppg = off_ppg,
           home_def_ppg = def_ppg,
           home_off_cpr = off_cpr,
           home_def_cpr = def_cpr) |>
    left_join(cpr_ppg, by = c("away" = "team")) |>
    rename(away_off_ppg = off_ppg,
           away_def_ppg = def_ppg,
           away_off_cpr = off_cpr,
           away_def_cpr = def_cpr))
}

make_game_prediction = function(home, away) {
  my_matchup = generate_matchup_metrics(home = home, away = away)
  prob = predict(model, my_matchup, type = "response")
  if (prob > 0.5) {
    win_prob = paste0(round(as.numeric(prob) * 100, 1), "%")
  } else {
    win_prob = paste0(round(as.numeric(1 - prob) * 100, 1), "%")
  }
  
  if (prob > 0.5) {
    winner = my_matchup$home[1]
    loser = my_matchup$away[1]
  } else {
    winner = my_matchup$away[1]
    loser = my_matchup$home[1]
  }
  
  return(sprintf("%s def. %s (%s)", winner, loser, win_prob))
}

make_game_prediction(home = "North Carolina", away = "UConn")
```

    ## [1] "North Carolina def. UConn (71%)"

``` r
make_game_prediction(home = "UConn", away = "North Carolina")
```

    ## [1] "UConn def. North Carolina (67.8%)"

``` r
yesterday_scorigamis = end |>
  filter(date == Sys.Date() - 1) |>
  mutate(final_score = paste0(win_score, "-", lose_score)) |>
  left_join(score_counts, by = c("final_score" = "score")) |>
  filter(score_count == 1) |>
  mutate(desc = paste0("SCORIGAMI: ", win_team, " def. ", lose_team, " ", final_score),
         total_score = win_score + lose_score) |>
  arrange(desc(total_score)) |>
  pull(desc)

gami_counts = end |>
  filter(date == Sys.Date() - 1) |>
  mutate(final_score = paste0(win_score, "-", lose_score)) |>
  left_join(score_counts, by = c("final_score" = "score")) |>
  mutate(gami = ifelse(score_count == 1, 1, 0)) |>
  count(gami) |>
  pull(n)

gami_pct = paste0(round(gami_counts[2] / sum(gami_counts) * 100, 1), "%")
sprintf("%s of yesterday's games were Scorigamis", gami_pct)
```

    ## [1] "18.8% of yesterday's games were Scorigamis"

``` r
for (gami in yesterday_scorigamis) {
  print(gami)
}
```

    ## [1] "SCORIGAMI: Memphis def. Wichita State 112-86"
    ## [1] "SCORIGAMI: Tulane def. Tulsa 94-87"
    ## [1] "SCORIGAMI: Valparaiso def. Illinois State 59-50"

``` r
today_results = end |>
  filter(date == Sys.Date()) |>
  mutate(desc = paste0(win_team, " def. ", lose_team, " ", win_score, "-", lose_score),
         margin = win_score - lose_score) |>
  arrange(margin) |>
  pull(desc)

sprintf("%s games today", length(today_results))
```

    ## [1] "19 games today"

``` r
print("----------")
```

    ## [1] "----------"

``` r
for (res in today_results) {
  print(res)
}
```

    ## [1] "Brown def. Harvard 74-72"
    ## [1] "Texas A&M-Corpus Christi def. SE Louisiana 73-68"
    ## [1] "Siena def. Niagara 93-88"
    ## [1] "Northwestern State def. Houston Christian 69-64"
    ## [1] "Central Connecticut def. Merrimack 75-70"
    ## [1] "Sacred Heart def. Le Moyne 80-73"
    ## [1] "Michigan def. Ohio State 73-65"
    ## [1] "Iowa def. Minnesota 86-77"
    ## [1] "Wagner def. Stonehill 64-54"
    ## [1] "Fairleigh Dickinson def. St. Francis (PA) 81-71"
    ## [1] "Marquette def. Villanova 87-74"
    ## [1] "George Washington def. George Mason 75-62"
    ## [1] "Cornell def. Pennsylvania 77-60"
    ## [1] "Princeton def. Dartmouth 76-58"
    ## [1] "Yale def. Columbia 89-70"
    ## [1] "Saint Joseph's def. La Salle 82-62"
    ## [1] "Fairfield def. Canisius 88-63"
    ## [1] "Texas Southern def. Mississippi Valley State 93-61"
    ## [1] "Norfolk State def. Virginia-Lynchburg 118-73"

``` r
make_game_prediction(home = "North Carolina", away = "Marshall")
```

    ## [1] "North Carolina def. Marshall (97.2%)"

### BIGGEST PLAYS IN MOST RECENT HEELS GAME

``` r
most_recent_id = end |>
  filter(home == "North Carolina" | away == "North Carolina") |>
  slice_max(date, n = 1, with_ties = F) |>
  pull(game_id)

xxx = suppressMessages(get_pbp_game(game_ids = most_recent_id, extra_parse = F) |>
  select(home, away, home_score, away_score, half, time_remaining_half, secs_remaining, description, win_prob, naive_win_prob))

vec = xxx |>
  mutate(win_prob = round(win_prob, 5),
         naive_win_prob = round(naive_win_prob, 5),
         wp_lag = lag(win_prob),
         wp_delta = win_prob - wp_lag,
         wp_delta_abs = abs(wp_delta)) |>
  select(home, away, home_score, away_score, half, time_remaining_half, description, wp_delta_abs) |>
  slice_max(wp_delta_abs, n = 10, with_ties = F) |>
  mutate(leader = case_when(home_score > away_score ~ home, away_score > home_score ~ away, T ~ "Tied"),
         lead_score = case_when(home_score > away_score ~ home_score, away_score > home_score ~ away_score, T ~ home_score),
         trail_score = case_when(home_score > away_score ~ away_score, home_score < away_score ~ home_score, T ~ away_score),
         score = paste0(lead_score, "-", trail_score),
         delta = round(wp_delta_abs * 100, 2),
         str = paste0(half, "H ", time_remaining_half, " - ", description, " ", leader, " leads ", score, " (", delta, "% change)")) |>
  pull(str)

for (v in vec) {
  print(v)
}
```

    ## [1] "1H 18:10 - Chris Bell made Three Point Jumper. Assisted by Maliq Brown. North Carolina leads 3-2 (3.26% change)"
    ## [1] "1H 17:08 - Armando Bacot made Layup. Syracuse leads 4-3 (2.31% change)"
    ## [1] "1H 16:43 - Armando Bacot made Layup. Assisted by Harrison Ingram. Syracuse leads 6-3 (2.02% change)"
    ## [1] "1H 19:23 - Chris Bell missed Jumper. Syracuse leads 2-0 (1.95% change)"
    ## [1] "1H 11:18 - RJ Davis made Three Point Jumper. Assisted by Elliot Cadeau. Syracuse leads 22-10 (1.69% change)"
    ## [1] "1H 15:21 - Jalen Washington made Layup. Assisted by Cormac Ryan. Syracuse leads 13-7 (1.68% change)"
    ## [1] "1H 15:30 - Maliq Brown made Layup. Syracuse leads 11-7 (1.68% change)"
    ## [1] "1H 15:50 - Harrison Ingram made Layup. Syracuse leads 11-5 (1.66% change)"
    ## [1] "1H 16:01 - JJ Starling made Layup. Syracuse leads 9-5 (1.65% change)"
    ## [1] "1H 16:20 - Cormac Ryan made Layup. Syracuse leads 9-3 (1.64% change)"

1.  1H 18:10 - Chris Bell made Three Point Jumper. Assisted by Maliq
    Brown. North Carolina leads 3-2 (3.26% change)
2.  1H 17:08 - Armando Bacot made Layup. Syracuse leads 4-3 (2.31%
    change)
3.  1H 16:43 - Armando Bacot made Layup. Assisted by Harrison Ingram.
    Syracuse leads 6-3 (2.02% change)
4.  1H 19:23 - Chris Bell missed Jumper. Syracuse leads 2-0 (1.95%
    change)
5.  1H 11:18 - RJ Davis made Three Point Jumper. Assisted by Elliot
    Cadeau. Syracuse leads 22-10 (1.69% change)
6.  1H 15:21 - Jalen Washington made Layup. Assisted by Cormac Ryan.
    Syracuse leads 13-7 (1.68% change)
7.  1H 15:30 - Maliq Brown made Layup. Syracuse leads 11-7 (1.68%
    change)
8.  1H 15:50 - Harrison Ingram made Layup. Syracuse leads 11-5 (1.66%
    change)
9.  1H 16:01 - JJ Starling made Layup. Syracuse leads 9-5 (1.65% change)
10. 1H 16:20 - Cormac Ryan made Layup. Syracuse leads 9-3 (1.64% change)
