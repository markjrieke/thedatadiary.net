# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)
library(cmdstanr)

# import results ---------------------------------------------------------------

actually <- 
  jsonlite::fromJSON("https://raw.githubusercontent.com/tekkamanendless/umactually/master/data.json") %>%
  map_if(is.data.frame, list) %>%
  as_tibble()

people <- 
  actually %>%
  unnest(people) %>%
  select(id, name) %>%
  rowid_to_column("pid")

episodes <-
  actually %>%
  select(episodes) %>%
  unnest(episodes) %>%
  select(eid = dropouttv_productid,
         season = season_number,
         episode = number,
         players,
         questions) %>%
  filter(season <= 8)

# how many points were awarded in a three-player game? -------------------------

# number of times in each 3-player game that points are awarded to 0/1 player,
# 2 players, or all 3 players
split_point_3 <- 
  episodes %>%
  filter(!eid %in% c("s03e02", "s05e01", "s05e21")) %>%
  mutate(n_players = map_int(players, nrow)) %>%
  filter(n_players <= 3) %>%
  unnest(questions) %>%
  mutate(n_winners = map_int(winners, length)) %>%
  select(eid, n_winners) %>%
  mutate(n_winners = if_else(n_winners %in% 0:1, "0_1", as.character(n_winners))) %>%
  count(eid, n_winners) %>%
  mutate(n_winners = paste0("n_", n_winners)) %>%
  pivot_wider(names_from = n_winners,
              values_from = n,
              values_fill = 0)

# how many points were awarded in a team game? ---------------------------------

split_point_team <- 
  episodes %>%
  filter(eid %in% c("s03e02", "s05e01", "s05e21")) %>%
  unnest(questions) %>%
  mutate(n_winners = map_int(winners, length)) %>% 
  select(eid, n_winners) %>%
  mutate(n_winners = if_else(n_winners %in% 0:1, "0_1", as.character(n_winners))) %>%
  count(eid, n_winners) %>%
  mutate(n_winners = paste0("n_", n_winners)) %>%
  pivot_wider(names_from = n_winners,
              values_from = n,
              values_fill = 0)

# which player earned the point when one is awarded in a three-player game? ----

questions_301 <- 
  episodes %>%
  filter(!eid %in% c("s03e02", "s05e01", "s05e21")) %>%
  mutate(n_players = map_int(players, nrow)) %>%
  filter(n_players == 3) %>%
  unnest(questions) %>%
  mutate(n_winners = map_int(winners, length)) %>%
  filter(n_winners <= 1)

positions_301 <- 
  questions_301 %>%
  unnest(players) %>%
  distinct(eid, position, id) %>%
  left_join(people) %>%
  select(eid, pid, position) %>%
  mutate(position = paste0("player_", position))

responses_301 <- 
  questions_301 %>%
  select(eid,
         question = number,
         winners) %>%
  mutate(n_winners = map_int(winners, length),
         id = if_else(n_winners == 0, list("host"), winners)) %>%
  select(-ends_with("winners")) %>%
  unnest(id) %>%
  left_join(people) %>%
  left_join(positions_301) %>%
  mutate(position = replace_na(position, "host")) %>%
  count(eid, position) %>%
  pivot_wider(names_from = position,
              values_from = n,
              values_fill = 0) %>%
  relocate(host, .after = player_3)

pim_301 <- 
  positions_301 %>%
  pivot_wider(names_from = position,
              values_from = pid)

# which players earned points when two are awarded in a three-player game? -----

questions_32 <- 
  episodes %>%
  filter(!eid %in% c("s03e02", "s05e01", "s05e21")) %>%
  mutate(n_players = map_int(players, nrow)) %>%
  filter(n_players == 3) %>%
  unnest(questions) %>%
  mutate(n_winners = map_int(winners, length)) %>%
  filter(n_winners == 2)

positions_32 <- 
  questions_32 %>%
  unnest(players) %>%
  distinct(eid, position, id) %>%
  left_join(people) %>%
  select(eid, pid, position) %>%
  mutate(position = paste0("player_", position))

pim_32 <-
  positions_32 %>%
  pivot_wider(names_from = position,
              values_from = pid)

responses_32 <- 
  questions_32 %>%
  select(eid,
         question = number,
         winners) %>%
  unnest(winners) %>%
  rename(id = winners) %>%
  left_join(people) %>%
  left_join(positions_32) %>%
  count(eid, question, position) %>%
  pivot_wider(names_from = position,
              values_from = n,
              values_fill = 0) %>%
  rename_with(~str_replace(.x, "player", "response")) %>%
  left_join(pim_32) %>%
  select(-question)

pim_32 <- 
  responses_32 %>%
  select(eid, starts_with("player"))

responses_32 <- 
  responses_32 %>%
  select(eid, starts_with("response"))

# which player earned a point when one was awarded in a four-player game? ------

questions_4 <- 
  episodes %>%
  filter(!eid %in% c("s03e02", "s05e01", "s05e21")) %>%
  mutate(n_players = map_int(players, nrow)) %>%
  filter(n_players == 4) %>%
  unnest(questions)

positions_4 <- 
  questions_4 %>%
  unnest(players) %>%
  distinct(eid, position, id) %>%
  left_join(people) %>%
  select(eid, pid, position) %>%
  mutate(position = paste0("player_", position))

responses_4 <- 
  questions_4 %>%
  select(eid,
         question = number,
         winners) %>%
  mutate(n_winners = map_int(winners, length),
         id = if_else(n_winners == 0, list("host"), winners)) %>%
  select(-ends_with("winners")) %>%
  unnest(id) %>%
  left_join(people) %>%
  left_join(positions_4) %>%
  mutate(position = replace_na(position, "host")) %>%
  count(eid, position) %>%
  pivot_wider(names_from = position,
              values_from = n,
              values_fill = 0) %>%
  relocate(host, .after = player_4)

pim_4 <- 
  positions_4 %>%
  pivot_wider(names_from = position,
              values_from = pid)

# which team earned a point when one was awarded in a team game? ---------------

questions_teams <- 
  actually %>%
  select(episodes) %>%
  unnest(episodes) %>%
  select(eid = dropouttv_productid,
         season = season_number,
         episode = number,
         teams,
         questions) %>%
  filter(eid %in% c("s03e02", "s05e01", "s05e21")) %>%
  unnest(questions)

teams <- 
  actually %>%
  unnest(teams) %>%
  unnest(players) %>%
  select(id, players)

positions_teams <- 
  questions_teams %>%
  unnest(teams) %>%
  distinct(eid, position, id) %>%
  right_join(teams) %>%
  arrange(eid, position, id, players) %>%
  group_by(eid) %>%
  mutate(rank = 1,
         team_position = position,
         position = cumsum(rank)) %>%
  ungroup() %>%
  select(eid,
         position,
         team_id = id,
         team_position,
         id = players) %>%
  mutate(position = paste0("player_", position))

responses_teams <- 
  questions_teams %>%
  select(eid,
         question = number,
         winners) %>%
  mutate(n_winners = map_int(winners, length),
         team_id = if_else(n_winners == 0, list("host"), winners)) %>%
  filter(n_winners <= 1) %>%
  select(-ends_with("winners")) %>%
  unnest(team_id) %>%
  count(eid, team_id) %>%
  left_join(positions_teams %>% distinct(eid, team_id, team_position)) %>%
  mutate(team_position = if_else(is.na(team_position), 
                                 "host", 
                                 paste0("team_", team_position))) %>%
  select(eid, team_position, n) %>%
  pivot_wider(names_from = team_position,
              values_from = n,
              values_fill = 0)

pim_teams <- 
  positions_teams %>%
  left_join(people) %>%
  select(eid, pid, position) %>%
  pivot_wider(names_from = position,
              values_from = pid)

# model! -----------------------------------------------------------------------

# compile model
actually_model <-
  cmdstan_model(
    "posts/2024-10-06-actually/stan/actually.stan",
    dir = "posts/2024-10-06-actually/exe/"
  )

stan_data <-
  list(
    N3 = nrow(split_point_3),
    N301 = nrow(responses_301),
    N32 = nrow(responses_32),
    N4 = nrow(responses_4),
    NT = nrow(split_point_team),
    NT01 = nrow(responses_teams),
    P = nrow(people),
    pim_301 = as.matrix(pim_301[,2:4]),
    pim_32 = as.matrix(pim_32[,2:4]),
    pim_4 = as.matrix(pim_4[,2:5]),
    pim_teams = as.matrix(pim_teams[,2:5]),
    S3 = as.matrix(split_point_3[,2:4]),
    ST = as.matrix(split_point_team[,2:3]),
    K301 = rowSums(responses_301[,2:5]),
    K4 = rowSums(responses_4[,2:6]),
    KT01 = rowSums(responses_teams[,2:4]),
    R301 = as.matrix(responses_301[,2:5]),
    R32 = as.matrix(responses_32[,2:4]),
    R4 = as.matrix(responses_4[,2:6]),
    RT01 = as.matrix(responses_teams[,2:4]),
    kappa_mu = 0,
    kappa_sigma = 1.5,
    delta_mu = -2,
    delta_sigma = 0.5,
    alpha_mu = 0,
    alpha_sigma = 1,
    sigma_p_mu = 0,
    sigma_p_sigma = 1
  )

actually_fit <-
  actually_model$sample(
    data = stan_data,
    seed = 12345,
    iter_warmup = 2000,
    iter_sampling = 2000,
    init = 0.01,
    step_size = 0.002,
    chains = 4,
    parallel_chains = 4
  )

# explore output ---------------------------------------------------------------

# raw skill parameters
player_skill <-
  actually_fit$draws("beta", format = "df") %>%
  as_tibble()

player_skill <- 
  player_skill %>%
  pivot_longer(starts_with("beta"),
               names_to = "variable",
               values_to = "estimate") %>%
  nest(data = -variable) %>%
  mutate(pid = parse_number(variable)) %>%
  left_join(people) %>%
  select(name, data) %>%
  nplyr::nest_select(data, .draw, estimate)

# average rank based on skill
avg_rank <-
  player_skill %>% 
  unnest(data) %>%
  group_by(.draw) %>%
  arrange(desc(estimate)) %>%
  mutate(rank = rank(-estimate)) %>%
  group_by(name) %>%
  summarise(rank_score = mean(rank)) %>%
  arrange(rank_score) %>%
  rowid_to_column("rank")

# probability that each player is the best player
prob_best <- 
  actually_fit$summary("prob_best")

prob_best <- 
  prob_best %>%
  mutate(pid = parse_number(variable)) %>%
  left_join(people) %>%
  select(name, 
         prob_best = mean) 

alpha <- actually_fit$summary("alpha")

# write main results -----------------------------------------------------------

avg_rank %>%
  write_csv("posts/2024-10-06-actually/out/avg_rank.csv")

prob_best %>%
  write_csv("posts/2024-10-06-actually/out/prob_best.csv")

player_skill %>%
  write_csv("posts/2024-10-06-actually/out/player_skill.csv")

alpha %>%
  write_csv("posts/2024-10-06-actually/out/alpha.csv")

# game-specific simulations ----------------------------------------------------

summarise_game <- function(p1, p2, p3) {
  
  actually_fit$draws(glue::glue("game_{p1}_{p2}_{p3}"), format = "df") %>%
    as_tibble() %>%
    pivot_longer(starts_with("game"),
                 names_to = "variable",
                 values_to = "score") %>%
    nest(data = -variable) %>%
    mutate(pid = case_when(str_detect(variable, "\\[1\\]") ~ p1,
                           str_detect(variable, "\\[2\\]") ~ p2,
                           str_detect(variable, "\\[3\\]") ~ p3)) %>%
    left_join(people) %>%
    select(name, data) %>%
    unnest(data) %>%
    group_by(name) %>%
    tidybayes::median_qi(score, .width = c(0.66, 0.95)) %>%
    write_csv(glue::glue("posts/2024-10-06-actually/out/scores_{p1}_{p2}_{p3}.csv"))
  
  actually_fit$summary(glue::glue("prob_{p1}_{p2}_{p3}")) %>%
    mutate(pid = case_when(str_detect(variable, "\\[1\\]") ~ p1,
                           str_detect(variable, "\\[2\\]") ~ p2,
                           str_detect(variable, "\\[3\\]") ~ p3)) %>%
    left_join(people) %>%
    select(name, p_win = mean) %>%
    write_csv(glue::glue("posts/2024-10-06-actually/out/prob_{p1}_{p2}_{p3}.csv"))
  
}

summarise_game(7, 25, 68)
summarise_game(57, 153, 168)
summarise_game(11, 63, 128)
summarise_game(24, 27, 51)




