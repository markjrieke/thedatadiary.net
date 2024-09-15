library(tidyverse)
library(riekelib)
library(cmdstanr)

# actually 01-02 ---------------------------------------------------------------

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

# 3-player game - split point model --------------------------------------------

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

# team game - split point model ------------------------------------------------

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

# 3-player game - 0-1 correct responses ----------------------------------------

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

# 3-player game - 2 correct responses ------------------------------------------

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

# 4-player game - 0-1 correct responses ----------------------------------------

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

# team games - 0/1 correct responses -------------------------------------------

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

# compile model
actually_model <-
  cmdstan_model("posts/2024-08-28-actually/actualy_04.stan")

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

# blegh ------------------------------------------------------------------------

# just episodes without split points / no team episodes
easy_eps <- 
  episodes %>%
  filter(!eid %in% c("s03e02", "s05e01", "s05e21")) %>%
  unnest(questions) %>%
  mutate(n_winners = map_int(winners, length)) %>%
  select(eid, n_winners) %>%
  group_by(eid) %>%
  summarise(n_winners = max(n_winners)) %>%
  filter(n_winners <= 1) %>%
  pull(eid)

episodes <- 
  episodes %>%
  filter(eid %in% easy_eps)

# just episodes with 3 players
easy_eps <- 
  episodes %>%
  mutate(n_players = map_int(players, nrow)) %>%
  filter(n_players <= 3) %>%
  pull(eid)

episodes <- 
  episodes %>%
  filter(eid %in% easy_eps)

# assign player ids
pids <- 
  episodes %>%
  unnest(players) %>%
  left_join(people) %>%
  distinct(name) %>%
  arrange(name) %>%
  rowid_to_column("pid")

# map players to episodes
eid_pid <- 
  episodes %>%
  unnest(players) %>%
  left_join(people) %>%
  left_join(pids) %>%
  mutate(position = paste0("p", position)) %>%
  select(eid,
         position,
         pid) %>%
  pivot_wider(names_from = position,
              values_from = pid)

# map players to position
positions <- 
  episodes %>%
  unnest(players) %>%
  left_join(people) %>%
  left_join(pids) %>%
  select(eid, pid, position) %>%
  mutate(position = paste0("p", position))

responses <- 
  episodes %>%
  unnest(questions) %>%
  select(eid, 
         question = number,
         winners) %>%
  mutate(n_winners = map_int(winners, length),
         winners = if_else(n_winners == 0, list("host"), winners)) %>%
  select(-n_winners) %>%
  unnest(winners) %>%
  left_join(people, by = c("winners" = "id")) %>%
  left_join(pids) %>%
  left_join(positions) %>%
  mutate(position = replace_na(position, "host")) %>%
  count(eid, position) %>%
  pivot_wider(names_from = position,
              values_from = n) %>%
  rename_with(~str_replace(.x, "p", "y")) %>%
  mutate(across(c(host, starts_with("y")), ~replace_na(.x, 0)))

results <- 
  eid_pid %>%
  left_join(responses) %>%
  mutate(K = y1 + y2 + y3 + host) 

pim <- 
  results %>%
  select(starts_with("p")) %>%
  as.matrix()

R <- 
  results %>%
  select(starts_with("y"), 
         host) %>%
  as.matrix()

stan_data <- 
  list(
    N = nrow(results),
    K = results$K,
    R = R
  )

actually_model <- 
  cmdstan_model("actually_01.stan")

actually_fit <-
  actually_model$sample(
    data = stan_data,
    seed = 2024,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 4,
    parallel_chains = 4,
    init = 0.01,
    step_size = 0.002
  )

stan_data <- 
  list(
    N = nrow(results),
    P = nrow(pids),
    K = results$K,
    R = R,
    pim = pim
  )

actually_model <- 
  cmdstan_model("posts/2024-08-28-actually/actually_02.stan")

actually_fit <-
  actually_model$sample(
    data = stan_data,
    seed = 2024,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 4,
    parallel_chains = 4,
    init = 0.01,
    step_size = 0.002
  )

actually_fit$summary("eta_p") -> tmp

tmp %>%
  mutate(pid = parse_number(variable)) %>% 
  left_join(pids) %>%
  mutate(name = fct_reorder(name, median)) %>%
  ggplot(aes(x = name,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_pointrange() + 
  coord_flip() +
  riekelib::theme_rieke()
  
str_split_teams <- function(str) {
  
  str %>%
    str_flatten(collapse = "|") %>%
    str_replace("kaitlin-and-jess", "kaitlin-thompson|jessica-ross") %>%
    str_replace("matt-and-marisha", "matt-mercer|marisha-ray") %>%
    str_replace("becca-and-david", "becca-scott|david-kerns") %>%
    str_replace("justin-and-ben", "justin-matson|benjamin-siemon") %>%
    str_replace("ify-and-brodie", "ify-nwadiwe|brodie-reed") %>%
    str_replace("dani-and-danielle", "dani-fernandez|danielle-radford") %>%
    str_split_1("\\|")
  
}

actually %>%
  select(episodes) %>%
  unnest(episodes) %>% 
  select(season = season_number,
         episode = number,
         questions) %>% 
  filter(season <= 8) %>%
  unnest(questions) %>%
  select(season,
         episode,
         question = number,
         winners) %>% 
  mutate(n_winners = map_int(winners, length),
         winners = if_else(n_winners == 0, list("host"), winners),
         winners = map(winners, str_split_teams),
         eid = glue::glue("S{season}E{episode}")) %>%
  filter(n_winners <= 1,
         !eid %in% c("S3E2", "S5E1", "S5E21")) %>%
  unnest(winners) %>%
  rename(id = winners) %>%
  left_join(people) %>%
  mutate(name = replace_na(name, "host")) %>%
  group_by(season, 
           episode) %>%
  count(name)

actually %>%
  select(episodes) %>%
  unnest(episodes) %>% 
  select(season = season_number,
         episode = number,
         players) %>%
  unnest(players) %>%
  filter(season <= 8) %>%
  group_by(season, episode) %>%
  filter(score == max(score)) %>%
  ungroup() %>%
  count(id) %>%
  arrange(desc(n)) %>%
  left_join(people) %>%
  slice_head(n = 10) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(x = name,
             y = n)) + 
  geom_col() + 
  coord_flip() +
  expand_limits(y = c(0, 10))
  
# actually 03 ------------------------------------------------------------------

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

episodes <- 
  episodes %>%
  filter(!eid %in% c("s03e02", "s05e01", "s05e21")) %>%
  mutate(n_players = map_int(players, nrow)) %>%
  filter(n_players == 3) %>%
  select(-n_players) %>%
  unnest(questions) %>%
  mutate(n_winners = map_int(winners, length)) %>%
  select(eid, n_winners) %>%
  mutate(category = case_when(n_winners <= 1 ~ "n_0_1",
                              n_winners == 2 ~ "n_2",
                              n_winners == 3 ~ "n_3")) %>%
  group_by(eid) %>%
  count(category) %>%
  ungroup() %>%
  pivot_wider(names_from = category,
              values_from = n,
              values_fill = 0) %>%
  mutate(n_tot = n_0_1 + n_2 + n_3)

actually_03 <-
  cmdstan_model("posts/2024-08-28-actually/actually_03.stan")


