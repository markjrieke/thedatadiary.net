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
  select(id, name)

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


