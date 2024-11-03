# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(ggdist)
library(ggblend)
library(riekelib)
library(cmdstanr)

# true underlying population/group characteristics
groups <-
  tibble(group = LETTERS[1:4],
         group_mean = c(0.4, 0.8, 0.7, 0.6),
         population = c(0.6, 0.2, 0.1, 0.1),
         p_respond = c(0.1, 0.05, 0.03, 0.01)) %>%
  mutate(p_sampled = population * p_respond,
         p_sampled = p_sampled/sum(p_sampled))

# true mean among population
true_mean <-
  groups %>%
  summarise(true_mean = sum(group_mean * population)) %>%
  pull(true_mean)

# simulate polls given differential response rates by group
n_sims <- 5000
sample_size <- 700
sigma <- 0.05

crossing(poll = 1:n_sims,
         differential_response = c(TRUE, FALSE),
         group_correlation = c(TRUE, FALSE),
         group = groups$group) %>%
  left_join(groups) %>%
  mutate(p_sampled = if_else(differential_response, p_sampled, population),
         group_mean = if_else(group_correlation, group_mean, true_mean)) %>%
  nest(data = -c(poll, differential_response, group_correlation)) %>%
  mutate(K = map(data, ~rmultinom(1, sample_size, .x$p_sampled)[,1])) %>%
  unnest(c(data, K)) %>%
  uncount(K) %>%
  bind_cols(Y = rnorm(nrow(.), .$group_mean, sigma)) %>%
  group_by(poll,
           group,
           differential_response,
           group_correlation) %>%
  mutate(n_group = n()) %>%
  group_by(poll,
           differential_response,
           group_correlation) %>%
  mutate(n_total = n(),
         observed = n_group/n_total,
         weight = population/observed) %>%
  summarise(weighted = sum(Y * weight)/sum(weight),
            unweighted = mean(Y)) %>%
  ungroup() %>%
  pivot_longer(ends_with("weighted"),
               names_to = "method",
               values_to = "p") %>%
  filter(differential_response,
         group_correlation,
         method == "weighted") %>%
  pull(p) %>%
  sd()
  mutate(case = case_when(differential_response & group_correlation ~ "Cell 4: Bias ↓ Variance ↓",
                          !differential_response & group_correlation ~ "Cell 3: Bias -- Variance ↓",
                          differential_response & !group_correlation ~ "Cell 2: Bias -- Variance ↑",
                          !differential_response & !group_correlation ~ "Cell 1: Bias -- Variance --")) %>%
  ggplot(aes(x = case,
             y = p,
             color = method,
             fill = method)) + 
  stat_histinterval(slab_alpha = 0.75) %>% partition(vars(method)) %>% blend("darken") +
  geom_hline(yintercept = true_mean,
             linetype = "dotted",
             color = "gray40") +
  scale_color_brewer(palette = "Set2") + 
  scale_fill_brewer(palette = "Set2") + 
  scale_y_percent() + 
  coord_flip() + 
  theme_rieke() + 
  theme(legend.position = "bottom") + 
  labs(title = "**Effect of weighting on estimates of the population mean**",
       subtitle = paste("Weighting on subgroups **decreases biase** and **decreases variance** when group",
                        "membership is highly correlated with response rate and preference in **continuous outcomes**",
                        sep = "<br>"),
       x = NULL,
       y = NULL,
       caption = paste("Distribution of weighted and unweighted population means for 5,000 simulated surveys",
                       "Dashed line indicates simulated true population mean",
                       sep = "<br>"))

groups %>%
  nest(data = everything()) %>%
  mutate(n = map(data, ~rmultinom(1, 1e4, .x$population)[,1])) %>%
  unnest(c(data, n)) %>%
  bind_cols(n0 = rbinom(nrow(.), .$n, .$p_respond)) %>%
  mutate(n1 = n - n0,
         pc = n/sum(n),
         v = pc^2/n0) %>%
  summarise(v = (sum(v) * 0.05) |> sqrt())

crossing(poll = 1:5000,
         differential_response = c(TRUE, FALSE),
         group_correlation = c(TRUE, FALSE),
         groups) %>%
  mutate(group_mean = if_else(group_correlation, group_mean, true_mean),
         p_respond = if_else(differential_response, p_respond, sum(groups$p_respond * groups$population))) %>%
  nest(data = -c(poll, differential_response, group_correlation)) %>%
  mutate(n = map(data, ~rmultinom(1, 1e4, .x$population)[,1])) %>%
  unnest(c(data, n)) %>%
  bind_cols(n0 = rbinom(nrow(.), .$n, .$p_respond)) %>%
  mutate(n1 = n - n0,
         p)

sims <- 
  groups %>%
  bind_cols(sigma = c(0.05, 0.075, 0.1, 0.15)) %>%
  nest(data = everything()) %>%
  mutate(n = map(data, ~rmultinom(1, 1e4, .x$population)[,1])) %>%
  unnest(c(data, n)) %>%
  bind_cols(n0 = rbinom(nrow(.), .$n, .$p_respond)) %>%
  uncount(n0) %>%
  bind_cols(Y = rnorm(nrow(.), .$group_mean, .$sigma)) %>%
  left_join(groups %>% select(group) %>% rowid_to_column("gid"))

wtmean <- cmdstan_model("posts/2024-11-01-aapor/wtmean.stan")

stan_data <-
  list(
    N = nrow(sims),
    G = nrow(groups),
    Y = sims$Y,
    gid = sims$gid,
    wt = groups$population
  )

wtfit <-
  wtmean$sample(
    data = stan_data,
    seed = 4321,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 2000
  )

# 0.527, 0.00245sd
# 0.534, 0.00595
wtfit$summary("wt_mean")

library(survey)
library(srvyr)

data(api)
data(scd)

apistrat %>%
  as_survey_design(1,
                   strata = stype,
                   fpc = fpc,
                   weight = pw,
                   variables = c(stype, starts_with("api"))) %>%
  mutate(api_diff = api00 - api99) %>%
  rename(api_students = api.stu) %>%
  summarise(api_diff = survey_mean(api_diff, vartype = "se"))

sims %>%
  nest(data = -group) %>%
  left_join(groups %>% select(group, population)) %>%
  mutate(observed = map_dbl(data, nrow),
         observed = observed/sum(observed),
         weight = population/observed) %>%
  select(group, data, weight) %>%
  unnest(data) %>%
  as_survey_design(1, 
                   strata = group,
                   weight = weight,
                   variables = Y) %>%
  summarise(pop_mean = survey_mean(Y, vartype = "se"))


