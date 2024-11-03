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
# 0.533, 0.00501sd
wtfit$summary("wt_mean")

gpt_data <- 
  tibble(
    group = c('A', 'B', 'C'),
    gid = 1:3,
    group_mean = c(20, 25, 15),
    group_sd = c(5, 7, 4),
    n = c(500, 400, 600),
    wt = c(0.3, 0.4, 0.3)
  )

gpt_Y <- 
  gpt_data %>%
  uncount(n) %>%
  bind_cols(Y = rnorm(nrow(.), .$group_mean, .$group_sd))

stan_data <-
  list(
    N = nrow(gpt_Y),
    G = nrow(gpt_data),
    Y = gpt_Y$Y,
    gid = gpt_Y$gid,
    wt = gpt_data$wt
  )

wtfit <-
  wtmean$sample(
    data = stan_data,
    seed = 4321,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 5000
  )

wtfit$summary("wt_mean")

tmp <- 
  sims %>%
  group_by(group) %>%
  summarise(n = max(n),
            n0 = n(),
            mean = mean(Y),
            sd = sd(Y)) %>%
  mutate(proportion = n/sum(n))

weighted_mean <- 
  tmp %>%
  summarise(weighted_mean = sum(proportion * mean)) %>%
  pull(weighted_mean)

tmp %>%
  mutate(var = sd^2,
         var_contrib = (proportion^2/n0) * var) %>%
  summarise(variance_weighted_mean = sum(var_contrib)) %>%
  mutate(sd = sqrt(variance_weighted_mean))

gpt_data %>%
  mutate(var = group_sd^2 / n,
         weighted_var = wt^2 * var) %>%
  summarise(wt_var = sum(weighted_var)/sum(wt^2)) %>%
  mutate(wt_sd = sqrt(wt_var))

weighted_mean_sd <- sqrt(sum((data$wt^2) * (data$group_sd^2)) / (sum(data$wt))^2)

data <- tibble(
  value = c(10, 20, 30, 40),
  weight = c(2, 3, 5, 4),    # importance weights
  uncertainty = c(1.2, 0.5, 1.0, 1.5)  # standard deviation of each observation
)

wt_mean <- sum(gpt_data$group_mean * gpt_data$wt) / sum(gpt_data$wt)
wt_var <- sum(gpt_data$wt * ((gpt_data$group_mean - wt_mean)^2 + gpt_data$group_sd^2)) / sum(gpt_data$wt)
sqrt(wt_var)

sims %>%
  group_by(group) %>%
  summarise(n = max(n),
            n0 = n(),
            ymean = mean(Y),
            yvar = var(Y))
  
weighted.se.mean <- function(x, w) {
  
  # calculate effective N & correction factor
  n_eff <- (sum(w))^2 / sum(w^2)
  correction = n_eff / (n_eff - 1)
  
  # get weighted variance
  numerator <- sum(w * (x - weighted.mean(x, w))^2)
  denominator <- sum(w)
  
  # get weighted standard error of the mean
  se <- sqrt((correction * (numerator/denominator))/n_eff)
  return(se)
  
}

sims %>%
  select(-c(group_mean, population, p_respond, p_sampled, sigma)) %>%
  nest(data = Y) %>%
  mutate(n0 = map_int(data, nrow),
         o = n0/sum(n0),
         p = n/sum(n),
         wt = p/o,
         mean = map_dbl(data, ~mean(.x$Y)),
         wt_mean = sum(p * mean)/sum(p)) %>%
  unnest(data) %>%
  mutate(wt2 = wt^2,
         wY2 = wt * Y^2) %>%
  summarise(sum_wY2 = sum(wY2),
            sum_w = sum(wt),
            wt_mean = max(wt_mean),
            sum_w2 = sum(wt2)) %>%
  mutate(a = sum_wY2/sum_w - wt_mean^2,
         b = sum_w2 / (sum_w^2 - sum_w2),
         var = a * b,
         sd = sqrt(var))



  group_by(group) %>%
  summarise(n = max(n),
            n0 = n(),
            group_mean = mean(Y),
            group_sd = sd(Y)) %>%
  mutate(observed = n0/sum(n0),
         population = n/sum(n),
         wt = population/observed * n0) %>%
  nest(data = everything()) %>%
  mutate(wt_mean = map_dbl(data, ~sum(.x$wt * .x$group_mean)/sum(.x$wt)),
         wt_sd_mean = map_dbl(data, ~sqrt(sum((.x$wt^2) * (.x$group_sd^2)) / (sum(.x$wt))^2)))
  
  mutate(weighted_mean = map_dbl(data, ~weighted.mean(.x$Y, .x$w)),
         weighted_var = map_dbl(data, ~Hmisc::wtd.var(.x$Y, .x$w)),
         weighted_sd = sqrt(weighted_var))
  mutate(weighted_mean = map_dbl(data, ~weighted.mean(.x$Y, .x$w)),
         weighted_se_mean = map_dbl(data, ~weighted.se.mean(.x$Y, .x$w)),
         weighted_sd = map_dbl(data, ~weighted_se_mean))
  
  
  # Calculate the weighted mean and standard deviation of the weighted mean
  weighted_mean_sd <- sqrt(sum((data$wt^2) * (data$group_sd^2)) / (sum(data$wt))^2)
  
  # Display the result
  weighted_mean_sd




