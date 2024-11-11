# setup ------------------------------------------------------------------------

library(tidyverse)
library(riekelib)
library(cmdstanr)
  
groups <- 
  crossing(strata_1 = LETTERS[1:2],
           strata_2 = 1:2) %>%
  mutate(group = paste0(strata_1, strata_2),
         group_mean = c(0.97, 0.90, 0.10, 0.03),
         population = c(0.25, 0.25, 0.25, 0.25),
         p_respond = c(0.05, 0.05, 0.05, 0.05),
         p_sampled = population * p_respond,
         p_sampled = p_sampled/sum(p_sampled))
  

set.seed(123)
pollsters <-
  tibble(pollster = paste("Pollster", 1:20),
         strategy = c(rep("cross", 10), rep("single", 10))) %>%
  bind_cols(bias = rnorm(nrow(.), 0, 0.05)) %>%
  bind_cols(p_survey = gamlss.dist::rBE(nrow(.), 1/7, 0.1))  %>%
  bind_cols(target_sample = rnorm(nrow(.), 1000, 200))

wtmean <-
  cmdstan_model(
    "posts/2024-11-01-aapor/stan/wtmean.stan",
    dir = "posts/2024-11-01-aapor/exe/"
  )
  
simulate_survey <- function() {
  
  pollsters %>%
    bind_cols(survey = rbinom(nrow(.), 1, .$p_survey)) %>%
    filter(survey == 1) %>%
    select(-survey)
  
}

weighted_mean <- function(data,
                          strategy) {
  
  # weight by all adjustment cells or only by strata_2
  if (strategy == "cross") {
    
    strata <-
      data %>%
      arrange(group)
    
  } else {
    
    strata <-
      data %>%
      group_by(strata_2) %>%
      summarise(K = sum(K),
                Y = sum(Y),
                population = sum(population)) %>%
      arrange(strata_2)
    
  }
  
  # weight responses according to strategy
  strata <- 
    strata %>%
    rowid_to_column("gid") %>%
    mutate(observed = K/sum(K),
           weight = population/observed)
  
  # pass data to stan
  stan_data <-
    list(
      N = nrow(strata),
      G = max(strata$gid),
      Y = strata$Y,
      K = strata$K,
      gid = strata$gid,
      wt = strata$weight
    )
  
  # fit "model" (really is just a method for geting a simulation-based mean/sd)
  strata_fit <-
    wtmean$sample(
      data = stan_data,
      seed = 2024,
      chains = 4,
      parallel_chains = 4,
      iter_warmup = 2000,
      iter_sampling = 2000
    )
  
  # sampling summary for weighted mean
  out <- strata_fit$summary("wt_mean")
  
  return(out)
  
}

set.seed(4321)
polls <- 
  tibble(day = 1:90) %>%
  mutate(surveys = map(day, ~simulate_survey())) %>%
  unnest(surveys) %>%
  bind_cols(n = rpois(nrow(.), .$target_sample)) %>%
  mutate(group = list(groups$group),
         K = map(n, ~rmultinom(1, .x, groups$p_sampled)[,1])) %>%
  unnest(c(group, K)) %>%
  left_join(groups %>% select(group, strata_2, population, group_mean)) %>%
  bind_cols(Y = rbinom(nrow(.), .$K, .$group_mean)) %>%
  nest(data = -c(day, pollster, strategy)) %>%
  mutate(fit = pmap(list(data, strategy), ~weighted_mean(..1, ..2)))

polls <- 
  polls %>%
  mutate(n = map_int(data, ~sum(.x$K))) %>%
  left_join(pollsters %>% select(pollster, bias)) %>%
  unnest(fit) %>%
  mutate(across(c(mean, median, q5, q95),
                ~expit(logit(.x) + bias)))

binomial_data <- 
  polls %>%
  select(day,
         pollster,
         mean,
         n) %>%
  mutate(Y = round(mean * n),
         K = n,
         did = day,
         pid = parse_number(pollster))

binomial_model <-
  cmdstan_model(
    "posts/2024-11-01-aapor/stan/binomial.stan",
    dir = "posts/2024-11-01-aapor/exe/"
  )

stan_data <-
  list(
    N = nrow(binomial_data),
    D = 90,
    P = max(binomial_data$pid),
    did = binomial_data$did,
    pid = binomial_data$pid,
    K = binomial_data$K,
    Y = binomial_data$Y,
    sigma_p_sigma = 0.06,
    sigma_d_sigma = 0.02,
    alpha_mu = 0,
    alpha_sigma = 0.2,
    prior_check = 0
  )

binomial_fit <-
  binomial_model$sample(
    data = stan_data,
    seed = 2024,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 2000,
    init = 0.01,
    step_size = 0.002
  )

binomial_fit$summary("theta") %>%
  mutate(did = parse_number(variable)) %>%
  ggplot(aes(x = did,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(alpha = 0.125) + 
  geom_line() +
  expand_limits(y = c(0.4, 0.6))

binomial_fit$summary("beta_p") %>%
  mutate(pollster = paste("Pollster", parse_number(variable))) %>%
  left_join(pollsters) %>%
  left_join(binomial_data %>% count(pollster)) %>%
  mutate(pollster = paste0(pollster, " (", n, ")"),
         pollster = fct_reorder(pollster, bias)) %>%
  ggplot(aes(x = pollster,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_pointrange() +
  geom_point(aes(y = bias),
             color = "red") +
  coord_flip() +
  facet_wrap(~strategy, scales = "free") +
  expand_limits(y = c(-0.1, 0.1))

beta_data <- 
  polls %>%
  select(day,
         pollster,
         mean,
         sd) %>%
  mutate(Y = mean,
         sigma = sd,
         did = day,
         pid = parse_number(pollster))

beta_model <-
  cmdstan_model(
    "posts/2024-11-01-aapor/stan/beta.stan",
    dir = "posts/2024-11-01-aapor/exe/"
  )

stan_data <-
  list(
    N = nrow(beta_data),
    D = 90,
    P = max(beta_data$pid),
    did = beta_data$did,
    pid = beta_data$pid,
    Y = beta_data$Y,
    sigma = beta_data$sigma,
    sigma_p_sigma = 0.06,
    sigma_d_sigma = 0.02,
    alpha_mu = 0,
    alpha_sigma = 0.2,
    prior_check = 0
  )

beta_fit <-
  beta_model$sample(
    data = stan_data,
    seed = 2024,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 2000,
    init = 0.01,
    step_size = 0.002
  )

beta_fit$summary("theta") %>%
  mutate(did = parse_number(variable)) %>%
  ggplot(aes(x = did,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(alpha = 0.125) + 
  geom_line() +
  expand_limits(y = c(0.4, 0.6))

beta_fit$summary("beta_p") %>%
  mutate(pollster = paste("Pollster", parse_number(variable))) %>%
  left_join(pollsters) %>%
  left_join(binomial_data %>% count(pollster)) %>%
  mutate(pollster = paste0(pollster, " (", n, ")"),
         pollster = fct_reorder(pollster, bias)) %>%
  ggplot(aes(x = pollster,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_pointrange() +
  geom_point(aes(y = bias),
             color = "red") +
  coord_flip() +
  facet_wrap(~strategy, scales = "free") +
  expand_limits(y = c(-0.1, 0.1))

normal_data <- beta_data

normal_model <-
  cmdstan_model(
    "posts/2024-11-01-aapor/stan/normal.stan",
    dir = "posts/2024-11-01-aapor/exe/"
  )

stan_data <-
  list(
    N = nrow(normal_data),
    D = 90,
    P = max(normal_data$pid),
    did = normal_data$did,
    pid = normal_data$pid,
    Y = normal_data$Y,
    sigma = normal_data$sigma,
    sigma_p_sigma = 0.06,
    sigma_d_sigma = 0.02,
    alpha_mu = 0,
    alpha_sigma = 0.2,
    prior_check = 0
  )

normal_fit <-
  normal_model$sample(
    data = stan_data,
    seed = 2024,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 2000,
    init = 0.01,
    step_size = 0.002
  )

normal_fit$summary("theta") %>%
  mutate(did = parse_number(variable)) %>%
  ggplot(aes(x = did,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_ribbon(alpha = 0.125) + 
  geom_line() +
  expand_limits(y = c(0.4, 0.6))

normal_fit$summary("beta_p") %>%
  mutate(pollster = paste("Pollster", parse_number(variable))) %>%
  left_join(pollsters) %>%
  left_join(binomial_data %>% count(pollster)) %>%
  mutate(pollster = paste0(pollster, " (", n, ")"),
         pollster = fct_reorder(pollster, bias)) %>%
  ggplot(aes(x = pollster,
             y = median,
             ymin = q5,
             ymax = q95)) + 
  geom_pointrange() +
  geom_point(aes(y = bias),
             color = "red") +
  coord_flip() +
  facet_wrap(~strategy, scales = "free") +
  expand_limits(y = c(-0.1, 0.1))
