# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(riekelib)
library(cmdstanr)
library(ggdist)
library(ggblend)

# plot colors
dd_green <- "#5A9282"
dd_oppo <- "#c0aa72"
  
# true conditions --------------------------------------------------------------

# group-level probability of support, population proportion, and probability of response
groups <- 
  crossing(strata_1 = LETTERS[1:2],
           strata_2 = 1:2) %>%
  mutate(group = paste0(strata_1, strata_2),
         group_mean = c(0.97, 0.90, 0.10, 0.03),
         population = c(0.25, 0.25, 0.25, 0.25),
         p_respond = c(0.05, 0.05, 0.05, 0.05),
         p_sampled = population * p_respond,
         p_sampled = p_sampled/sum(p_sampled))
  
# pollster bias, frequency of surveying, and target sample size
set.seed(123)
pollsters <-
  tibble(pollster = paste("Pollster", 1:20),
         strategy = c(rep("cross", 10), rep("single", 10))) %>%
  bind_cols(bias = rnorm(nrow(.), 0, 0.05)) %>%
  bind_cols(p_survey = gamlss.dist::rBE(nrow(.), 1/7, 0.1))  %>%
  bind_cols(target_sample = rnorm(nrow(.), 1000, 200))

# stan models ------------------------------------------------------------------

# util stan "model" for estimating the weighted mean/sd
wtmean <-
  cmdstan_model(
    "posts/2024-11-01-aapor/stan/wtmean.stan",
    dir = "posts/2024-11-01-aapor/exe/"
  )

# poll aggregation model with a binomial likelihood
binomial_model <-
  cmdstan_model(
    "posts/2024-11-01-aapor/stan/binomial.stan",
    dir = "posts/2024-11-01-aapor/exe/"
  )

# poll aggregation model with a beta likelihood (mean-variance parameterization)
beta_model <-
  cmdstan_model(
    "posts/2024-11-01-aapor/stan/beta.stan",
    dir = "posts/2024-11-01-aapor/exe/"
  )

# poll aggregation model with a normal likelihood
normal_model <-
  cmdstan_model(
    "posts/2024-11-01-aapor/stan/normal.stan",
    dir = "posts/2024-11-01-aapor/exe/"
  )

# simulation functions ---------------------------------------------------------
  
# simulate whether (or not) each pollster conducts a survey on any given day
simulate_survey <- function() {
  
  pollsters %>%
    bind_cols(survey = rbinom(nrow(.), 1, .$p_survey)) %>%
    filter(survey == 1) %>%
    select(-survey)
  
}

# compute the weighted mean/sd for a survey given a weighting strategy
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

# plotting functions -----------------------------------------------------------

# extract the likelihood from a model fit to be used in the plot
extract_likelihood <- function(model) {
  
  # extract raw stan code
  model_code <- 
    model$code() %>%
    str_c(collapse = "\n")
  
  # infer likelihood from model code
  likelihood <- 
    case_when(
      str_detect(model_code, "binomial_logit_lpmf\\(Y \\| K, mu\\)") ~ "binomial",
      str_detect(model_code, "beta_lpdf\\(Y \\| alpha_poll, beta_poll\\)") ~ "beta",
      str_detect(model_code, "normal_lpdf\\(Y \\| mu, sigma\\)") ~ "normal"
    )
  
  return(likelihood)
  
}

# plot the modeled two-party voteshare along with the distribution of polls
plot_voteshare <- function(model, cred_level = 0.95) {
  
  # extract likelihood to use in chart
  likelihood <- extract_likelihood(model)
  
  # extract parameter draws for voteshare
  draws <- 
    model$draws("theta", format = "df") %>%
    as_tibble()
  
  # used in caption
  n_draws <- nrow(draws)
  
  # plot!
  out <- 
    draws %>% 
    pivot_longer(starts_with("theta"),
                 names_to = "did",
                 values_to = "theta") %>%
    nest(data = -did) %>%
    mutate(did = parse_number(did),
           med = map_dbl(data, ~quantile(.x$theta, probs = 0.5)),
           .lower = map_dbl(data, ~quantile(.x$theta, probs = (1 - cred_level)/2)),
           .upper = map_dbl(data, ~quantile(.x$theta, probs = (1 - cred_level)/2 + cred_level))) %>%
    ggplot(aes(x = did,
               y = med,
               ymin = .lower,
               ymax = .upper)) + 
    geom_ribbon(alpha = 0.4,
                fill = dd_green) +
    geom_point(data = polls,
               mapping = aes(x = day,
                             y = mean,
                             size = n,
                             ymin = NULL,
                             ymax = NULL),
               shape = 21,
               color = dd_green,
               alpha = 0.5) + 
    geom_line(color = dd_green,
              linewidth = 0.8) %>% copy_under(color = "white", linewidth = 1.6) + 
    scale_y_percent() + 
    theme_rieke() + 
    theme(legend.position = "none") + 
    labs(title = "**A Summary of Simulated Sentiment**",
         subtitle = glue::glue("Two-party candidate voteshare as modeled by a ",
                               "**{color_text(likelihood, dd_green)}** likelihood"),
         x = "Day of campaign",
         y = NULL,
         caption = glue::glue("Shaded region indicates {scales::label_percent(accuracy = 1)(cred_level)} ",
                              "posterior credible<br>",
                              "interval based on {scales::label_comma()(n_draws)} MCMC samples")) +
    expand_limits(y = c(0.4, 0.6))
  
  return(out)
  
}

# plot the parameter estimates as fitted by the model
plot_parameters <- function(model) {
  
  # extract likelihood to use in chart
  likelihood <- extract_likelihood(model)
  
  # extract parameter draws for voteshare
  draws <- 
    model$draws("beta_p", format = "df") %>%
    as_tibble()
  
  # used in caption
  n_draws <- nrow(draws)
  
  # plot!
  out <- 
    draws %>%
    pivot_longer(starts_with("beta"),
                 names_to = "pollster",
                 values_to = "beta") %>%
    mutate(pollster = paste("Pollster", parse_number(pollster))) %>%
    group_by(pollster) %>%
    median_qi(beta, .width = c(0.66, 0.95)) %>%
    left_join(pollsters) %>%
    left_join(polls %>% count(pollster)) %>%
    mutate(pollster = glue::glue("{pollster} ({n})"),
           pollster = fct_reorder(pollster, bias),
           strategy = if_else(strategy == "cross",
                              "Weight on Highly Correlated Vars",
                              "Weight on Mildly Correlated Vars")) %>%
    ggplot(aes(x = pollster,
               y = beta,
               ymin = .lower,
               ymax = .upper)) +
    geom_pointinterval(color = dd_green) + 
    geom_point(aes(y = bias),
               color = dd_oppo,
               size = 3,
               alpha = 0.75) +
    facet_wrap(~strategy, scales = "free_y") + 
    coord_flip() + 
    theme_rieke() +
    labs(title = "**Poll-model Parameters**",
         subtitle = glue::glue("**{color_text('Parameter estimates', dd_green)}** and ",
                               "**{color_text('simulated values', dd_oppo)}** of pollster bias as modeled by a ",
                               "**{color_text(likelihood, dd_green)}** likelihood"),
         x = NULL,
         y = "\u03b2<sub>p</sub> (logit scale)",
         caption = glue::glue("Pointrange indicates 66/95% posterior credible",
                              "interval based on {scales::label_comma()(n_draws)} MCMC samples",
                              .sep = "<br>")) + 
    expand_limits(y = c(-0.13, 0.13))
  
  return(out)
  
}

# simulate polls ---------------------------------------------------------------

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

# binomial model ---------------------------------------------------------------

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

# plots
binomial_fit %>% plot_voteshare()
binomial_fit %>% plot_parameters()

# beta model -------------------------------------------------------------------

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

beta_fit %>% plot_voteshare()
beta_fit %>% plot_parameters()

# normal model -----------------------------------------------------------------

# normal model uses the same data as the beta model
normal_data <- beta_data

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

normal_fit %>% plot_voteshare()
normal_fit %>% plot_parameters()
