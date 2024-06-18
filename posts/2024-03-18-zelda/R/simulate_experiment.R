# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(riekelib)

# util function for converting from mean/variance to alpha/beta parameterization
beta_alpha <- function(mu, sigma) {
  mu * (1 - sigma^2)/sigma^2
}

# util function for converting from mean/variance to alpha/beta parameterization
beta_beta <- function(mu, sigma) {
  alpha <- beta_alpha(mu, sigma)
  beta <- alpha * (1 - mu)/mu
  return(beta)
}

# ccdf of beta discrete weibull distribution
beta_discrete_weibull_ccdf <- function(x, a = 1, b = 1, c = 1) {
  lp <- lbeta(a + x^c, b) - lbeta(a, b)
  out <- exp(lp)
  return(out)
}

# ccdf of beta geometric distribution
beta_geometric_ccdf <- function(x, a = 1, b = 1) {
  lp <- lbeta(a + x, b) - lbeta(a, b)
  out <- exp(lp)
  return(out)
}

# experimenta/player data
n_days <- 150
daily_players <- 5000

# parameters for simulating data
mu_bdw <- c(0.35, 0.4)
sigma_bdw <- c(0.075, 0.08)
mu_bg <- c(0.97, 0.97)
sigma_bg <- c(0.015, 0.05)
kappa <- c(0.25, 0.225)
phi <- c(3, 6)
eta <- c(-0.25, -0.5)

# convert from mean/variance to alpha/beta
alpha_bdw <- beta_alpha(mu_bdw, sigma_bdw)
beta_bdw <- beta_beta(mu_bdw, sigma_bdw)
alpha_bg <- beta_alpha(mu_bg, sigma_bg)
beta_bg <- beta_beta(mu_bg, sigma_bg)

# simulate data ----------------------------------------------------------------

# generate sampling probabilities per arm
probs <- 
  crossing(arm = c("control", "treatment"),
           day = 0:n_days) %>%
  group_by(arm) %>%
  mutate(arm_id = if_else(arm == "control", 1, 2),
         bdw_ccdf = beta_discrete_weibull_ccdf(day, alpha_bdw[arm_id], beta_bdw[arm_id], kappa[arm_id]),
         bg_ccdf = beta_geometric_ccdf(day, alpha_bg[arm_id], beta_bg[arm_id]),
         bdw_r = bdw_ccdf/lag(bdw_ccdf, default = 1),
         bg_r = bg_ccdf/lag(bg_ccdf, default = 1),
         theta = expit(phi[arm_id] + eta[arm_id] * day),
         retain = theta * bdw_r + (1 - theta) * bg_r,
         survival = cumprod(retain),
         hazard = 1 - lead(retain),
         pdf = survival * hazard,
         prob = if_else(is.na(pdf), survival, pdf)) %>%
  ungroup() %>% 
  select(arm_id, 
         lifetime = day, 
         prob) 

# simulate daily arm assignments for players (equal prob each arm)
set.seed(987)
assignments <- 
  tibble(day = 1:n_days) %>%
  bind_cols(players = rpois(nrow(.), daily_players)) %>%
  uncount(players) %>%
  bind_cols(arm_id = sample(1:max(probs$arm_id), nrow(.), replace = TRUE))

# simulate player lifetime based on arm assignment
set.seed(654)
for (a in 1:2) {
  
  arm_lifetime_probs <-
    probs %>%
    filter(arm_id == a) %>%
    arrange(lifetime) %>%
    pull(prob)
  
  assignments <- 
    assignments %>%
    filter(arm_id == a) %>%
    select(day, arm_id) %>%
    bind_cols(lifetime = sample(0:n_days,
                                size = nrow(.),
                                replace = TRUE,
                                prob = arm_lifetime_probs)) %>%
    bind_rows(assignments %>%
                filter(arm_id != a))
  
  rm(arm_lifetime_probs)
  
}

# summarise experimental data
experiment <- 
  assignments %>%
  count(day, arm_id, lifetime) %>%
  mutate(final_day = if_else(day + lifetime >= n_days, 
                             n_days, 
                             day + lifetime),
         censored = if_else(final_day == n_days, 1, 0),
         observed = if_else(final_day == n_days, final_day - day, lifetime)) %>%
  group_by(arm_id, observed, censored) %>%
  summarise(n = sum(n), 
            .groups = "drop")

# save output
experiment %>%
  write_csv("posts/2024-03-18-zelda/data/experiment.csv")

