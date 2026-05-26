library(tidyverse)
library(cmdstanr)
library(ggdist)
library(riekelib)
library(patchwork)

# Setup a color palette to be used for groups A/B
pal <- NatParksPalettes::NatParksPalettes$Acadia[[1]][c(2,8)]

set.seed(2026)
mvn_data <-
  tibble(group = c("A", "B"),
         mu = list(c(0, 2), c(-2, 1)),
         sigma = list(c(0.25, 1), c(0.5, 0.75)),
         rho = c(0.8, -0.4)) %>%
  mutate(Sigma = pmap(list(sigma, rho),
                      ~matrix(c(..1[1]^2, prod(..1) * ..2, prod(..1) * ..2, ..1[2]^2),
                              nrow = 2)),
         y = pmap(list(mu, Sigma), ~MASS::mvrnorm(5000, ..1, ..2))) 

mvn_data %>%
  mutate(y = map(y, ~as_tibble(.x, .name_repair = "universal"))) %>%
  unnest(y) %>%
  rename(y1 = `...1`,
         y2 = `...2`) %>%
  ggplot(aes(x = y1,
             y = y2,
             color = group)) + 
  geom_point(alpha = 0.25,
             shape = 21) +
  scale_color_manual(values = pal) +
  theme_rieke()

individual_model <- cmdstan_model("posts/2026-05-25-sufficient-mvn/individual-mvn.stan")

model_data <- 
  mvn_data %>%
  mutate(gid = rank(group),
         y = map(y, ~as_tibble(.x, .name_repair = "universal"))) %>%
  unnest(y) %>%
  rename(y1 = `...1`,
         y2 = `...2`)

stan_data <-
  list(
    N = nrow(model_data),
    K = length(mvn_data$mu[[1]]),
    Y = cbind(model_data$y1, model_data$y2),
    G = max(model_data$gid),
    gid = model_data$gid,
    mu_mu = 0,
    sigma_mu = 5,
    lambda_sigma = 1,
    lkj_prior = 2
  )

start_time <- as.numeric(Sys.time())
individual_fit <-
  individual_model$sample(
    data = stan_data,
    seed = 1234,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 10,
    parallel_chains = 10
  )
run_time_individual <- as.numeric(Sys.time()) - start_time

plot_posterior <- function(fit) {
  
  truth <- 
    mvn_data %>%
    select(group, mu, sigma, rho) %>%
    mutate(rho = map(rho, ~.x)) %>%
    pivot_longer(c(mu, sigma, rho),
                 names_to = "parameter",
                 values_to = "draw") %>%
    mutate(parameter = case_when(parameter == "mu" ~ "\u03bc",
                                 parameter == "sigma" ~ "\u03c3",
                                 parameter == "rho" ~ "\u03c1"),
           parameter_group = parameter,
           kid = map(draw, ~1:length(.x))) %>%
    unnest(c(draw, kid)) %>%
    mutate(parameter = if_else(parameter != "\u03c1",
                               paste0(parameter, "<sub>", kid, "</sub>"),
                               parameter))
  
  fit$draws(c("mu", "sigma", "rho"), format = "df") %>%
    as_tibble() %>%
    select(starts_with("mu"), starts_with("sigma"), starts_with("rho")) %>%
    pivot_longer(everything(),
                 names_to = "parameter",
                 values_to = "draw") %>%
    filter(str_detect(parameter, "mu|sigma|,1,2]")) %>%
    nest(data = -parameter) %>%
    mutate(parameter = if_else(str_detect(parameter, "rho"),
                               str_replace(parameter, ",1,2", ""),
                               parameter),
           group = if_else(str_detect(parameter, "\\[1"), "A", "B"),
           kid = if_else(str_detect(parameter, ",1"), 1, 2),
           kid = if_else(str_detect(parameter, "rho"), NA, kid),
           parameter = str_extract(parameter, "mu|sigma|rho"),
           parameter = case_when(parameter == "mu" ~ "\u03bc",
                                 parameter == "sigma" ~ "\u03c3",
                                 parameter == "rho" ~ "\u03c1"),
           parameter_group = parameter,
           parameter = if_else(parameter != "\u03c1",
                               paste0(parameter, "<sub>", kid, "</sub>"),
                               parameter)) %>%
    unnest(data) %>%
    ggplot(aes(x = draw,
               y = parameter,
               fill = group)) +
    stat_histinterval(slab_alpha = 0.75) +
    geom_point(data = truth,
               color = "red") +
    scale_fill_manual(values = pal) +
    facet_wrap(~parameter_group, scales = "free", ncol = 1) +
    theme_rieke() +
    theme(axis.text.y.left = ggtext::element_markdown(color = "#363a3c"))
  
}

plot_posterior(individual_fit) +
  labs(title = "**Fit against individual observations**",
       subtitle = "Can be used as a baseline for evaluating the sufficient formulation",
       x = NULL,
       y = NULL)

sufficient_model <- cmdstan_model("posts/2026-05-25-sufficient-mvn/sufficient-mvn.stan")

xxt <- function(x) {
  return(x %*% t(x))
}

yyts <- function(y) {
  out <- matrix(0, nrow = 2, ncol = 2)
  for (i in 1:length(y[,1])) {
    out <- out + xxt(y[i,])
  }
  return(out)
}

yts <- function(y) {
  out <- c(0, 0)
  for (i in 1:length(y[,1])) {
    out <- out + y[i,]
  }
  return(out)
}

model_data <-
  mvn_data %>%
  mutate(yyts = map(y, yyts),
         yts = map(y, yts),
         n_obs = map_dbl(y, nrow),
         gid = rank(group))

stan_data <- 
  list(
    N = nrow(model_data),
    K = length(mvn_data$mu[[1]]),
    G = max(model_data$gid),
    yyts = model_data$yyts,
    yts = model_data$yts,
    n_obs = model_data$n_obs,
    gid = model_data$gid,
    mu_mu = 0,
    sigma_mu = 5,
    lambda_sigma = 1,
    lkj_prior = 2
  )

start_time <- as.numeric(Sys.time())
sufficient_fit <-
  sufficient_model$sample(
    data = stan_data,
    seed = 1234,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 10,
    parallel_chains = 10
  )
run_time_sufficient <- as.numeric(Sys.time()) - start_time

run_time_individual / run_time_sufficient

plot_posterior(sufficient_fit)  +
  labs(title = "**Fit using the sufficient multivariate normal**",
       subtitle = "Recovers the same posterior from the fit to individual observations!",
       x = NULL,
       y = NULL)



