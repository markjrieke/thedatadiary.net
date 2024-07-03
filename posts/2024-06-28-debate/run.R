# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(riekelib)
library(cmdstanr)

run_date <- mdy("6/28/24")
int_run_date <- run_date

# util functions ---------------------------------------------------------------

set_omega <- function(run_date,
                      e_day_omega = 600,
                      s_day_omega = 100) {
  
  # find current day as integer
  D <- as.integer(mdy("11/5/24") - mdy("5/1/24")) + 1
  d <- as.integer(run_date - mdy("5/1/24")) + 1
  
  # estimate linear transform between s_day and e_day omegas on the outcome scale
  inputs <- 0.5 * c(s_day_omega, e_day_omega)
  y <- qbeta(0.975, inputs, inputs)
  x <- c(1, D)
  m <- (y[2] - y[1])/(x[2] - x[1])
  b <- y[1] - m * x[1]
  
  # linear transform output for current day
  q <- logit(m * d + b)
  
  # internal function for finding omega that generates q
  find_omega <- function(omega, q, p) {
    qnorm(p, 0, omega) - q
  }
  
  # optimize
  omega <- uniroot(find_omega, interval = c(0, 1), q = q, p = 0.975)$root
  
  return(omega)
  
}

fetch_cpi <- function() {
  
  current_date <- Sys.Date()
  last_month <- floor_date(floor_date(current_date, "month") - 1, "month")
  
  out <-
    read_csv(
      paste0(
        "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1317&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=",
        last_month,
        "&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
        current_date,
        "&revision_date=",
        current_date,
        "&nd=1947-01-01"
      )
    )
  
  return(out)
  
}

fetch_gdp <- function() {
  
  current_date <- Sys.Date()
  last_quarter <- mdy("4/1/24")
  
  out <-
    read_csv(
      paste0(
        "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1317&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=",
        last_quarter,
        "&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
        current_date,
        "&revision_date=",
        current_date,
        "&nd=1947-01-01"
      )
    )
  
  return(out)
  
}

# prior model - biden ----------------------------------------------------------

# import economic data
gdp <- fetch_gdp()
cpi <- fetch_cpi()

# estimate inflation relative to most recent cpi release
max_cpi <-
  cpi %>%
  filter(DATE == max(DATE)) %>%
  pull(CPIAUCSL)

cpi <-
  cpi %>%
  rename_with(str_to_lower) %>%
  rename(cpi = cpiaucsl) %>%
  mutate(inflation = max_cpi/cpi)

# adjust gdp for inflation
real_gdp <-
  gdp %>%
  rename_with(str_to_lower) %>%
  left_join(cpi) %>%
  select(-cpi) %>%
  mutate(real_gdp = gdp * inflation) %>%
  select(date, real_gdp)

# append national data with 2nd quarter real annualized gdp growth
prior_model_data <-
  read_csv("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/data/static/abramowitz.csv") %>%
  mutate(begin_quarter = mdy(paste0("4/1/", year - 1)),
         end_quarter = mdy(paste0("4/1/", year))) %>%
  left_join(real_gdp, by = c("begin_quarter" = "date")) %>%
  rename(begin_gdp = real_gdp) %>%
  left_join(real_gdp, by = c("end_quarter" = "date")) %>%
  rename(end_gdp = real_gdp) %>%
  mutate(real_gdp_growth = end_gdp/begin_gdp - 1) %>%
  select(-ends_with("quarter"),
         -ends_with("gdp"))

# import historical election day approval estimates
e_day_approval_historical <-
  read_csv("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/out/approval-prior/e_day_approval_historical.csv")

# prep for passing to stan
prior_model_data <-
  prior_model_data %>%
  left_join(e_day_approval_historical) %>%
  transmute(V = if_else(inc_party == "dem", dem, rep),
            V = V/(dem + rep),
            G = real_gdp_growth,
            I = inc_running,
            A_mu,
            A_sigma)

# import current approval forecast
e_day_approval_current <-
  read_csv("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/out/approval/e_day_approval_current.csv") %>%
  filter(run_date == max(run_date))

# get most recent gdp growth
G_new <-
  real_gdp %>%
  filter(date == max(date)) %>%
  mutate(begin_quarter = date - years(1)) %>%
  rename(end_quarter = date,
         end_gdp = real_gdp) %>%
  left_join(real_gdp, by = c("begin_quarter" = "date")) %>%
  rename(begin_gdp = real_gdp) %>%
  mutate(G_new = end_gdp/begin_gdp - 1) %>%
  pull(G_new)

# import pvi prior
pvi_summary <-
  read_csv("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/out/pvi/pvi_summary.csv")

# compile model to exe
prior_model <-
  cmdstan_model(
    "posts/2024-06-28-debate/priors.stan"
  )

# pass to stan --- biden
stan_data <-
  list(
    N = nrow(prior_model_data),
    V = prior_model_data$V,
    G = prior_model_data$G,
    I = prior_model_data$I,
    A_mu = prior_model_data$A_mu,
    A_sigma = prior_model_data$A_sigma,
    alpha_mu = 0,
    alpha_sigma = 1,
    beta_a_mu = 0,
    beta_a_sigma = 1,
    beta_g_mu = 0,
    beta_g_sigma = 1,
    beta_i_mu = 0,
    beta_i_sigma = 1,
    sigma_sigma = 1,
    A_new_mu = e_day_approval_current$mean,
    A_new_sigma = e_day_approval_current$sd,
    G_new = G_new,
    I_new = 1,
    S = nrow(pvi_summary),
    e_day_mu = pvi_summary$pvi_mu,
    e_day_sigma = pvi_summary$pvi_sd
  )

# fit !
prior_fit <-
  prior_model$sample(
    data = stan_data,
    seed = 2024,
    iter_warmup = 1250,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8
  )

# extract state priors on the logit scale
priors <-
  prior_fit$draws("theta_state", format = "df") %>%
  as_tibble() %>%
  pivot_longer(starts_with("theta_state"),
               names_to = "parameter",
               values_to = "estimate") %>%
  mutate(estimate = logit(estimate)) %>%
  drop_na() %>%
  filter(estimate < 100) %>%
  group_by(parameter) %>%
  summarise(e_day_mu = mean(estimate),
            e_day_sigma = sd(estimate)) %>%
  ungroup() %>%
  mutate(parameter = parse_number(parameter)) %>%
  arrange(parameter) %>%
  bind_cols(pvi_summary) %>%
  select(state,
         e_day_mu,
         e_day_sigma)

# internal renaming
priors_biden <- priors

# prior model - generic candidate ----------------------------------------------

# update to a non-incumbent
stan_data$I_new <- 0

# fit !
prior_fit <-
  prior_model$sample(
    data = stan_data,
    seed = 2024,
    iter_warmup = 1250,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8
  )

# extract state priors on the logit scale
priors <-
  prior_fit$draws("theta_state", format = "df") %>%
  as_tibble() %>%
  pivot_longer(starts_with("theta_state"),
               names_to = "parameter",
               values_to = "estimate") %>%
  mutate(estimate = logit(estimate)) %>%
  drop_na() %>%
  filter(estimate < 100) %>%
  group_by(parameter) %>%
  summarise(e_day_mu = mean(estimate),
            e_day_sigma = sd(estimate)) %>%
  ungroup() %>%
  mutate(parameter = parse_number(parameter)) %>%
  arrange(parameter) %>%
  bind_cols(pvi_summary) %>%
  select(state,
         e_day_mu,
         e_day_sigma)

# internal renaming
priors_generic <- priors

# poll model -------------------------------------------------------------------

# number of days in election cycle
D <- as.integer(mdy("11/5/2024") - mdy("5/1/2024") + 1)

# read in static data
banned_pollsters <- read_csv("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/data/static/banned_pollsters.csv")
allowed_candidates <- read_csv("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/data/static/allowed_candidates.csv")
population_rank <- read_csv("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/data/static/population_rank.csv")

# read in pre-computed feature data
sid <- read_csv("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/data/features/sid.csv")
F_r <- read_rds("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/data/features/F_r.rds")
F_s <- read_rds("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/data/features/F_s.rds")
wt <- read_rds("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/data/features/wt.rds")

# read in wrangled polls
polls <- read_csv("https://raw.githubusercontent.com/markjrieke/2024-potus/dev/out/polls/polls_out.csv")

# filter out to day after debate
polls <- 
  polls %>%
  filter(end_date <= mdy("6/28/24"))

# modify priors
priors_biden <- 
  priors_biden %>%
  mutate(state = str_replace(state, "[ ]+(?=[0-9])", " CD-")) %>%
  filter(run_date == int_run_date,
         !state %in% c("National", "Nebraska", "Maine"))

priors_generic <- 
  priors_generic %>%
  mutate(state = str_replace(state, "[ ]+(?=[0-9])", " CD-")) %>%
  filter(run_date == int_run_date,
         !state %in% c("National", "Nebraska", "Maine"))

# enforce prior ordering
priors_biden <- 
  priors_biden %>%
  left_join(sid %>% select(sid, state)) %>%
  arrange(sid) %>%
  select(-c(sid))

priors_generic <- 
  priors_generic %>%
  left_join(sid %>% select(sid, state)) %>%
  arrange(sid) %>%
  select(-c(sid))

# update generic to include aggregates
priors_generic <- 
  priors_generic %>%
  select(-e_day_sigma) %>%
  bind_rows(tibble(state = c("National", "Nebraska", "Maine"),
                   e_day_mu = (wt %*% priors_generic$e_day_mu)[,1]))

# set omega
omega <- set_omega(run_date)

# compile model to exe
poll_model <-
  cmdstan_model(
    "posts/2024-06-28-debate/polls.stan",
    force_recompile = TRUE
  )

# pass to stan
stan_data <-
  list(
    N = nrow(polls),
    D = D,
    R = nrow(F_r),
    A = nrow(wt),
    S = max(sid$sid),
    G = max(polls$gid),
    M = max(polls$mid),
    C = max(polls$cid),
    P = max(polls$pid),
    did = polls$did,
    sid = polls$sid,
    gid = polls$gid,
    mid = polls$mid,
    cid = polls$cid,
    pid = polls$pid,
    g_ref = polls %>% filter(group == "lv") %>% distinct(gid) %>% pull(gid),
    c_ref = polls %>% filter(candidate_sponsored == "None") %>% distinct(cid) %>% pull(cid),
    F_r = F_r,
    wt = wt,
    K = polls$K,
    Y = polls$Y,
    beta_g_sigma = 0.02,
    beta_c_sigma = 0.015,
    sigma_n_sigma = 0.02,
    sigma_p_sigma = 0.05,
    sigma_m_sigma = 0.02,
    alpha_mu_r = priors_biden$e_day_mu,
    alpha_sigma_r = priors_biden$e_day_sigma,
    rho_alpha = 3,
    rho_beta = 6,
    alpha_sigma = 0.05,
    phi_sigma = 0.05,
    psi_sigma = 0.05,
    omega = omega,
    electors = sid$electors,
    F_s = F_s,
    alpha_mu_g = priors_generic$e_day_mu,
    prior_check = 0
  )

# fit !
poll_fit <-
  poll_model$sample(
    data = stan_data,
    seed = 2024,
    iter_warmup = 1250,
    iter_sampling = 1250,
    chains = 8,
    parallel_chains = 8
  )

# explore posterior ------------------------------------------------------------

# generic - biden
delta_summary <- 
  poll_fit$summary(c("delta_theta", "delta_evs", "delta_win_state", "delta_win_pres", "delta_tie_pres"))

delta_summary %>%
  write_csv("posts/2024-06-28-debate/delta_summary.csv")

# raw probability of winning
raw_prob <-
  poll_fit$summary(c("win_pres", "win_pres_g", "win_state", "win_state_g"))

raw_prob %>%
  write_csv("posts/2024-06-28-debate/raw_prob.csv")

raw_prob %>%
  filter(str_detect(variable, "win_state")) %>%
  mutate(sid = parse_number(variable),
         candidate = if_else(str_detect(variable, "_g"), "generic", "biden")) %>%
  select(candidate, 
         sid, 
         p_win = mean) %>%
  left_join(sid) %>%
  filter(state %in% c("Arizona", 
                      "Nevada", 
                      "Georgia",
                      "New Mexico",
                      "North Carolina",
                      "Colorado",
                      "Wisconsin", 
                      "Minnesota",
                      "Pennsylvania", 
                      "Michigan",
                      "Virginia",
                      "New Hampshire",
                      "Maine")) %>%
  select(state, p_win, candidate) %>%
  pivot_wider(names_from = candidate,
              values_from = p_win) %>%
  mutate(state = fct_reorder(state, biden)) %>%
  ggplot(aes(x = state)) + 
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "gray40") + 
  geom_segment(aes(y = biden,
                   yend = generic),
               linewidth = 2.5,
               color = "gray80",
               alpha = 0.75) +
  geom_point(aes(y = biden),
             fill = "royalblue",
             color = "white",
             shape = 21,
             size = 5) + 
  geom_point(aes(y = generic),
             fill = "orange",
             color = "white",
             shape = 21,
             size = 5) + 
  scale_y_percent() + 
  coord_flip() + 
  theme_rieke() +
  labs(title = "**Crumbling in competitive states**",
       subtitle = glue::glue("**{color_text('Biden', 'royalblue')}** has a better chance of winning ",
                             "in competitive states than a **{color_text('Generic Democrat', 'orange')}**"),
       y = "Probability of a Democratic win",
       x = NULL) +
  expand_limits(y = c(0, 1)) 

ggquicksave("posts/2024-06-28-debate/probs.png")

# raw electoral votes won
evs_draws <-
  poll_fit$draws(c("evs", "evs_g"), format = "df")

evs_draws %>%
  as_tibble() %>% 
  select(starts_with("evs")) %>%
  pivot_longer(everything(), 
               names_to = "candidate",
               values_to = "evs") %>%
  mutate(candidate = if_else(str_detect(candidate, "_"), "Generic Democrat", "Biden")) %>%
  ggplot(aes(x = evs,
             y = candidate,
             fill = candidate)) + 
  ggdist::stat_histinterval(.width = c(0.66, 0.95),
                            alpha = 0.75,
                            breaks = seq(from = 0, to = 540, by = 7)) +
  geom_vline(xintercept = 269,
             linetype = "dashed",
             color = "gray40") + 
  scale_fill_manual(values = c("royalblue", "orange")) + 
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = "**Biden is the better bet**",
       subtitle = glue::glue("**{color_text('Biden', 'royalblue')}** is more likely ",
                             "to win the electoral college than a ", 
                             "**{color_text('Generic Democrat', 'orange')}**<br>",
                             "alternative would be, should he be replaced"),
       x = "Forecasted Electoral Votes Won",
       y = NULL,
       caption = paste("Pointrange indicates 66/95% posterior credible",
                       "interval based on 10,000 MCMC samples",
                       sep = "<br>")) +
  expand_limits(x = c(0, 538))

ggquicksave("posts/2024-06-28-debate/electoral_college.png")


