# libraries
library(tidyverse)
library(ggdist)
library(ggblend)
library(riekelib)

# true underlying population/group characteristics
groups <-
  tibble(group = LETTERS[1:2],
         population = c(0.5, 0.5))

# simulation parameters
n_sims <- 5000
sample_size <- 700

# simulate responses -----------------------------------------------------------

sims <- 
  
  # simulate different levels of subgroup correlation with the outcome
  crossing(sim = 1:n_sims,
         beta = seq(from = 0.03, to = 0.97, by = 0.01),
         groups) %>%
  
  # simulate different levels of subgroup correlation with nonresponse
  nest(data = -c(sim, group, population)) %>%
  nest(data = -sim) %>%
  crossing(tibble(case = 1:4,
                  p_respond = list(c(0.05, 0.05), c(0.07, 0.03), c(0.08, 0.02), c(0.09, 0.01)))) %>%
  unnest(c(data, p_respond)) %>%
  unnest(data) %>%
  group_by(sim, beta, case) %>%
  mutate(p_sampled = population * p_respond,
         p_sampled = p_sampled/sum(p_sampled)) %>%
  ungroup() %>%
  
  # simulate survey collection / response
  mutate(p_support = if_else(group == "A", beta, 1 - beta)) %>%
  nest(data = -c(sim, beta, case)) %>%
  mutate(K = map(data, ~rmultinom(1, sample_size, .$p_sampled)[,1])) %>%
  unnest(c(data, K)) %>%
  bind_cols(Y = rbinom(nrow(.), .$K, .$p_support)) %>%
  
  # compute weighted/unweighted results per survey
  group_by(sim, beta, case) %>%
  mutate(n_total = sum(K),
         observed = K/n_total,
         weight = population/observed) %>%
  summarise(weighted = sum(Y * weight)/sum(K * weight),
            unweighted = sum(Y)/sum(K)) %>%
  ungroup() %>%
  
  # summarise mean/sd for each condition of case, p_support, and p_respond
  pivot_longer(ends_with("weighted"),
               names_to = "method",
               values_to = "p") %>%
  group_by(beta, method, case) %>%
  summarise(mean = mean(p),
            sd = sd(p)) %>%
  ungroup()
  
# plot! ------------------------------------------------------------------------

sims %>%
  mutate(case = case_match(case,
                           1 ~ "Response Rates: 5/5%",
                           2 ~ "Response Rates: 7/3%",
                           3 ~ "Response Rates: 8/2%",
                           4 ~ "Response Rates: 9/1%")) %>%
  ggplot(aes(x = beta,
             y = sd,
             color = method)) + 
  geom_point() +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~case, scales = "free_y") + 
  theme_rieke() +
  theme(legend.position = "none") + 
  labs(title = "**Effect of weighting on SE of the mean**",
       subtitle = glue::glue("In binary outcomes, **",
                             color_text("weighting", RColorBrewer::brewer.pal(6, "Set2")[2]),
                             "** can reduce the SE relative to the **",
                             color_text("unweighted", RColorBrewer::brewer.pal(6, "Set2")[1]),
                             "** mean,<br>",
                             "depending on the relative proportion of nonresponse among subgroups"),
       x = "Subgroup A probability of support",
       y = "SE",
       caption = paste("SE estimated from 5,000 simulated surveys of 700 respondents",
                       "for each level of support among subgroup A",
                       sep = "<br>"))



