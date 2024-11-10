# libraries
library(tidyverse)
library(ggdist)
library(ggblend)
library(riekelib)

# true underlying population/group characteristics
groups <-
  tibble(group = LETTERS[1:2],
         population = c(0.5, 0.5),
         p_respond = c(0.1, 0.02)) %>%
  mutate(p_sampled = population * p_respond,
         p_sampled = p_sampled/sum(p_sampled))

# simulation parameters
n_sims <- 5000
sample_size <- 700

# plot! ------------------------------------------------------------------------

tmp <- 
  crossing(sim = 1:n_sims,
         beta = seq(from = 0.03, to = 0.97, by = 0.01),
         groups) %>%
  mutate(p_support = if_else(group == "A", beta, 1 - beta)) %>%
  nest(data = -c(sim, beta)) %>%
  mutate(K = map(data, ~rmultinom(1, sample_size, .$p_sampled)[,1])) %>%
  unnest(c(data, K)) %>%
  bind_cols(Y = rbinom(nrow(.), .$K, .$p_support)) %>%
  group_by(sim, beta) %>%
  mutate(n_total = sum(K),
         observed = K/n_total,
         weight = population/observed) %>%
  summarise(weighted = sum(Y * weight)/sum(K * weight),
            unweighted = sum(Y)/sum(K)) %>%
  ungroup() %>%
  pivot_longer(ends_with("weighted"),
               names_to = "method",
               values_to = "p") %>%
  group_by(beta, method) %>%
  summarise(mean = mean(p),
            sd = sd(p)) %>%
  ungroup()
  
tmp %>%
  ggplot(aes(x = beta,
             y = sd,
             color = method)) + 
  geom_point() +
  scale_color_brewer(palette = "Set2") +
  theme_rieke() +
  theme(legend.position = "none") + 
  labs(title = "**Effect of weighting on SE of the mean**",
       subtitle = glue::glue("In binary outcomes, **",
                             color_text("weighting", RColorBrewer::brewer.pal(6, "Set2")[2]),
                             "** reduces the SE relative to the **",
                             color_text("unweighted", RColorBrewer::brewer.pal(6, "Set2")[1]),
                             "** mean"),
       x = "Subgroup A probability of support",
       y = "SE",
       caption = paste("SE estimated from 5,000 simulated surveys of 700 respondents",
                       "for each level of support among subgroup A",
                       sep = "<br>"))



