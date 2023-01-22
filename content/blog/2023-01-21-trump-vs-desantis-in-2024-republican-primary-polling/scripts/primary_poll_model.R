# setup ------------------------------------------------------------------------

# libraries
library(tidyverse)
library(riekelib)
library(tidybayes)
library(tidytext)

# read in polls from fte
polls <- 
  read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_primary_polls.csv")

# get to polls used in fte's graphics ------------------------------------------

# prep polls
polls <- 
  polls %>%
  
  # reframe display name to match fte graphics
  mutate(pollster_sponsor = if_else(is.na(sponsors), 
                                    pollster, 
                                    paste(pollster, sponsors, sep = "/"))) %>%
  select(question_id, 
         pollster_sponsor,
         ends_with("date"),
         sample_size,
         race_id,
         cycle,
         candidate_name,
         pct,
         -sponsor_candidate,
         -election_date) %>%
  
  # group poll name/date together for easier filtering
  mutate(across(ends_with("date"), lubridate::mdy),
         month = lubridate::month(start_date, label = TRUE),
         start_day = lubridate::day(start_date),
         end_day = lubridate::day(end_date),
         poll = glue::glue("{pollster_sponsor} {month}. {start_day}-{end_day}")) %>%
  select(-pollster_sponsor, 
         -ends_with("date"), 
         -month,
         -ends_with("day"))

polls_h2h <-
  polls %>%
  
  # get to just polls in fte's first graphic
  filter(poll %in% c("YouGov Nov. 9-11",
                     "Léger/The Association for Canadian Studies Nov. 11-13",
                     "Quinnipiac Nov. 16-20",
                     "Marquette Law School Nov. 15-22",
                     "Premise Nov. 19-20",
                     "Fabrizio/Impact/Wall Street Journal Dec. 3-7",
                     "Suffolk/USA Today Dec. 7-11",
                     "McLaughlin Dec. 9-14",
                     "Morning Consult Dec. 10-14",
                     "Echelon Insights Dec. 12-14",
                     "Harris Poll/Harvard CAPS Dec. 14-15",
                     "YouGov/Yahoo News Dec. 15-19",
                     "YouGov/Economist Dec. 17-20")) %>%
  
  # get to just h2h questions reported in fte's graphic
  filter(!question_id %in% c(165907,
                             165696,
                             166262,
                             166424,
                             166427,
                             166428,
                             166429,
                             166616,
                             166501,
                             166502,
                             166504,
                             166505,
                             166506,
                             166466,
                             166467,
                             166470,
                             166471,
                             166472,
                             166667))

polls_multi <- 
  polls %>%
  
  # filter to multiway polls evaluated by fte
  filter(poll %in% c("Zogby Nov. 9-11",
                     "Seven Letter Insight Nov. 10-15",
                     "Ipsos/FiveThirtyEight Nov. 9-21",
                     "Emerson College Polling Society Nov. 18-19",
                     "Morning Consult/Politico Nov. 18-20",
                     "YouGov/Economist Nov. 26-29",
                     "Marist/NPR | PBS NewsHour Dec. 6-8",
                     "Monmouth U. Dec. 8-12",
                     "McLaughlin Dec. 9-14",
                     "Cygnal Political Dec. 12-14",
                     "Echelon Insights Dec. 12-14",
                     "Harris Poll/Harvard CAPS Dec. 14-15",
                     "YouGov/Yahoo News Dec. 15-19",
                     "Big Village Dec. 16-18",
                     "Morning Consult Dec. 31-2")) %>% 
  
  # remove non-multiway questions from those polls
  filter(!question_id %in% c(165786,
                             165787,
                             165788,
                             166185,
                             165606,
                             166383,
                             166425,
                             166427,
                             166428,
                             166429,
                             166508,
                             166502,
                             166503,
                             166504,
                             166505,
                             166506,
                             166467,
                             166468,
                             166470,
                             166471,
                             166472,
                             166668,
                             166682,
                             166683,
                             166684))

# prep for stan ----------------------------------------------------------------

matrix_h2h <- 
  polls_h2h %>%
  
  # fill in missing sample sizes
  mutate(sample_size = case_when(question_id == 165697 ~ round(1408 * 0.28),
                                 question_id == 165400 ~ round(1500 * 0.28),
                                 TRUE ~ sample_size)) %>%
  
  # get # of votes by candidate in wide format
  mutate(votes = round(sample_size * pct/100),
         candidate_name = if_else(str_detect(candidate_name, "Trump"), 
                                  "trump",
                                  "desantis")) %>% 
  select(-pct) %>%
  pivot_wider(names_from = candidate_name,
              values_from = votes) %>%
  
  # turn to matrix
  select(trump, desantis) %>%
  as.matrix()

matrix_multi <- 
  polls_multi %>%
  
  # group other candidates
  mutate(candidate_name = case_when(str_detect(candidate_name, "Trump") ~ "trump",
                                    str_detect(candidate_name, "DeSantis") ~ "desantis",
                                    TRUE ~ "other")) %>%
  group_by(question_id, candidate_name) %>%
  summarise(sample_size = max(sample_size),
            pct = sum(pct)) %>%
  ungroup() %>%
  
  # get candidate votes in wide format
  mutate(votes = round(sample_size * pct/100)) %>%
  select(-pct) %>%
  pivot_wider(names_from = candidate_name,
              values_from = votes) %>%
  
  # turn to matrix
  select(trump, desantis, other) %>%
  as.matrix()

# add to list for stan
poll_data <-
  list(
    # number of polls
    N_h2h = nrow(matrix_h2h),
    N_multi = nrow(matrix_multi),
    
    # number of categories
    K_h2h = ncol(matrix_h2h),
    K_multi = ncol(matrix_multi),
    
    # response counts
    R_h2h = matrix_h2h,
    R_multi = matrix_multi
  )

# model & output ---------------------------------------------------------------

republican_primary <-
  rstan::stan(
    file = "content/blog/2023-01-21-trump-vs-desantis-in-2024-republican-primary-polling/scripts/primary_poll_model.stan",
    data = poll_data,
    chains = 4, 
    iter = 2000
  )

# get names reformatted into long format
republican_posterior <- 
  republican_primary %>%
  posterior::as_draws_df() %>%
  as_tibble() %>%
  select(starts_with("p")) %>%
  rename_with(~str_replace(.x, "\\[1\\]", "_trump")) %>%
  rename_with(~str_replace(.x, "\\[2\\]", "_desantis")) %>%
  rename_with(~str_replace(.x, "\\[3\\]", "_other")) %>%
  rowid_to_column(".draw") %>%
  pivot_longer(starts_with("p"),
               names_to = "parameter",
               values_to = "estimate")

# figures ----------------------------------------------------------------------

pal <- RColorBrewer::brewer.pal(3, "Dark2")
pal_d <- pal[1]
pal_t <- pal[2]
pal_o <- pal[3]

tibble(label = c(glue::glue("**{color_text('Trump', pal_t)}** + ",
                            "**{color_text('DeSantis', pal_d)}** + ",
                            "**{color_text('Other', pal_o)}** = 100%"),
                 glue::glue("**{color_text('Trump', pal_t)}** + ",
                            "**{color_text('DeSantis', pal_d)}** = 100%")),
       x = c(0, 0),
       y = c(1, 0)) %>%
  ggplot(aes(x = x,
             y = y,
             label = label)) + 
  ggtext::geom_richtext(family = "Roboto Slab",
                        size = 12,
                        label.color = "white") +
  expand_limits(y = c(-0.5, 1.5)) +
  geom_segment(x = 0.017,
               y = 0.87,
               xend = -0.02,
               yend = 0.12,
               linewidth = 1.25,
               color = pal_t,
               arrow = arrow(type = "closed",
                             length = unit(0.125, "inches"))) +
  geom_segment(x = 0.017,
               y = 0.87,
               xend = 0.005,
               yend = 0.12,
               linewidth = 1.25,
               color = pal_d,
               arrow = arrow(type = "closed",
                             length = unit(0.125, "inches"))) +
  theme_void()

ggquicksave("content/blog/2023-01-21-trump-vs-desantis-in-2024-republican-primary-polling/img/fig_01.png")

# results plot
republican_posterior %>%
  filter(!str_detect(parameter, "switch")) %>%
  group_by(parameter) %>%
  median_qi(estimate, .width = 0.5) %>%
  select(parameter, estimate) %>%
  mutate(parameter = str_remove(parameter, "p_")) %>%
  separate(parameter, c("poll_type", "candidate")) %>%
  mutate(poll_type = if_else(poll_type == "h2h", "Head-to-Head", "Multiway"),
         candidate = str_to_title(candidate),
         candidate = if_else(candidate == "Desantis", "DeSantis", candidate),
         fill_col = case_when(candidate == "Trump" ~ pal_t,
                              candidate == "DeSantis" ~ pal_d,
                              candidate == "Other" ~ pal_o),
         poll_type = fct_relevel(poll_type, "Multiway"),
         width = if_else(poll_type == "Multiway", 0.5, 1/3)) %>%
  ggplot(aes(x = reorder_within(candidate, estimate, poll_type),
             y = estimate,
             fill = fill_col,
             width = width)) + 
  geom_col(alpha = 0.75) + 
  facet_wrap(~poll_type, scales = "free_y") + 
  scale_y_continuous(labels = scales::label_percent()) + 
  scale_x_reordered() +
  scale_fill_identity() + 
  coord_flip() +
  theme_rieke() +
  theme(legend.position = "none") +
  labs(title = glue::glue("Battle for the Ballot: ",
                          "**{color_text('Trump', pal_t)}** vs. ",
                          "**{color_text('DeSantis', pal_d)}**"),
       subtitle = "Trump wins a crowded primary, but loses head-to-head against DeSantis",
       x = NULL,
       y = NULL,
       caption = "Median posterior estimated support<br>in multiway and head-to-head polls")

ggquicksave("content/blog/2023-01-21-trump-vs-desantis-in-2024-republican-primary-polling/img/fig_02.png")

# margin plot
pal_multi <- NatParksPalettes::NatParksPalettes$Yellowstone[[1]][6]
pal_h2h <- NatParksPalettes::NatParksPalettes$Yellowstone[[1]][1]

republican_posterior %>%
  filter(str_detect(parameter, "trump|desantis"),
         !str_detect(parameter, "switch")) %>%
  mutate(parameter = str_remove(parameter, "p_")) %>%
  separate(parameter, c("poll_type", "candidate")) %>%
  pivot_wider(names_from = candidate,
              values_from = estimate) %>%
  mutate(trump_margin = trump - desantis,
         fill = if_else(poll_type == "h2h", pal_h2h, pal_multi)) %>% 
  ggplot(aes(x = trump_margin,
             fill = fill)) + 
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray40",
             alpha = 0.5) + 
  geom_histogram(binwidth = 0.0025,
                 color = "white",
                 alpha = 0.75) +
  geom_text(x = -0.0025,
            y = 500,
            label = "DeSantis beats Trump",
            family = "Roboto Slab",
            hjust = "right",
            color = "gray60") + 
  geom_text(x = 0.0025,
            y = 500,
            label = "Trump beats DeSantis",
            family = "Roboto Slab",
            hjust = "left",
            color = "gray60") +
  scale_fill_identity() + 
  scale_x_continuous(labels = scales::label_percent()) + 
  theme_rieke() +
  theme(axis.text.y = element_blank(),
        legend.position = "none") +
  labs(title = glue::glue("Of Monsters, Matchups, and Margins"),
       subtitle = glue::glue("Trump's margin over DeSantis in ",
                             "**{color_text('multiway', pal_multi)}** and ",
                             "**{color_text('head-to-head', pal_h2h)}** polls"),
       x = NULL,
       y = NULL,
       caption = "Posterior estimation of voteshare margin<br>in multiway and head-to-head polls")

ggquicksave("content/blog/2023-01-21-trump-vs-desantis-in-2024-republican-primary-polling/img/fig_03.png")

# switching plot
republican_posterior %>%
  filter(str_detect(parameter, "switch")) %>%
  mutate(fill = if_else(str_detect(parameter, "trump"), pal_t, pal_d),
         parameter = if_else(str_detect(parameter, "trump"), "Other to\nTrump", "Other to\nDeSantis")) %>%
  ggplot(aes(x = parameter,
             y = estimate,
             fill = fill)) + 
  stat_histinterval(alpha = 0.75,
                    breaks = seq(from = 0, to = 1, by = 0.0125),
                    slab_color = "white",
                    slab_size = 0.5,
                    outline_bars = TRUE) +
  coord_flip() + 
  scale_fill_identity() +
  scale_y_continuous(labels = scales::label_percent()) + 
  theme_rieke() +
  labs(title = "Oh, the places (the others) will go!",
       subtitle = glue::glue("Most ",
                             "**{color_text('Other', pal_o)}** voters would prefer ",
                             "**{color_text('DeSantis', pal_d)}** over ",
                             "**{color_text('Trump', pal_t)}**"),
       x = NULL,
       y = NULL,
       caption = "Posterior estimation of choosing Trump or DeSantis<br>when the preferred alternative isn't available")

ggquicksave("content/blog/2023-01-21-trump-vs-desantis-in-2024-republican-primary-polling/img/fig_04.png")

