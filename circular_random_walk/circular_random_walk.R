library(tidyverse)
library(scales)
theme_set(theme_light())

sim <- crossing(trial = 1:10000,
                step = 1:1000) %>%
  mutate(direction = sample(c(1, -1), n(), replace = TRUE)) %>%
  group_by(trial) %>%
  mutate(seat = cumsum(direction) %% 20) %>%
  ungroup() %>%
  distinct(trial, seat, .keep_all = TRUE) %>%
  filter(seat != 0)

by_seat <- sim %>%
  group_by(trial) %>%
  mutate(is_last = row_number() == 19) %>%
  group_by(seat) %>%
  summarize(avg_step = mean(step),
            pct_last = mean(is_last))
  
by_seat %>% 
  ggplot(aes(seat, avg_step)) +
  geom_line() +
  expand_limits(y = 0)

by_seat %>% 
  ggplot(aes(seat, pct_last)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 1 / 19, lty = 2, color = "red") +
  expand_limits(y = 0)

sim %>% 
  ggplot(aes(step)) +
  geom_histogram() +
  facet_wrap(~seat)