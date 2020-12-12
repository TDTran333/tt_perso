# TidyTuesday
# Mobile vs. Landline adoption

# Libraries

library(tidyverse)
library(WDI)
library(scales)
library(fuzzyjoin)
library(gganimate)

source('ggplot_theme_Publication-2.R')

theme_set(theme_Publication())

# Load Data
tuesdata <- tidytuesdayR::tt_load('2020-11-10')

glimpse(tuesdata$mobile)
glimpse(tuesdata$landline)

# Combine the two datasets

mobile <- tuesdata$mobile %>%
  rename(subscriptions = mobile_subs) %>%
  mutate(type = "mobile")

landline <- tuesdata$landline %>%
  rename(subscriptions = landline_subs) %>%
  mutate(type = "landline")

phones <- bind_rows(mobile, landline) %>% 
  rename(country = entity)

glimpse(phones)

# Visualize data

phones %>%
  filter(country == "United States") %>%
  ggplot(aes(x = year, y = subscriptions, color = type)) +
  geom_line() +
  labs(x = "Years", 
       y = "Median subscriptions per 100 people",
       title = "Mobiles vs. landlines adoption over time in the United States.") 

avg_country_pop <- phones %>% 
  group_by(country) %>% 
  summarize(avg_population = mean(total_pop, na.rm = TRUE)) %>% 
  arrange(desc(avg_population))

phones %>% 
  semi_join(avg_country_pop %>% slice_max(avg_population, n = 10), by = "country") %>% 
  ggplot(aes(x = year, y = subscriptions, color = type, group = interaction(type, country))) +
  geom_line() +
  facet_wrap(~continent) +
  labs(x = "Years", 
       y = "Median subscriptions per 100 people",
       title = "Mobiles vs. landlines adoption over time for top 10 most populous countries.") 

f_summarize_subs <- . %>% 
  filter(!is.na(subscriptions)) %>% 
  summarize(avg_subs = mean(subscriptions),
            median_subs = median(subscriptions), 
            q25 = quantile(subscriptions, 0.25), 
            q75 = quantile(subscriptions, 0.75))

phones %>% 
  group_by(year, continent, type) %>% 
  f_summarize_subs %>% 
  ggplot(aes(x = year, y = median_subs, color = type)) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25) + 
  facet_wrap(~continent) +
  labs(x = "Years", 
       y = "Median subscriptions per 100 people", 
       title = "Phones vs. landlines adoption rate over time by continents",
       subtitle = "Ribbon : 25th-75th percentile")

country_incomes <- WDI(indicator = c(gdp_per_capita = "NY.GDP.PCAP.PP.KD",
                                     pop = "SP.POP.TOTL"), 
                       start = 2005, end = 2005, extra = TRUE) %>% 
    as_tibble() %>% 
    select(code = iso3c, income, gdp_per_capita, pop) %>% 
    filter(!is.na(income)) %>% 
    mutate(income = fct_relevel(income, 
                                c("Low income", 
                                  "Lower middle income", 
                                  "Upper middle income", 
                                  "High income")))

by_year_income <- phones %>% 
  inner_join(country_incomes, by = "code") %>% 
  group_by(year, income, type) %>% 
  f_summarize_subs() 

by_year_income %>% 
  ggplot(aes(x = year, y = median_subs, color = type)) +
  geom_line() +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25) + 
  facet_wrap(~income) +
  labs(
    x = "Years", 
    y = "Median subscriptions per 100 people", 
    title = "Phones vs. landlines adoption rate over time by income",
    subtitle = "Ribbon : 25th-75th percentile"
  )

by_year_income %>% 
  mutate(income = fct_rev(income)) %>% 
  ggplot(aes(x = year, y = median_subs, color = income)) +
  geom_line() +
  facet_wrap(~type, ncol = 1) +
  labs(
    x = "Years", 
    y = "Median subscriptions per 100 people", 
    title = "Phones vs. landlines adoption rate over time by income",
    subtitle = "Ribbon : 25th-75th percentile",
    color = "Income"
  )
  
mobile %>% 
  inner_join((landline %>% select(code, year, total_pop, gdp_per_cap)),
             by = c("code", "year"),
             suffix = c("_mobile", "_landline")) %>% 
  filter(total_pop_landline != total_pop_mobile) %>% 
  ggplot(aes(total_pop_mobile, total_pop_landline)) +
  geom_point() +
  geom_abline(color = "red")

phones %>% 
  semi_join(avg_country_pop %>% slice_max(avg_population, n = 50), by = "country") %>% 
  ggplot(aes(x = year, y = subscriptions, color = type, group = interaction(type, country))) +
  geom_line() +
  facet_wrap(~continent) +
  geom_hline(yintercept = 50, lty = 2) +
  labs(color = "") 

countries_summarized <- phones %>% 
  filter(!is.na(subscriptions)) %>% 
  select(-total_pop, - gdp_per_cap) %>% 
  pivot_wider(names_from = type, values_from = subscriptions) %>% 
  group_by(continent, country, code) %>% 
  summarize(year_past_50_mobile = na_if(min(year[mobile >= 50]), Inf),
            peak_landline = max(landline, na.rm = TRUE),
            peak_mobile = max(mobile),
            n_mobile = sum(!is.na(mobile))) %>%
  ungroup() %>% 
  inner_join(country_incomes, by = "code") %>% 
  filter(n_mobile >= 25) %>% 
  arrange(desc(year_past_50_mobile))

countries_summarized %>% 
  filter(!is.na(gdp_per_capita)) %>% 
  ggplot(aes(gdp_per_capita, 
             year_past_50_mobile, 
             color = continent, 
             size = pop)) +
  geom_point() +
  scale_x_log10(labels = dollar) +
  labs(x = "GDP per capita (in 2017 $)",
       y = "Year where subscriptions go past 50%")

countries_summarized %>% 
  filter(!is.na(gdp_per_capita)) %>% 
  ggplot(aes(gdp_per_capita, 
             peak_landline, 
             color = continent, 
             size = pop)) +
  geom_point() +
  scale_x_log10(labels = dollar) +
  labs(x = "GDP per capita (in 2017 $)",
       y = "Peak landlines") +
  facet_wrap(~continent) +
  theme(legend.position = "none")

countries_summarized %>% 
  filter(continent == "Oceania") %>% 
  arrange(desc(gdp_per_capita)) %>% 
  View()

world_map_mobile <- map_data("world") %>%
  as.tibble() %>%
  regex_left_join(maps::iso3166, c(region = "mapname")) %>% 
  left_join(mobile, by = c(a3 = "code"))
  
world_map_mobile %>% 
  ggplot(aes(long, lat, group = group, fill = subscriptions)) +
  geom_polygon() +
  coord_fixed(1.5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       midpoint = 30) +
  ggthemes::theme_map() +
  labs(title = 'Year: {as.integer(current_frame)}', fill = "# of subscriptions") +
  transition_manual(year)





