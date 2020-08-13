# Albert Dominguez
# August 13, 2020
# Cumulative Cases and Deaths per Region



library(tidyverse)
head(covid)
region = data.frame(state = state.name, region = state.region)
head(region)

covidregion <- covid %>%
  right_join(region, by = "state") %>%
  group_by(region, date) %>%
  summarize(cases  = sum(cases),
            deaths = sum(deaths)) %>%
  pivot_longer(cols = c('cases', 'deaths'))

ggplot(covidregion, aes(x = date, y = value)) +
  geom_line(aes(col = region)) +
  facet_grid(name~region, scale = "free_y") +
  theme_bw() +
  theme(legend.position = "NA") +
  labs(title = "Cummulative Cases and Deaths: Region",
       y = "Daily Cumulative Count",
       x = "Date",
       subtitle = "COVID-19 Data: NY-Times" )
