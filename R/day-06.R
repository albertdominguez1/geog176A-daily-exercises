# Albert Dominguez
# August 12, 2020
# Codes for plots regarding COVID-19 cases


states <- covid %>%
  filter(date == max(date)) %>%
  group_by(state) %>%
  summarize(cases = sum(cases, n.rm = TRUE)) %>%
  arrange(-cases) %>%
  ungroup() %>%
  head(6) %>%
  pull(state)

covid %>%
  filter(state %in% states) %>%
  group_by(state, date) %>%
  summarize(cases = sum(cases, n.rm = TRUE)) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(aes(color = state)) +
  labs(title = "Cumulative Case Counts: COVID-19 Pandemic",
       x = "Date",
       y = "Cases") +
  theme_bw() +
  theme(legend.position="none") +
  facet_wrap(~state)


covid %>%
  group_by(date) %>%
  summarize(cases = sum(cases, n.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(color = "red") +
  labs(title = "National Cumulative Case Counts: COVID-19 Pandemic",
       x = "Date",
       y = "Cases")+
  theme_light()



