# This file creates a graphic comparing income with average house value over time

income_zhvi <- zhvi %>% 
  group_by(metro, date) %>% 
  summarize(mean_zhvi = mean(zhvi, na.rm = TRUE)) %>% 
  full_join(median_income, by = c("metro", "date")) %>% 
  select(metro, date, mean_zhvi, median_income) %>% 
  drop_na()

income_zhvi$metro <- factor(
  income_zhvi$metro,
  levels = c("New York-Newark-Jersey City",
             "San Francisco-Oakland-Hayward",
             "Seattle-Tacoma-Bellevue"),
  labels = c("New York Area",
             "San Francisco Area",
             "Seattle Area")
)

graphic <- income_zhvi %>%
  ggplot(aes(x = median_income, y = mean_zhvi, color = metro)) +
  geom_point() +
  scale_x_continuous(labels = scales::dollar) + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Average ZHVI vs. Median Income: {frame_time}",
       x = "Median Income",
       y = "Average ZHVI") +
  transition_time(date)

anim_save("housing-explorations-app/income_graphic.gif", graphic)