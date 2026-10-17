library(ggplot2)


SMARD |>
  group_by(year) |>
  summarise(load_mean = mean(load, na.rm = TRUE)) |>
  ggplot(aes(year, load_mean, colour = factor(year))) +
  geom_line(aes(year, load_mean),colour = "red") +
  geom_point() +
  labs(x = "Jahr", y = "mittlere Load (GWh)")

ffm_clean |> 
  group_by(year, month) |>
  summarise(T2M_mean = mean(T2M, na.rm = T), .groups = "drop") |>
  ggplot(aes(month, T2M_mean, group = year, colour = year))+
  geom_line() +
  geom_point() + 
  labs(x = "Jahr", y = "mean temperature")

head(SMARD)heyearad(SMARD)
