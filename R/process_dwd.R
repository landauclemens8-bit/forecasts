

ffm_clean = ffm %>% 
  mutate(time_local = with_tz(time_utc, "Europe/Berlin")) |>   # new deutsche Zeit
  mutate(date  = as.Date(time_local, tz = "Europe/Berlin"),
         year  = year(time_local),
         month = month(time_local),
         day   = day(time_local),
         hour  = hour(time_local)) |>
  select(time_utc, time_local, date, year, month, day, hour, T2M, WS10)


head(ffm)
tail(ffm_clean)

write.csv2(ffm_clean, "data/processed/ffm_clean.csv", row.names = FALSE)

