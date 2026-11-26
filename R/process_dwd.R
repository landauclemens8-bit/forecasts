library(dplyr)
library(lubridate)
ffm_clean = ffm %>% 
  mutate(time_local = with_tz(time_utc, "Europe/Berlin")) |>   # 
  mutate(date  = as.Date(time_utc, tz = "utc"),
         year  = year(time_utc),
         month = month(time_utc),
         day   = day(time_utc),
         weekday = weekdays(time_utc),
         hour  = hour(time_utc)) |>
  select(time_utc, time_local, date, year, month, day, weekday, hour, T2M, WS10)


tail(ffm)
tail(ffm_clean)

write.csv2(ffm_clean, "data/processed/ffm_clean.csv", row.names = FALSE)

