

library(jsonlite)

# URL linkt von open-Meteo API
url = paste0("https://ensemble-api.open-meteo.com/v1/ensemble?latitude=50.0565&longitude=8.5925&hourly=temperature_2m,wind_speed_10m&models=ecmwf_ifs025_ensemble")

# in data frame umwandeln
om = as.data.frame(fromJSON(url)$hourly)
names(om)

# getting time in the right format 
om$time = as.POSIXct(om$time, format = "%Y-%m-%dT%H:%M", tz = "Europe/Berlin")

# time to forecast
targets_utc = as.POSIXct(c("2026-10-23 12:00", "2026-10-23 16:00", "2026-10-23 20:00",
                            "2026-10-24 12:00", "2026-10-24 16:00", "2026-10-24 20:00"),
                          tz = "Europe/Berlin")

om_t = om |> filter(time %in% targets_utc)


probs = c(0.025, 0.25, 0.5, 0.75, 0.975)

robs = c(0.025, 0.25, 0.5, 0.75, 0.975)
# part the data into T2M and WS10
temp_members = om_t |> select(starts_with("temperature_2m"))
wind_members = om_t |> select(starts_with("wind_speed_10m"))

# compute quantalies 
q_temp = t(apply(temp_members, 1, quantile, probs = probs, na.rm = TRUE))
q_wind = t(apply(wind_members, 1, quantile, probs = probs, na.rm = TRUE))


# formating for submission 
forecast_date = "2026-10-21"   # Mittwoch der Abgabe
# for temperature
sub_q_temp = data.frame(
  forecast_date = forecast_date,
  target  = "temperature",
  horizon = c("36 hour", "40 hour", "44 hour", "60 hour", "64 hour", "68 hour"),
  round(q_temp, 2)
)
names(sub_q_temp)[4:8] = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
rownames(sub_q_temp) = NULL

# for wind 
sub_q_wind = data.frame(
  forecast_date = forecast_date,
  target  = "wind",
  horizon = c("36 hour", "40 hour", "44 hour", "60 hour", "64 hour", "68 hour"),
  round(q_wind, 2)
)
names(sub_q_wind)[4:8] = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
rownames(sub_q_wind) = NULL

# save in processed data
write.csv2(sub_q_temp, "data/processed/quantiles_temp.csv", row.names = FALSE)
write.csv2(sub_q_wind, "data/processed/quantiles_wind.csv", row.names = FALSE)
