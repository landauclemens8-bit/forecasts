library(jsonlite)
library(dplyr)
library(lubridate)

spalten_namen = c("time", "temp_obs", paste0("member_temp_", 1:50), "wind_obs",paste0("member_wind_", 1:50))

# read data and formating 
data_cal = read.csv("data/raw/open-meteo_30-10.csv", sep = ",",             
               header = FALSE,        
               skip = 3,              
               col.names = spalten_namen # set new col names 
)


# getting time in the right format 
data_cal$time = as.POSIXct(data_cal$time, format = "%Y-%m-%dT%H:%M", tz = "UTC")

data_cal1 = data_cal |>   # 
  mutate(date  = as.Date(time, tz = "UTC"),
         year  = year(time),
         month = month(time),
         day   = day(time),
         hour  = hour(time)) |>
  select(time, date, year, month, day, hour, temp_obs, paste0("member_temp_", 1:50), wind_obs,paste0("member_wind_", 1:50))


# time to forecast
targets_utc = as.POSIXct(c("2026-10-30 12:00", "2026-10-30 16:00", "2026-10-30 20:00"),
                         tz = "UTC")

data_cal_filtered = data_cal1 |> 
  filter(day %in% c(30,31,1,2))



member_cols = paste0("member_temp_", 1:50)
ens = as.matrix(data_cal_filtered[, member_cols])
obs = data_cal_filtered$temp_obs

# Rang der Beobachtung, bei Ties zufällig innerhalb der möglichen Ränge
rank_obs = function(members, y) {
  lo <- sum(members <  y) + 1
  hi <- sum(members <= y) + 1
  if (lo == hi) lo else sample(lo:hi, 1)
}

r = sapply(seq_len(nrow(ens)), function(i) rank_obs(ens[i, ], obs[i]))

m = length(member_cols)
hist((r-1)/50, breaks = seq(0, 1, by = 0.02), freq = FALSE,
     xlab = "Rang", main = "Rank-Histogramm Temperatur")
abline(h = 1)

# passt alles nicht so gut, daten sind stark korelliert und es sind zu wenig daten um wirkliche aussagen zu treffen 

