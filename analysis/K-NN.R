library(dplyr)
library(tidyr)
library(FNN)

# update the date with the two skripts (nur ein mal am tag!)
source("R/download_dwd.R")
source("R/process_dwd.R")

ffm = read.csv2("data/processed/ffm_clean.csv")
tail(ffm)

# remove rows with NAs
ffm_temp = ffm %>% drop_na()

KNN_function = function(obs_time, features, k){
  library(dplyr)
  library(tidyr)
  library(FNN)
  ffm = read.csv2("data/processed/ffm_clean.csv")
  
  # remove rows with NAs
  ffm_temp = ffm %>% drop_na()
  
  # add cols with target values
  ffm_temp = ffm_temp %>%
    mutate(
      ziel_36h = lead(T2M, 60),   # 60, weil wir nur daten bis dienstag abend haben 
      ziel_40h = lead(T2M, 64),   # deswegen müssen wir 24 stunden drauf rechen 
      ziel_44h = lead(T2M, 68),
      ziel_60h = lead(T2M, 84),
      ziel_64h = lead(T2M, 88),
      ziel_68h = lead(T2M, 92)
    )
  
  # cols I want to have an impact on K-NN
  feature_cols = features
  
  # scale the selected cols
  train_scaled = scale(ffm_temp[, feature_cols])
  
  # compute train center and scale, to scale the current values
  train_center = attr(train_scaled, "scaled:center")
  train_scale  = attr(train_scaled, "scaled:scale")
  
  
  current_state = ffm_temp |>
    filter(time_utc == obs_time) |>
    select(all_of(feature_cols))
  
  current_scaled = scale(current_state, center = train_center, scale = train_scale)
  
  neighbor_indices = knnx.index(data = train_scaled, query = as.matrix(current_scaled), k = k)[1, ]
  
  ziel_spalten = c("ziel_36h", "ziel_40h", "ziel_44h", "ziel_60h", "ziel_64h", "ziel_68h")
  
  # matrix with the k nearest neigbours 
  neighbor_outcomes_matrix = t(as.matrix(ffm_temp[neighbor_indices, ziel_spalten]))
  
  return(neighbor_outcomes_matrix)
}

probs = c(0.025, 0.25, 0.5, 0.75, 0.975)

features = c("T2M", "month", "hour", "day", "WS10")
q_temp_KNN = t(apply(KNN_function("2026-11-17 23:00:00", features, 50), 1, quantile, probs = probs, na.rm = TRUE))

q_temp_KNN = (q_temp_KNN + q_temp)/2

forecast_date = "2026-11-18"   # Mittwoch der Abgabe
# for temperature

su_q_temp = data.frame(
  forecast_date = forecast_date,
  target  = "temperature",
  horizon = c("36 hour", "40 hour", "44 hour", "60 hour", "64 hour", "68 hour"),
  round(q_temp_KNN, 2)
)
names(su_q_temp)[4:8] = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
rownames(su_q_temp) = NULL

su_q_temp
write.csv(su_q_temp, "data/processed/quantiles_temp.csv", row.names = F)





