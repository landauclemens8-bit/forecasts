library(dplyr)
library(tidyr)
library(FNN)

# update the date with the two skripts (nur ein mal am tag!)
source("R/download_dwd.R")
source("R/process_dwd.R")

ffm = read.csv2("data/processed/ffm_clean.csv")

# add cols with target values
ffm_temp = ffm %>%
  mutate(
    ziel_36h = lead(T2M, 60),
    ziel_40h = lead(T2M, 64),
    ziel_44h = lead(T2M, 68),
    ziel_60h = lead(T2M, 84),
    ziel_64h = lead(T2M, 88),
    ziel_68h = lead(T2M, 92)
  )

# remembering row number, remove NAs one time
ffm_train = ffm_temp %>%
  mutate(row_id = row_number()) %>%
  drop_na() %>%
  filter(hour == 23)               # always the same time, when the forecast is made

KNN_function = function(obs_time, features, k,  w = rep(1, length(features))){
  
  # cuurent state beefore removing rows 
  current_state = ffm_temp |>
    filter(time_utc == obs_time) |>
    select(all_of(features))
  
  # for filter in next step 
  i_now = which(ffm_temp$time_utc == obs_time)
  hour_now = ffm_temp$hour[i_now]
  # only rows, with known 92-h-target to obs_time, othwerwise would contain NAs
  ffm_temp = ffm_train %>%
    filter(row_id <= i_now - 92, hour == hour_now)
  
  feature_cols = features
  # scale the data
  train_scaled = scale(ffm_temp[, feature_cols])
  train_center = attr(train_scaled, "scaled:center")
  train_scale  = attr(train_scaled, "scaled:scale")
  
  current_scaled = scale(current_state, center = train_center, scale = train_scale)
  
  
  # weights of the features 
  train_w   = sweep(train_scaled, 2, w, "*")
  current_w = sweep(as.matrix(current_scaled), 2, w, "*")
  
  # brute-force statt kd-Baum
  neighbor_indices = knnx.index(data = train_w, query = current_w,
                                k = k, algorithm = "brute")[1, ]
  
  ziel_spalten = c("ziel_36h", "ziel_40h", "ziel_44h", "ziel_60h", "ziel_64h", "ziel_68h")
  neighbor_outcomes_matrix = t(as.matrix(ffm_temp[neighbor_indices, ziel_spalten]))
  
  return(neighbor_outcomes_matrix)
}

probs = c(0.025, 0.25, 0.5, 0.75, 0.975)

features = c("T2M", "month", "day", "WS10")
q_temp_KNN = t(apply(KNN_function("2026-11-23 23:00:00", features, 50), 1, quantile, probs = probs, na.rm = TRUE))

# q_temp_KNN = (q_temp_KNN + q_temp)/2

forecast_date = "2026-11-25"   # Mittwoch der Abgabe
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





