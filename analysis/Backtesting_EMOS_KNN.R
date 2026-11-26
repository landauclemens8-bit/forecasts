# EMOS Backtesting 

#K-NN function laden
source("analysis/K-NN.R")

features = c("T2M", "month", "day")
k = 50
years_back = 30
leads = c(60, 64, 68, 84, 88, 92)
horizon_names = c("36 hour", "40 hour", "44 hour", "60 hour", "64 hour", "68 hour")

ffm = read.csv2("data/processed/ffm_clean.csv")
n = nrow(ffm)

# observations of the forecasthorizons 
obs_all = sapply(leads, function(l) lead(ffm$T2M, l))
colnames(obs_all) = horizon_names

# contains rownumbers of the simulated forecast dates (Tuesday, 23:00 Uhr)
cand = which(ffm$weekday == "Tuesday" & ffm$hour == 23 &
               seq_len(n) > n - years_back*8766 &  # 8766 stunden pro jahr 
               seq_len(n) <= n - 92)       # nicht die letzten 92 tage 

cand = cand[complete.cases(ffm[cand, features]) & complete.cases(obs_all[cand, ])]

bt_emos = do.call(rbind, lapply(cand, function(i) {
  # KNN: 6 Horizonte x k Nachbarn
  nb = KNN_function(ffm$time_utc[i], features, k)
  
  data.frame(origin = ffm$time_utc[i], horizon = hor_names, obs = obs_all[i, ],
             knn_mean = apply(nb, 1, mean, na.rm = TRUE), 
             knn_var  = apply(nb, 1, var, na.rm = TRUE),
             knn_q0.025 = apply(nb, 1, quantile, probs = 0.025, na.rm = TRUE),
             knn_q0.25 = apply(nb, 1, quantile, probs = 0.25, na.rm = TRUE), 
             knn_q0.5 = apply(nb, 1, quantile, probs = 0.5, na.rm = TRUE), 
             knn_q0.75  = apply(nb, 1, quantile, probs = 0.75, na.rm = TRUE), 
             knn_q0.975 = apply(nb, 1, quantile, probs = 0.975, na.rm = TRUE)
  )
}))

head(bt_emos)


# origin dates 
origins = unique(bt_emos$origin)

# split into Training and test data 70/30
split_point = 0.7 * length(origins)
train_origins = origins[1:split_point]
test_origins = origins[(split_point +1):length(origins)]
# emos train
emos_train = bt_emos %>%
  filter(origin %in% train_origins)
# emos test
emos_test = bt_emos %>%
  filter(origin %in% test_origins)


# CRPS einer Gaussian forecast dist
crps_gaussian <- function(mu, sigma, y) {
  z = (y - mu) / sigma
  sigma * (z * (2 * pnorm(z) - 1) + 2 * dnorm(z) - 1 / sqrt(pi))
}

# estimate parameters
fit_emos = function(data){
  objective = function(theta) {
    a = theta[1]
    b = theta[2]
    c = exp(theta[3]) # exp() so the variance is non-negative
    d = exp(theta[4])
    
    mu = a + b * data$knn_mean
    
    sigma = sqrt(c + d * data$knn_var)
    
    scores = crps_gaussian(y = data$obs, mu = mu, sigma = sigma)
    
    mean(scores, na.rm = TRUE)
  }
  
  opt = optim(par = c(0, 1, log(1), log(1)), fn = objective, method = "BFGS")
  
  list(a = opt$par[1], b = opt$par[2], c = exp(opt$par[3]), d = exp(opt$par[4]), 
       crps = opt$value, convergence = opt$convergence)
}


predict_emos = function(data, fit) {
  mu = fit$a + fit$b * data$knn_mean
  sigma = sqrt(fit$c + fit$d * data$knn_var)
  
  data.frame(mu = mu, sigma = sigma,
             q0.025 = qnorm(0.025, mu, sigma), 
             q0.25 = qnorm(0.25, mu, sigma), 
             q0.5 = qnorm(0.50, mu, sigma),
             q0.75 = qnorm(0.75, mu, sigma),
             q0.975 = qnorm(0.975, mu, sigma)
  )
}


emos_fits = lapply(hor_names, function(h) {
  train_h = emos_train %>%
    filter(horizon == h)
  
  fit_emos(train_h)
})

names(emos_fits) = hor_names

# Parameter anzeigen
emos_parameters = do.call(rbind, lapply(hor_names, function(h) {
  fit = emos_fits[[h]]
  
  data.frame(horizon = h, 
             a = fit$a, b = fit$b, c = fit$c, d = fit$d, 
             training_CRPS = fit$crps, 
             convergence = fit$convergence)
}))

emos_parameters

emos_predictions = do.call(rbind, lapply(hor_names, function(h) {
  test_h = emos_test %>%
    filter(horizon == h)
  
  pred = predict_emos(data = test_h, fit = emos_fits[[h]])
  
  cbind(test_h[, c("origin", "horizon", "obs")], pred)
}))

head(emos_predictions)


pinball = function(y, q, tau) {
  ifelse(y >= q, tau * (y - q), (1 - tau) * (q - y))
}

emos_test_scores = do.call(rbind, lapply(hor_names, function(h) {
  knn_h = emos_test %>%
    filter(horizon == h)
  
  emos_h = emos_predictions %>%
    filter(horizon == h)
  
  # Pinball Loss KNN
  knn_pl = sapply(1:5, function(j) {
    pinball(knn_h$obs, knn_h[[paste0("knn_q", probs[j])]], probs[j])
  })
  
  # Pinball Loss EMOS
  emos_pl = sapply(1:5, function(j) {
    pinball(emos_h$obs, emos_h[[paste0("q", probs[j])]], probs[j])
  })
  
  data.frame(horizon = h, KNN_pinball = mean(knn_pl, na.rm = TRUE), 
             EMOS_pinball = mean(emos_pl, na.rm = TRUE),
             KNN_coverage = mean(knn_h$obs >= knn_h$knn_q0.025 &
                                   knn_h$obs <= knn_h$knn_q0.975, 
                                 na.rm = TRUE),
             EMOS_coverage = mean(emos_h$obs >= emos_h$q0.025 & 
                                    emos_h$obs <= emos_h$q0.975,
                                  na.rm = TRUE),
             KNN_width = mean(knn_h$knn_q0.975 - knn_h$knn_q0.025, na.rm = TRUE),
             EMOS_width = mean(emos_h$q0.975 - emos_h$q0.025, na.rm = TRUE),
             KNN_bias = mean(knn_h$obs - knn_h$knn_q0.5, na.rm = TRUE),
             EMOS_bias = mean(emos_h$obs - emos_h$mu, na.rm = TRUE),
             EMOS_CRPS = mean(crps_gaussian(emos_h$obs, emos_h$mu, emos_h$sigma),
                              na.rm = TRUE)
  )
}))

emos_test_scores

library(tidyr)

score_long = emos_test_scores %>%
  select(horizon, KNN_coverage, EMOS_coverage, KNN_width, EMOS_width, KNN_pinball,
         EMOS_pinball) %>% 
  pivot_longer(cols = -horizon, names_to = c("model", "metric"), 
               names_pattern = "(KNN|EMOS)_(.*)", values_to = "value")

# Coverage
ggplot(score_long %>% filter(metric == "coverage"),
       aes(x = horizon, y = value, fill = model)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(title = "95% Coverage: KNN vs EMOS",
       y = "Coverage", x = "Forecast horizon") +
  theme_classic()

ggplot(score_long %>% filter(metric == "width"),
  aes(x = horizon, y = value, fill = model)) +
  geom_col(position = "dodge") +
  labs(title = "95% Interval width: KNN vs EMOS",
       y = "Mean interval width (°C)", x = "Forecast horizon") +
  theme_classic()

ggplot(score_long %>% filter(metric == "pinball"),
       aes(x = horizon, y = value, fill = model)) +
  geom_col(position = "dodge") +
  labs(title = "Pinball Loss: KNN vs EMOS", 
       y = "Mean Pinball Loss", x = "Forecast horizon") +
  theme_classic()
emos_predictions$PIT = pnorm((emos_predictions$obs - emos_predictions$mu) /
    emos_predictions$sigma)

ggplot(emos_predictions, aes(x = PIT)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10, boundary = 0, 
                 color = "black", fill = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  facet_wrap(~ horizon) +
  labs(title = "PIT Histogram – Gaussian EMOS", x = "PIT", y = "Density") +
  theme_classic()

######
# forecast for the week
######

# KNN-Ensemble
nb = KNN_function("2026-11-24 23:00:00", features, 50)

# KNN-Mittelwert und Varianz
knn_mean = apply(nb, 1, mean, na.rm = TRUE)
knn_var = apply(nb, 1, var, na.rm = TRUE)

# EMOS-Verteilung
mu = fit$a + fit$b * knn_mean
sigma = sqrt(fit$c + fit$d * knn_var)

# EMOS-Quantile
q_temp_EMOS = sapply(probs, function(p) {
  qnorm(p, mean = mu, sd = sigma)
})

q_temp_emos_Ensable = (q_temp_EMOS + q_temp)/2

forecast_date = "2026-11-25"

su_q_temp = data.frame(forecast_date = forecast_date, target = "temperature", 
                       horizon = c("36 hour", "40 hour", "44 hour",
                                   "60 hour", "64 hour", "68 hour"), 
                       round(q_temp_emos_Ensable, 2))

names(su_q_temp)[4:8] = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")

rownames(su_q_temp) <- NULL

su_q_temp

write.csv(su_q_temp, "data/processed/quantiles_temp.csv", row.names = FALSE)





