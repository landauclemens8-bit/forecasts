# Backtesting KNN

probs     = c(0.025, 0.25, 0.5, 0.75, 0.975)
features  = c("T2M", "month", "WS10", "day")
leads     = c(60, 64, 68, 84, 88, 92)
hor_names = c("36 hour", "40 hour", "44 hour", "60 hour", "64 hour", "68 hour")


ffm   = read.csv2("data/processed/ffm_clean.csv") 
n   = nrow(ffm)

# observations
obs_all = sapply(leads, function(l) lead(ffm$T2M, l))
names(obs_all) = c("60 hour", "64 hour", "68 hour", "84 hour", "88 hour", "92 hour")


# contains rownumbers of the simulated forecast dates (Tuesday, 23:00 Uhr)
years_back = 20 # wie weit ich daten verwenden möchte
cand = which(ffm$weekday == "Tuesday" & ffm$hour == 23 &
               seq_len(n) > n - years_back*8766 &  # 8766 stunden pro jahr 
               seq_len(n) <= n - 92)       # nicht die letzten 92 tage 

cand = cand[complete.cases(ffm[cand, features]) & complete.cases(obs_all[cand, ])]




# PIT Histogramm for calibration
tes = do.call(rbind, lapply(cand, function(i){
  nb = KNN_function(ffm$time_utc[i], features, 60) 
  nb = data.frame(nb)
  names(nb) = 1:60
  data.frame(nb, obs = obs_all[i, ])
}))

pit_values = rowMeans(tes[, 1:60] <= tes[,61])
# plot of the Histogramm
hist(pit_values,
     breaks = seq(0, 1, length.out = 15 + 1),
     freq = FALSE,            # density instead of absolute quantaty
     ylim = c(0, 3),        
     col = "blue",
     border = "white",
     main = "PIT Histogramm (Forecast Calibration)",
     xlab = "PIT-Werte",
     ylab = "Dichte")

# refrence line for perfect unitf
abline(h = 1, col = "red", lwd = 2, lty = 2)




## Pinball loss/ qunatile score function

pinball = function(y, q, tau) ifelse(y >= q, tau*(y - q), (1 - tau)*(q - y))

bt_loss = function(features, w = rep(1, length(features)), k = 50, origins = cand){
  bt = do.call(rbind, lapply(origins, function(i){
    nb = KNN_function(ffm$time_utc[i], features, k, w)
    q  = t(apply(nb, 1, quantile, probs = probs, na.rm = TRUE))
    data.frame(horizon = hor_names, q, obs = obs_all[i, ])
  }))
  names(bt)[2:6] = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
  
  pl = sapply(1:5, function(j) pinball(bt$obs, bt[[j + 1]], probs[j]))
  c(pinball = mean(pl, na.rm = TRUE),
    cov95   = mean(bt$obs >= bt$q0.025 & bt$obs <= bt$q0.975, na.rm = TRUE),
    width95 = mean(bt$q0.975 - bt$q0.025, na.rm = TRUE))
}




# choosing wich features even matter 
# all possible feature combis 
feature_combi = unlist(lapply(1:4, function(ft){
  combn(features, ft, simplify = F)
}), recursive = F)

# computing the pinball loss for each combi
wich_features = do.call(rbind, lapply(feature_combi, function(ft){
  result = bt_loss(features = ft)
  
  data.frame(features = paste(ft, collapse = " + "),
    n_features = length(ft), t(result))
}))
# plot for the feature selection
library(ggplot2)
ggplot(wich_features) + 
  geom_point(aes(x = pinball, y = width95, colour = features, shape = features), size = 3) +
  scale_shape_manual(values = 1:15) +
  theme_classic()
# lowest pinball has T2M + month + day
# 
wich_f_order_pb = wich_features[order(wich_features$pinball),]
wich_f_order_width = wich_features[order(wich_features$width95),]


# for choosing the right k 
opt_features = c("T2M", "month", "day")
ks = seq(5, 100, by = 5)
wich_k = do.call(rbind, lapply(ks, function(k){
  result = bt_loss(features = opt_features, k = k)
  data.frame( k = k, t(result))
}))


# ploting bt_loss() for k = 1,...,100
ggplot(wich_k, aes(x = k, y = pinball)) + geom_line(linewidth = 1, colour = "blue") +
  ylim(0.95,1) +
  labs(x = "k", y = "pinball loss", title = "pinball loss depending on k") +
  theme_classic()
# min ca. at k = 60 

# ploting the 95% quantile for k = seq(5,100, by 5)
ggplot(wich_k, aes(x = k, y = width95)) + geom_line(linewidth = 1, colour = "blue") +
  labs(x = "k", y = "width of 95% quantile", title = "quantile width depending on k") +
  theme_classic()
# getting larger for increasing k, so may choose k between 50 and 70









