library(ggplot2)
library(dplyr)
library(lubridate)
#Datensatz einlesen 
SMARD = read.csv2("data/processed/SMARD.csv")


#days
days = c(5,6)
# hours
hours = c(12, 16, 20)
# probabilities
probs = c(0.025, 0.25, 0.5, 0.75, 0.975)
# empty data frame for the quantiles
quantiles = matrix(numeric(0), ncol = length(probs))
#quantiles = data.frame()
for(wd in days){
  for (h in hours) {
    lastDays = SMARD |> 
      filter(wday(date, week_start = 1) == wd, hour == h)
    last20 = tail(lastDays,20)
    q = quantile(last20$residual_load, probs = probs, na.rm = TRUE)
    quantiles = rbind(quantiles,q)
  }
}

quantiles

forecast_date = "2026-10-21"   # Mittwoch der Abgabe

sub_res_demand = data.frame(
  forecast_date = forecast_date,
  target  = "demand",
  horizon = c("36 hour", "40 hour", "44 hour", "60 hour", "64 hour", "68 hour"),
  round(quantiles, 2)
)
names(sub_res_demand)[4:8] = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
rownames(sub_res_demand) = NULL

sub_res_demand

# save in processed data
write.csv2(sub_res_demand, "data/processed/quantiles_res_demand.csv", row.names = FALSE)

#Plots
lastDays = SMARD |> 
  filter(wday(date, week_start = 1) == wd, hour == 12)
last20 = tail(lastDays,120)

letzte = SMARD |> 
  filter(hour == 12)
letzte50 = tail(letzte, 50)
ggplot(last20, aes(y = residual_load)) + geom_boxplot() + 
  geom_point(aes(y = load, x = 0, colour = date)) +
  theme_classic()
ggplot(letzte50, aes(date, residual_load)) + 
  geom_line(aes(date, residual_load))


