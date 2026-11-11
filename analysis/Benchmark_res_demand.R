library(ggplot2)
library(dplyr)
library(lubridate)
#Datensatz einlesen 
SMARD = read.csv2("data/processed/SMARD.csv")

tail(SMARD)

#days
days = c(5,6)
# hours
hours = c(12, 16, 20)
# probabilities
probs = c(0.025, 0.25, 0.5, 0.75, 0.975)
# empty data frame for the quantiles
quantiles = matrix(numeric(0), ncol = length(probs))
# res load von den letzten freitagen und Samstagen
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

# res load von den letzten Tagen 
quantilesDays = matrix(numeric(0), ncol = length(probs))
yearsDay = c(2021, 2022, 2023, 2024, 2025)
weekdays = c(5,6)
datesO = c(1:30) 
for (wd in weekdays) {
  for (h in hours) {
    lastYearDay = SMARD |> 
      filter(year %in% yearsDay ,month == 11, day %in% datesO, hour == h, wday(date, week_start = 1) == wd)
    q = quantile(lastYearDay$residual_load, probs = probs, na.rm = TRUE)
    quantilesDays = rbind(quantilesDays,q)
  }
}

quantilesDays

newquants_res = (quantiles + quantilesDays)/2

forecast_date = "2026-11-04"   # Mittwoch der Abgabe

sub_res_demand = data.frame(
  forecast_date = forecast_date,
  target  = "res_demand",
  horizon = c("36 hour", "40 hour", "44 hour", "60 hour", "64 hour", "68 hour"),
  round(newquants_res, 2)
)
names(sub_res_demand)[4:8] = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
rownames(sub_res_demand) = NULL

sub_res_demand

# save in processed data
write.csv2(sub_res_demand, "data/processed/quantiles_res_demand.csv", row.names = FALSE)




