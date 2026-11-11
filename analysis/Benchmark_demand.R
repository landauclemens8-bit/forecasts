library(ggplot2)
library(dplyr)
library(lubridate)
#Datensatz einlesen 
SMARD = read.csv2("data/processed/SMARD.csv")
head(SMARD)
#days
days = c(5,6)
# hours
hours = c(12, 16, 20)
# probabilities
probs = c(0.025, 0.25, 0.5, 0.75, 0.975)
# empty data frame for the quantiles
quantiles = matrix(numeric(0), ncol = length(probs))
#quantiles = data.frame()

# letzten 20 Tage Freitage/Samstage
for(wd in days){
  for (h in hours) {
    lastDays = SMARD |> 
      filter(wday(date, week_start = 1) == wd, hour == h)
    last20 = tail(lastDays,20)
    q = quantile(last20$load, probs = probs, na.rm = TRUE)
    quantiles = rbind(quantiles,q)
  }
}
quantiles

# this day in the last years
quantilesDay = matrix(numeric(0), ncol = length(probs))
lastYears = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)

# datum bei jeder abgabe ändern, month in der for-schleife auch 
datesO = c(7,8) 

for (d in datesO) {
  for (h in hours) {
    lastYearDay = SMARD |> 
      filter(year %in% lastYears,month == 11, day == d, hour == h)
    q = quantile(lastYearDay$load, probs = probs, na.rm = TRUE)
    quantilesDay = rbind(quantilesDay,q)
  }
}

quantilesDay

# mittelwert von den quantilen der letzten jahre an den tagen und der letzten Freitage und Samstage 
newquants = (quantiles + quantilesDay)/2

forecast_date = "2026-11-04"   # Mittwoch der Abgabe

sub_demand = data.frame(
  forecast_date = forecast_date,
  target  = "demand",
  horizon = c("36 hour", "40 hour", "44 hour", "60 hour", "64 hour", "68 hour"),
  round(newquants, 2)
)
names(sub_demand)[4:8] = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")
rownames(sub_demand) = NULL

sub_demand
# save in processed data
write.csv2(sub_demand, "data/processed/quantiles_demand.csv", row.names = FALSE)

#Plots
ggplot(recent20, aes(y = load)) + geom_boxplot() + 
  geom_point(aes(y = load, x = 0, colour = date)) +
  theme_classic()
ggplot(recent20, aes(date, load)) +
  geom_point() +
  geom_point(aes(x = as.Date("2026-10-23 2026"), y = 64.66), colour = "red")

