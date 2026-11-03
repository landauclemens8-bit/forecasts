# einlesen der dateinen 
temp_submission = read.csv2("data/processed/quantiles_temp.csv")
wind_submission = read.csv2("data/processed/quantiles_wind.csv")
demand_submission = read.csv2("data/processed/quantiles_demand.csv")
res_demand_submission = read.csv2("data/processed/quantiles_res_demand.csv")

submission = bind_rows(demand_submission, res_demand_submission, temp_submission, wind_submission)

write.csv(submission, "submissions/firstWeek.csv", row.names = FALSE)
