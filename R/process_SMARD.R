library(dplyr)
library(lubridate)

SMARD1 = read.csv2("data/raw/Realisierter_Stromverbrauch_2015_2021_Stunde.csv")
SMARD2 = read.csv2("data/raw/Realisierter_Stromverbrauch_202101010000_202610200000_Stunde.csv")
head(SMARD1)

SMARD = bind_rows(SMARD1, SMARD2)

colnames(SMARD)
SMARD = SMARD |> 
  select(-"Netzlast.inkl..Pumpspeicher..MWh..Berechnete.Auflösungen", -"Pumpspeicher..MWh..Berechnete.Auflösungen")

write.csv2(SMARD, "data/processed/SMARD.csv", row.names = FALSE)

# helper function for formatting numbers
fmt2 <- function(x){
  formatC(x, width = 2, format = "d", flag = "0")
}

# cleaning the data 
SMARD <- SMARD %>%
  transmute(date1 = .[,1], date2 = .[,2],
            load = as.numeric(gsub("\\,", ".", gsub("\\.", "", .[,3])))/1000,           # MWh to GWh
            residual_load = as.numeric(gsub("\\,", ".", gsub("\\.", "", .[,4])))/1000) %>%
  mutate(year = as.integer(substr(date1, 7, 10)), # make calendar variables
         month = as.integer(substr(date1, 4, 5)),
         day = as.integer(substr(date1, 1, 2)), 
         hour = as.integer(substr(date1, 12, 13)) + 1, 
         date = as.Date(paste0(year, "-", fmt2(month), "-",
                               fmt2(day))), 
         weekday = weekdays(date),
         comment = c(NA, diff(hour)), 
         comment = if_else(comment %in% c(0, 2), "time shift", "")) %>%
  select(date, year, month, day, hour, weekday, load, 
         residual_load, comment)



duplicated_hours = duplicated(SMARD[, c("date", "hour")])
SMARD %>% filter(duplicated_hours)



write.csv2(SMARD, "data/processed/SMARD.csv", row.names = FALSE)
