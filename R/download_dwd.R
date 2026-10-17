library("rdwd")
id = findID("Frankfurt/Main")

link_temp = selectDWD(id = id, res = "hourly", var = "air_temperature", per = "hr")
link_wind = selectDWD(id = id, res = "hourly", var = "wind", per = "hr")
temp = dataDWD(link_temp, hr = 2, force = T)  
# Rdwd stores downloaded files and can reuse the old copy when called up again, so have to set force = T for the latest data
wind = dataDWD(link_wind, hr = 2, force = T)


ffm = merge(temp[, c("MESS_DATUM", "TT_TU")],
            wind[, c("MESS_DATUM", "F")],
            by = "MESS_DATUM", all = TRUE)
names(ffm) = c ("time_utc", "T2M", "WS10")

saveRDS(ffm, "data/raw/ffm.rds")
