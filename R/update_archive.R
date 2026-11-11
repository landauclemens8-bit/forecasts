library(jsonlite)
library(dplyr)

# 1. Konfiguration (Koordinaten für Frankfurt)
lat = 50.0565
lon = 8.5925

# Gewünschter Zielordner und Dateipfad
archive_dir = "data/raw/EMOS-Data"
if (!dir.exists(archive_dir)) {
  dir.create(archive_dir, recursive = TRUE)
}
archive_file = file.path(archive_dir, "ensemble_archiv.csv")

# 2. URL für Open-Meteo Ensemble API zusammenbauen
url = paste0(
  "https://ensemble-api.open-meteo.com/v1/ensemble?",
  "latitude=", lat, "&longitude=", lon,
  "&hourly=temperature_2m,wind_speed_10m&models=ecmwf_ifs025_ensemble"
)

# 3. Daten abrufen und verarbeiten
cat("Lade aktuelle Ensemble-Daten von Open-Meteo herunter...\n")

tryCatch({
  response = fromJSON(url)
  om_new = as.data.frame(response$hourly)
  
  # Zeitstempel der Generierung hinzufügen
  om_new$issue_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  om_new = om_new %>% select(issue_time, time, everything())
  
  # 4. In das lokale Archiv einpflegen (Append & Deduplication)
  if (file.exists(archive_file)) {
    om_old = read.csv(archive_file, stringsAsFactors = FALSE)
    
    om_new$time = as.character(om_new$time)
    om_old$time = as.character(om_old$time)
    
    om_combined = bind_rows(om_old, om_new) %>% 
      distinct(issue_time, time, .keep_all = TRUE)
    
    write.csv(om_combined, archive_file, row.names = FALSE)
    cat("Archiv erfolgreich aktualisiert. Gesamte Zeilen:", nrow(om_combined), "\n")
    
  } else {
    write.csv(om_new, archive_file, row.names = FALSE)
    cat("Neues Archiv erstellt mit", nrow(om_new), "Zeilen.\n")
  }
  
}, error = function(e) {
  cat("Fehler beim Herunterladen der Daten:", e$message, "\n")
  quit(status = 1) # Beendet den Prozess mit Fehler, falls GitHub Actions abbrechen soll
})
