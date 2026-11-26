# Versuche die KNN methode etwas backtesting 

# da ich immer dienstag um 23 uhr meine Forecasts mache habe ich ir erst dei dienstage der 
# vergangenen 2 Jahre angeschaut

# habe dazu dei PIT histogramme geplotted und nach der calibrierung geschaut
# wenn man nur die letzten zwei jahre betrachtete ist bei dem PIT histogramm 
# der balken der PIT values > 0.9 deutlich zu hoch:
# das heißt, KNN prognostiziert tendenziell zu niedrige Temperaturen.

# um so mehr jahre ich zurück gehe, bzw mit einbeziehe um so kalibrierter wird das histogramm
# es ist zwar tendenziell immernoch etwas zu niedrig aber es wir stetig besser 