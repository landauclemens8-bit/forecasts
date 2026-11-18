# Week 3

# habe ein K-NN neerest neighbors modell gemacht, mit der möglichkeit verschidene 
# features mit hinein zu nehmne, wie z.B. WS10, Day, Month, hour 
# und habe von den k = 50 NN die zugehörigen 36, 40, ... usw horizonte genommen 
# und daraus meine verteilung gemacht und quantile berechnet 
# als features habe ich T2M, WS10, month, day und hour genommen 

# zum abschluss habe ich noch den mittelwert von den berechneten quantilen und 
# denen des ensamble forecast ISF 0.25 genommen 

# für die nächste woche will ich ein wenig backtesten, welche features ich am 
# besten mit rein nehmen sollte
# außerdem will ich schauen wie sich dieses modell in calibration
# und sparpness schlägt