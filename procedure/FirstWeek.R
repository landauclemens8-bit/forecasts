# First I strucktured my R project:
# with git to store my process and renv that my packeges are availeble everywher
# Folder to storage my data, with gitignore 
    # stplit into raw and processed data, that I can easily update my data 
    # the ffm data set can be updatet by running a skript, the SMARD has to be downloaded again 
    # next step was to manage the time-zone issue (time shift)

# then i did my first SMARD forecat with the Benchmark of the last 20 fridays at 12 
    # for both demand and re_demand 

# then i downloaded via open-meteo API the forcast benchmark for temp and wind (ECMWF IFS 0.25°) Frankfurt airport
  # then I converted the 51 ensemble members to the target quantiles by:
  # formating the time, filtering the target days/hours, computing the quantiles 