##########################################################
# TITLE: Import and analyses of EC and Biomet data
# AUTHOR: Yusri Yusup, PhD
# AFFILIATION: Universiti Sains Malaysia
# DATE: 2016-01-04
# 
# Note: 
#     1. For EC 'full output' results only and the data
#     must be in CSV format
#     2. When prompted, import EC data first then Biomet data
####################################################################

##### 0. Preliminaries ####################################
## Initial analyses setup

# Loading packages
library(Hmisc)
library(plyr)

# Sourcing custom functions
source('R/tool_convert_magic.R')
source('R/tool_convert_magic_num.R')
source('R/tool_proc_dat2.R')
source('R/tool_charactersNumeric.R')
source('R/tool_asNumeric.R')
source('R/tool_rho_cp.R') # To calculate rho_cp for H storage
source('R/tool_trapezium_intg.R') # To calculate storage using the trapezium rule
source('R/tool_abs_humidity.R') # To calculate abs humidity from RH for LE storage

##### 1. Data import #####################################################

# Import EC data
# Type of data = 'full output'
df_EC <- read.csv(file.choose(),header = TRUE,skip = 1)

# Import Biomet data
df_biomet <- read.csv(file.choose(), header = TRUE)
# Remove the first row
df_biomet <- df_biomet[-1,]
# Using convert_magic to convert all columns to 'character' first
df_biomet <- convert_magic(df_biomet[,c(seq(1,ncol(df_biomet)))],
                           c(rep('character',times = ncol(df_biomet))))
# Changing all the '-9999.0' or '-9999' (missing data) to NA
for (i in 3:length(df_biomet)){ # starts at index 2 to exclude time_stamp
  df_biomet[i][df_biomet[i] == '-9999' | df_biomet[i] == '-9999.0'] <- NA
}
rm(i)
# Configuring the date and time
time_stamp <- paste(df_biomet$date,df_biomet$time)
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
# Change all non-factors (or characters) to numeric)
df_biomet <- charactersNumeric(df_biomet)

# Remove the 'date' and 'time' column
df_biomet <- df_biomet[,c(-1,-2)]
# Combine time_stamp with biomet data
df_biomet <- cbind(time_stamp, df_biomet)

# Define the header names of the dataframes
h_names <- c("time_stamp",
             "daytime",
             "file_records",
             "used_records",
             "Tau",
             "qc_Tau",
             "H",
             "qc_H",
             "LE",
             "qc_LE",
             "co2_flux",
             "qc_co2_flux",
             "h2o_flux",
             "qc_h2o_flux",
             "H_strg",
             "LE_strg",
             "co2_strg",
             "h2o_strg",
             "co2_v.adv",
             "h2o_v.adv",
             "co2_molar_density",
             "co2_mole_fraction",
             "co2_mixing_ratio",
             "co2_time_lag",
             "co2_def_timelag",
             "h2o_molar_density",
             "h2o_mole_fraction",
             "h2o_mixing_ratio",
             "h2o_time_lag",
             "h2o_def_timelag",
             "sonic_temperature",
             "air_temperature",
             "air_pressure",
             "air_density",
             "air_heat_capacity",
             "air_molar_volume",
             "water_vapor_density",
             "e",
             "es",
             "specific_humidity",
             "RH",
             "VPD",
             "Tdew",
             "u_unrot",
             "v_unrot",
             "w_unrot",
             "u_rot",
             "v_rot",
             "w_rot",
             "wind_speed",
             "max_wind_speed",
             "wind_dir",
             "yaw",
             "pitch",
             "roll",
             "u.",
             "TKE",
             "L",
             "Z.L",
             "bowen_ratio",
             "T.",
             "model",
             "x_peak",
             "x_offset",
             "x_10.",
             "x_30.",
             "x_50.",
             "x_70.",
             "x_90.",
             "un_Ta",
             "Tau_scf",
             "un_H",
             "H_scf",
             "un_LE",
             "LE_scf",
             "un_co2_flux",
             "co2_scf",
             "un_h2o_flux",
             "h2o_scf",
             "spikes",
             "amplitude_resolution",
             "drop_out",
             "absolute_limits",
             "skweness_kurtosis",
             "skweness_kurtosis.1",
             "discontinuities",
             "discontinuities.1",
             "timelag",
             "timelag.1",
             "attack_angle",
             "non_steady_wind",
             "u_spikes",
             "v_spikes",
             "w_spikes",
             "ts_spikes",
             "co2_spikes",
             "h2o_spikes",
             "chopper",
             "detector",
             "pll",
             "sync",
             "mean_value",
             "u_var",
             "v_var",
             "w_var",
             "ts_var",
             "co2_var",
             "h2o_var",
             "w.ts_cov",
             "w.co2_cov",
             "w.h2o_cov",
             "co2_mean",
             "h2o_mean",
             "dew.point_mean",
             "co2_signal_strength_7500_mean")

# Use the proc_dat2 function to process the data
df_EC <- proc_dat2(df_EC,h_names)

# Combine EC and biomet data. Must have the same number of rows
df_EC <- cbind(df_EC,df_biomet)

#### Convert Biomet temperature to C from K ##########################

# Air temperature
df_EC$Ta_1_1_1 <- df_EC$Ta_1_1_1 - 273.15
df_EC$Ta_2_1_1 <- df_EC$Ta_2_1_1 - 273.15
df_EC$Ta_3_1_1 <- df_EC$Ta_3_1_1 - 273.15
df_EC$Ta_4_1_1 <- df_EC$Ta_4_1_1 - 273.15
df_EC$Ta_5_1_1 <- df_EC$Ta_5_1_1 - 273.15
# Soil temperature
df_EC$Ts_1_1_1 <- df_EC$Ts_1_1_1 - 273.15
df_EC$Ts_2_1_1 <- df_EC$Ts_2_1_1 - 273.15
df_EC$Ts_3_1_1 <- df_EC$Ts_3_1_1 - 273.15


#### Calculating storage H in canopy #################################

# Note: 5 heights are used here unlike in James' (LI-COR) script which uses
# only 4 heights
heights <- c(2,5,10,15,30.65) #Levels 1: 2 m, 2: 5 m, 3: 10 m, 4: 15 m, 5: 30.65 m


# Calculating rho * cp for each level
rhocp1 <- rho_cp(df_EC$RH_1_1_1,df_EC$Ta_1_1_1,df_EC$air_pressure)
rhocp2 <- rho_cp(df_EC$RH_2_1_1,df_EC$Ta_2_1_1,df_EC$air_pressure)
rhocp3 <- rho_cp(df_EC$RH_3_1_1,df_EC$Ta_3_1_1,df_EC$air_pressure)
rhocp4 <- rho_cp(df_EC$RH_4_1_1,df_EC$Ta_4_1_1,df_EC$air_pressure)
rhocp5 <- rho_cp(df_EC$RH_5_1_1,df_EC$Ta_5_1_1,df_EC$air_pressure)

# Calculating the difference of rho * c_p * (T2 - T1) in time
# Level 1, 2 m
rho_cp_dT1 <- numeric()
for (i in 1:length(rhocp1)){
  rho_cp_dT1[i] <- ((rhocp1[i]*df_EC$Ta_1_1_1[i]) - 
                      (rhocp1[i-1]*df_EC$Ta_1_1_1[i-1]))/(30 * 60)
}
# Level 2, 5 m
rho_cp_dT2 <- numeric()
for (i in 1:length(rhocp2)){
  rho_cp_dT2[i] <- ((rhocp2[i]*df_EC$Ta_2_1_1[i]) - 
                      (rhocp2[i-1]*df_EC$Ta_2_1_1[i-1]))/(30 * 60)
}

# Level 3, 10 m
rho_cp_dT3 <- numeric()
for (i in 1:length(rhocp3)){
  rho_cp_dT3[i] <- ((rhocp3[i]*df_EC$Ta_3_1_1[i]) - 
                      (rhocp3[i-1]*df_EC$Ta_3_1_1[i-1]))/(30 * 60)
}

# Level 4, 15 m
rho_cp_dT4 <- numeric()
for (i in 1:length(rhocp4)){
  rho_cp_dT4[i] <- ((rhocp4[i]*df_EC$Ta_4_1_1[i]) - 
                      (rhocp4[i-1]*df_EC$Ta_4_1_1[i-1]))/(30 * 60)
}

# Level 5, 30.65 or 30 m
rho_cp_dT5 <- numeric()
for (i in 1:length(rhocp5)){
  rho_cp_dT5[i] <- ((rhocp5[i]*df_EC$Ta_5_1_1[i]) - 
                      (rhocp5[i-1]*df_EC$Ta_5_1_1[i-1]))/(30 * 60)
}

# Integrating using the trapezium area rule
H_stor <- numeric()
for (i in 1:nrow(df_EC)){
  H_stor[i] <- trapezium_intg(heights,rho_cp_dT1[i],rho_cp_dT2[i],rho_cp_dT3[i],
                              rho_cp_dT4[i],rho_cp_dT5[i])
}

# Adding to df_EC
df_EC <- cbind(df_EC,H_stor)

rm(rho_cp_dT1,rho_cp_dT2,rho_cp_dT3,rho_cp_dT4,
   rho_cp_dT5,rhocp1,rhocp2,rhocp3,
   rhocp4,rhocp5,H_stor)

#### Calculating storage LE in canopy ####
# Storage calculations are based on the Finnigan (2006) paper
# Calculating absolute humidity from RH
# Level 1, 2 m
hum1 <- numeric()
for (i in 1:nrow(df_EC)){
  hum1[i] <- abs_humidity(df_EC$Ta_1_1_1[i],df_EC$RH_1_1_1[i])
}
# Level 2, 5 m
hum2 <- numeric()
for (i in 1:nrow(df_EC)){
  hum2[i] <- abs_humidity(df_EC$Ta_2_1_1[i],df_EC$RH_2_1_1[i])
}
# Level 3, 10 m
hum3 <- numeric()
for (i in 1:nrow(df_EC)){
  hum3[i] <- abs_humidity(df_EC$Ta_3_1_1[i],df_EC$RH_3_1_1[i])
}
# Level 4, 15 m
hum4 <- numeric()
for (i in 1:nrow(df_EC)){
  hum4[i] <- abs_humidity(df_EC$Ta_4_1_1[i],df_EC$RH_4_1_1[i])
}
# Level 5, 30.65 m
hum5 <- numeric()
for (i in 1:nrow(df_EC)){
  hum5[i] <- abs_humidity(df_EC$Ta_5_1_1[i],df_EC$RH_5_1_1[i])
}
# Adding to df_EC
df_EC <- cbind(df_EC,hum1,hum2,hum3,hum4,hum5)
rm(hum1,hum2,hum3,hum4,hum5)

# Calculating storage LE in canopy
L_v = 2540000 # [J/kg]

# Level 1, 2 m
diff_hum1 <- numeric()
#diff_hum1[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_EC$hum1)){
  diff_hum1[i] <- L_v * (df_EC$hum1[i] - df_EC$hum1[i-1])/(30 * 60)
}

# Level 2, 5 m
diff_hum2 <- numeric()
#diff_hum1[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_EC$hum2)){
  diff_hum2[i] <- L_v * (df_EC$hum2[i] - df_EC$hum2[i-1])/(30 * 60)
}

# Level 3, 10 m
diff_hum3 <- numeric()
#diff_hum1[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_EC$hum3)){
  diff_hum3[i] <- L_v * (df_EC$hum3[i] - df_EC$hum3[i-1])/(30 * 60)
}

# Level 4, 15 m
diff_hum4 <- numeric()
#diff_hum1[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_EC$hum4)){
  diff_hum4[i] <- L_v * (df_EC$hum4[i] - df_EC$hum4[i-1])/(30 * 60)
}

# Level 5, 30.65 or 30 m
diff_hum5 <- numeric()
#diff_hum1[1] <- NA # The first one should be NA because there is no data
# before index 1
for (i in 1:length(df_EC$hum5)){
  diff_hum5[i] <- L_v * (df_EC$hum5[i] - df_EC$hum5[i-1])/(30 * 60)
}

# Integrating using the trapezium area rule
LE_stor <- numeric()
for (i in 1:nrow(df_EC)){
  LE_stor[i] <- trapezium_intg(heights,diff_hum1[i],diff_hum2[i],diff_hum3[i],
                               diff_hum4[i],diff_hum5[i])
}

# Adding to df_EC
df_EC <- cbind(df_EC,LE_stor)
rm(LE_stor,L_v,diff_hum1,diff_hum2,diff_hum3,diff_hum4,diff_hum5,heights,i)

#### Exploratory plots ###################################

# Plot CO2 flux with time stamp
plot(df_EC$time_stamp,df_EC$co2_flux,pch=19)
# Overlay QC=2 (bad quality) on top of plot
points(df_EC$time_stamp[which(df_EC$qc_co2_flux==2)],
       df_EC$co2_flux[which(df_EC$qc_co2_flux==2)],pch=19,col='red')

# Plot LE (latent heat flux) with time stamp
plot(df_EC$time_stamp,df_EC$LE,pch=19)
# Overlay QC=2 (bad quality) on top of plot
points(df_EC$time_stamp[which(df_EC$qc_LE==2)],
       df_EC$LE[which(df_EC$qc_LE==2)],pch=19,col='red')

# Plot H (sensible heat flux) with time stamp
plot(df_EC$time_stamp,df_EC$H,pch=19)
# Overlay QC=2 (bad quality) on top of plot
points(df_EC$time_stamp[which(df_EC$qc_H==2)],
       df_EC$H[which(df_EC$qc_H==2)],pch=19,col='red')


##### Export data ########################################
write.table(df_EC,'df_EC.csv',sep=',')

##### Cleaning up ########################################
# Remove temporary variables
rm(h_names,time_stamp,df_biomet)

