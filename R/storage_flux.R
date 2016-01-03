####################################################################
# TITLE: Analysis of storage flux data for MPOB
# AUTHOR: Yusri Yusup, PhD
# AFFILIATION: Universiti Sains Malaysia
# DATE: 2015-12-22
#
# Note: 
#     1. Need to change the Excel file to csv file first
#
#
#
####################################################################

#### 1. Preliminaries ####
source('R/tool_trapezium_intg.R') # To calculate storage using the trapezium rule

#### 2. Data import ####

d_flux <- read.csv(file.choose(),skip = 1)
# Naming the 20 columns
# IMPORTANT NOTE: HAS TO BE 20 COLUMNS OR AN ERROR WOULD APPEAR
colnames(d_flux) <- c('year','month','day','hour','minute',
                      'co_2_strg','h_2O_strg', #conc. flux [u-mol m-2 s-1]
                      'H_strg', # sensible heat storage [W m-2]
                      'co2_1', # conc. [mmol m-3]
                      'co2_2', # conc. [mmol m-3]
                      'co2_3', # conc. [mmol m-3]
                      'co2_4', # conc. [mmol m-3]
                      'h2O_1', # conc. [mmol m-3]
                      'h2O_2', # conc. [mmol m-3]
                      'h2O_3', # conc. [mmol m-3]
                      'h2O_4', # conc. [mmol m-3]
                      'TA_1', # Temp [deg C]
                      'TA_2', # Temp [deg C]
                      'TA_3', # Temp [deg C]
                      'TA_4' # Temp [deg C]
                      )
# Format the time
date <- paste(d_flux$year,'-',d_flux$month,'-',d_flux$day,' ',
              d_flux$hour,':',d_flux$minute,':00',sep = '')
date <- strptime(date,'%Y-%m-%d %H:%M:%S')
# Remove the 'year', 'month', etc. columns
d_flux <- d_flux[,-c(1:5)]
d_flux <- cbind(date,d_flux)
rm(date)

#Levels 1: 2 m; 2: 5 m; 3: 10 m; 4: 15 m
# NOTE: Heights could change with location
heights <- c(2,5,10,15) # [m] 

#### 2. Analysis ####

#### 2.1 Calculate CO2 flux storage ####

# Calculating the difference of CO2,1 and CO2,2 in time
# Level 1, 2 m
co2_1 <- numeric()
for (i in 1:length(d_flux$co2_1)){
  co2_1[i] <- (d_flux$co2_1[i] - d_flux$co2_1[i-1])/(30 * 60)
}
# Level 2, 5 m
co2_2 <- numeric()
for (i in 1:length(d_flux$co2_2)){
  co2_2[i] <- (d_flux$co2_2[i] - d_flux$co2_2[i-1])/(30 * 60)
}
# Level 3, 10 m
co2_3 <- numeric()
for (i in 1:length(d_flux$co2_3)){
  co2_3[i] <- (d_flux$co2_3[i] - d_flux$co2_3[i-1])/(30 * 60)
}
# Level 4, 15 m
co2_4 <- numeric()
for (i in 1:length(d_flux$co2_4)){
  co2_4[i] <- (d_flux$co2_4[i] - d_flux$co2_4[i-1])/(30 * 60)
}
# Integrating using the trapezium area rule
co2_stor <- numeric() # CO2 storage flux [mmol CO2 m-2 s] 
for (i in 1:nrow(d_flux)){
  co2_stor[i] <- trapezium_intg(heights,co2_1[i],co2_2[i],co2_3[i],co2_4[i])
}
# Change from mmol to umol
co2_stor <- co2_stor * 1000 # [umol m-2 s-1]
# Combine with d_flux
d_flux <- cbind(d_flux,co2_stor)

#### 2.2 Calculate LE storage ####

# Calculating the difference of H2O,1 and H2O,2 in time
# Level 1, 2 m
h2o_1 <- numeric()
for (i in 1:length(d_flux$h2O_1)){
  h2o_1[i] <- (d_flux$h2O_1[i] - d_flux$h2O_1[i-1])/(30 * 60)
}
# Level 2, 5 m
h2o_2 <- numeric()
for (i in 1:length(d_flux$h2O_2)){
  h2o_2[i] <- (d_flux$h2O_2[i] - d_flux$h2O_2[i-1])/(30 * 60)
}
# Level 3, 10 m
h2o_3 <- numeric()
for (i in 1:length(d_flux$h2O_3)){
  h2o_3[i] <- (d_flux$h2O_3[i] - d_flux$h2O_3[i-1])/(30 * 60)
}
# Level 4, 15 m
h2o_4 <- numeric()
for (i in 1:length(d_flux$h2O_4)){
  h2o_4[i] <- (d_flux$h2O_4[i] - d_flux$h2O_4[i-1])/(30 * 60)
}
# Integrating using the trapezium area rule
LE_stor <- numeric() # CO2 storage flux [mmol CO2 m-2 s] 
for (i in 1:nrow(d_flux)){
  LE_stor[i] <- trapezium_intg(heights,h2o_1[i],h2o_2[i],h2o_3[i],h2o_4[i])
}
# Change from mmol to umol and multiply with latent heat of vaporization
# L_v = 2,540,000 J/kg = latent heat of vaporization
# MW = 18 kg mol = molar mass of H2O

LE_stor <- LE_stor * 18 * 2540000 / 1000 / 1000 # [W m-2]
# Combine with d_flux
d_flux <- cbind(d_flux,LE_stor)

#### 2.3 Calculate H storage ####

# Assumptions
# C_P = 1005 J/kg K = heat capacity of air at 20 C
# rho = 1.2754 kg m-3 = density of dry air at 20 C
rho = 1.2754
C_P = 1005

# Calculating the difference of H2O,1 and H2O,2 in time
# Level 1, 2 m
T_1 <- numeric()
for (i in 1:length(d_flux$TA_1)){
  T_1[i] <- (d_flux$TA_1[i] - d_flux$TA_1[i-1])/(30 * 60)
}
# Level 2, 5 m
T_2 <- numeric()
for (i in 1:length(d_flux$TA_2)){
  T_2[i] <- (d_flux$TA_2[i] - d_flux$TA_2[i-1])/(30 * 60)
}
# Level 3, 10 m
T_3 <- numeric()
for (i in 1:length(d_flux$TA_3)){
  T_3[i] <- (d_flux$TA_3[i] - d_flux$TA_3[i-1])/(30 * 60)
}
# Level 4, 15 m
T_4 <- numeric()
for (i in 1:length(d_flux$TA_4)){
  T_4[i] <- (d_flux$TA_4[i] - d_flux$TA_4[i-1])/(30 * 60)
}
# Integrating using the trapezium area rule
H_stor <- numeric() # CO2 storage flux [mmol CO2 m-2 s] 
for (i in 1:nrow(d_flux)){
  H_stor[i] <- trapezium_intg(heights,T_1[i],T_2[i],T_3[i],T_4[i])
}


H_stor <- rho * C_P * H_stor # [W m-2]
# Combine with d_flux
d_flux <- cbind(d_flux,H_stor)

#### 3. Delete all unused variables ####
rm(list = c(ls(pattern = c('c'))))
rm(list = c(ls(pattern = c('h'))))
rm(list = c(ls(pattern = c('T'))))
rm(C_P,H_stor,i,LE_stor)