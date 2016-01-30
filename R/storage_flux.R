#### TITLE: Import and analysis of storage flux data for MPOB #######
# AUTHOR: Yusri Yusup, PhD
# AFFILIATION: Universiti Sains Malaysia
# DATE: 2015-12-22
#
# Note: 
#     1. Will be using ports 5-8 for CO2 storage calculations; ports
#     1-4 are for the soil chambers
#     2. Cdry_Mean is in [ppm] (by mol)
#     3. Lin_Flux or Exp_Flux is the flux from soil [umol m-2 s-1]
#     4. A last column "X" might be created but can be ignored.
#     5. Need to prepare the profiler and soil chamber data using SoilFluxPro
# Export only:  Label, Port#, #Msgs, ObsDateTime, Obs#, Exp_Flux, Lin_Flux,
#               CrvFitStatus, Exp_R2, Lin_R2, Cdry_Mean

#### 1. Preliminaries ####
library(openair) # To average profiler data using timeAverage
source('R/tool_trapezium_intg_4.R') # To calculate storage using the trapezium rule
                                    # but only for 4 heights

#### 2. Data import ####

df_CO2_strg <- read.csv(file.choose(),sep='\t')

# Format the time
time_stamp <- strptime(df_CO2_strg$ObsDateTime,'%Y-%m-%d %H:%M:%S')
# Change to only half-hourly values (every 30 min)
time_stamp$min <- round(time_stamp$min / 30) * 30
time_stamp$sec <- round(time_stamp$sec/60) * 0

# Remove the first column ObsDateTime
df_CO2_strg <- df_CO2_strg[,-1]
df_CO2_strg <- cbind(time_stamp,df_CO2_strg)
rm(time_stamp)

#Levels 1: 2 m; 2: 5 m; 3: 10 m; 4: 15 m
# NOTE: Heights could change with location
heights <- c(2,5,15,30.65) # [m] 

#### 2. Some parameters calculations ###########

#### 2.1 Convert CO2 ppm to umol m-3 ###########

# Formula: CO2 [umol m-3] = CO2 [ppm] * molar density of air [mol m-3] / 10^6
# molar density of air = 44.6 mol m-3
df_CO2_strg$Cdry_Mean <- df_CO2_strg$Cdry_Mean * 44.6 / 10^6

#### 2.2 Separate data according to ports ######
# Port 1 Harvest path - under palm
port1 <- subset(df_CO2_strg, Port. == 1)
# Change to local time
port1$time_stamp <- format(port1$time_stamp,tz='Singapore')
# Port 2 Harvest path - open
port2 <- subset(df_CO2_strg, Port. == 2)
# Change to local time
port2$time_stamp <- format(port2$time_stamp,tz='Singapore')
# Port 3 Frond pile - north
port3 <- subset(df_CO2_strg, Port. == 3)
# Change to local time
port3$time_stamp <- format(port3$time_stamp,tz='Singapore')
# Port 4 Frond pile - south
port4 <- subset(df_CO2_strg, Port. == 4)
# Change to local time
port4$time_stamp <- format(port4$time_stamp,tz='Singapore')

# Port 5 Profile1 - 30.65 m
port5 <- subset(df_CO2_strg, Port. == 5)
colnames(port5)[1] <- 'date' # For the timeAverage function
# Port 6 Profile2 - 15 m
port6 <- subset(df_CO2_strg, Port. == 6)
colnames(port6)[1] <- 'date' # For the timeAverage function
# Port 7 Profile3 - 10 m
port7 <- subset(df_CO2_strg, Port. == 7)
colnames(port7)[1] <- 'date' # For the timeAverage function
# Port 8 Profile4 - 2 m
port8 <- subset(df_CO2_strg, Port. == 8)
colnames(port8)[1] <- 'date' # For the timeAverage function

# Use timeAverage function from the openair package to average Cdry_Mean
port5_t <- timeAverage(port5,avg.time = '30 min')
# Convert to local time zone
port5_t$date <- format(port5_t$date,tz='Singapore')
colnames(port5_t)[1] <- 'time_stamp'
# Have to change back to dataframe from list
port5_t <- as.data.frame(port5_t)

# Use timeAverage function from the openair package to average Cdry_Mean
port6_t <- timeAverage(port6,avg.time = '30 min')
# Convert to local time zone
port6_t$date <- format(port6_t$date,tz='Singapore')
colnames(port6_t)[1] <- 'time_stamp'
# Have to change back to dataframe from list
port6_t <- as.data.frame(port6_t)

# Use timeAverage function from the openair package to average Cdry_Mean
port7_t <- timeAverage(port7,avg.time = '30 min')
# Convert to local time zone
port7_t$date <- format(port7_t$date,tz='Singapore')
colnames(port7_t)[1] <- 'time_stamp'
# Have to change back to dataframe from list
port7_t <- as.data.frame(port7_t)

# Use timeAverage function from the openair package to average Cdry_Mean
port8_t <- timeAverage(port8,avg.time = '30 min')
# Convert to local time zone
port8_t$date <- format(port8_t$date,tz='Singapore')
colnames(port8_t)[1] <- 'time_stamp'
# Have to change back to dataframe from list
port8_t <- as.data.frame(port8_t)

# Delete temporary variables
rm(port5,port6,port7,port8)

#### 2.3 Merge dataframes by date ##############

# Merging the soil chamber data
df <- merge(port1,port2,by='time_stamp',all = TRUE)
# Remove unnecessary columns
df <- df[,-c(2,3,12,13,14,23)]
# Rename the headers
colnames(df) <- c('time_stamp','obs1','msg1','expf1','expR2_1','linf1','linR2_1',
                  'crv1','Cdry1',
                  'obs2','msg2','expf2','expR2_2','linf2','linR2_2','crv2','Cdry2')
df <- merge(df,port3,by='time_stamp',all=TRUE)
# Remove unnecessary columns
df <- df[,-c(18,19,28)]
# Rename the headers
colnames(df)[18:25] <- c('obs3','msg3','expf3','expR2_3','linf3','linR2_3','crv3',
                         'Cdry3')

df <- merge(df,port4,by='time_stamp',all=TRUE)
# Remove unnecessary columns
df <- df[,-c(26,27,36)]
# Rename the headers
colnames(df)[26:33] <- c('obs4','msg4','expf4','expR2_4','linf4','linR2_4','crv4',
                         'Cdry4')

# Merging the profiler data ports 5 and 6
df2 <- merge(port5_t,port6_t,by='time_stamp',all=TRUE)
# Remove unnecessary columns
df2 <- df2[,-c(2,5,6,7,8,10,13,14,15,16)]
# Rename the headers
colnames(df2)[2:7] <- c('obs5','msg5','Cdry5','obs6','msg6','Cdry6')

# Merging the profiler data ports 7 and 8
df3 <- merge(port7_t,port8_t,by='time_stamp',all=TRUE)
# Remove unnecessary columns
df3 <- df3[,-c(2,5,6,7,8,10,13,14,15,16)]
# Rename the headers
colnames(df3)[2:7] <- c('obs7','msg7','Cdry7','obs8','msg8','Cdry8')

# Merge both profiler data into one
df4 <- merge(df2,df3,by='time_stamp',all=TRUE)

# Merge with soil chamber data
df_pro <- merge(df,df4,by='time_stamp',all=TRUE)

# Change to POSIX again (?)
df_pro$time_stamp <- strptime(df_pro$time_stamp,'%Y-%m-%d %H:%M:%S')

#### 2.4 Calculate CO2 flux storage ############
# Calculating the difference of CO2,1 and CO2,2 in time
# Level 1, 2 m
co2_1 <- numeric()
for (i in 1:length(df_pro$Cdry8)){
  co2_1[i] <- (df_pro$Cdry8[i] - df_pro$Cdry8[i-1])/(30 * 60)
}
# Level 2, 10 m
co2_2 <- numeric()
for (i in 1:length(df_pro$Cdry7)){
  co2_2[i] <- (df_pro$Cdry7[i] - df_pro$Cdry7[i-1])/(30 * 60)
}
# Level 3, 15 m
co2_3 <- numeric()
for (i in 1:length(df_pro$Cdry6)){
  co2_3[i] <- (df_pro$Cdry6[i] - df_pro$Cdry6[i-1])/(30 * 60)
}
# Level 4, 15 m
co2_4 <- numeric()
for (i in 1:length(df_pro$Cdry5)){
  co2_4[i] <- (df_pro$Cdry5[i] - df_pro$Cdry5[i-1])/(30 * 60)
}
# Integrating using the trapezium area rule
co2_stor <- numeric() # CO2 storage flux [umol CO2 m-2 s] 
for (i in 1:nrow(df_pro)){
  co2_stor[i] <- trapezium_intg_4(heights,co2_1[i],co2_2[i],co2_3[i],co2_4[i])
}

# Convert from mol to umol
co2_stor <- co2_stor * 1000000

# Combine with df_pro
df_pro <- cbind(df_pro,co2_stor)

#### 3. Delete all unused variables ####
rm(df,df2,df3,df4,df_CO2_strg,i,heights)
rm(list = c(ls(pattern = c('co'))))
rm(list = c(ls(pattern = c('po'))))
