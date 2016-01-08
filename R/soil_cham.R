####################################################################
# TITLE: Import and analysis of soil chamber flux data for MPOB
# AUTHOR: Yusri Yusup, PhD
# AFFILIATION: Universiti Sains Malaysia
# DATE: 2016-01-03
#
# Note: 
#     1. Need to change the Excel file to csv file first
#
#
#
####################################################################

#### 1. Preliminaries ####

#### 2. Import the data ####
# Note: need to convert from Excel to CSV
df_soil <- read.csv(file.choose(),header = TRUE)

# Reorganizing the data to the correct data type

## 1. Label is alraeady a factor
# Label - the description of the sampling point
# 1. Frond pile - north - Soil flux chamber sampling method - PORT 3
# 2. Frond pile - south - Soil flux chamber sampling method - PORT 4
# 3. Harvest path - open - Soil flux chamber sampling method - PORT 2
# 4. Harvest path - under palm - Soil flux chamber sampling method - PORT 1
# 5. Profile1 - 30.65 m (height above ground); not used - PORT 5
# 6. Profile2 - 15 m (height above ground); not used - PORT 6
# 7. Profile3 - 10 m (height above ground); not used - PORT 7
# 8. Profile4 - 2 m (height above ground); not used - PORT 8

## 2. Port - need to change to factor
df_soil$Port. <- as.factor(df_soil$Port.)

## 3. Date - need to change to POSIXct - Singapore Time (SGT)
df_soil$ObsDateTime <- strptime(df_soil$ObsDateTime,
                                     "%m/%d/%y %H:%M")

## 4. CO2 soil flux estimated using exponential fit [umol m-2 s-1]
# Already in numeric
# Need to remove 0 values from samplers other than soil chamber
df_soil$Exp_Flux[which(df_soil$Exp_Flux == 0.00)] <- NA
## 5. CO2 soil flux estimated using linear fit [umol m-2 s-1]
# Already in numeric
# Need to remove 0 values from samplers other than soil chamber
df_soil$Lin_Flux[which(df_soil$Lin_Flux == 0.00)] <- NA

## 6. Curve fit status, either linear or exponential
# Already in factor but need to exchange the " " with NA
df_soil$CrvFitStatus[which(df_soil$CrvFitStatus == "")] <- NA

## 7. Port#, no need to use since the first "Port" is the same as this one

## 8. MeanRH [%]
# Need to change from "factor" to "numeric"
# A lot of missing/improbable values, e.g., >100% and "??"
# Changing "??" to NA first
df_soil$Mean.RH[which(df_soil$Mean.RH ==  "??")] <- NA
# Changing to character then to numeric
df_soil$Mean.RH <- as.character(df_soil$Mean.RH)
df_soil$Mean.RH <- as.numeric(df_soil$Mean.RH)
# Remove improbable values
df_soil$Mean.RH[which(df_soil$Mean.RH > 100)] <- NA

## 9. Mean.Tcham [deg C]
# Already in "numeric"
# But a lot of improbable values T > 100 C or T < 0 C
df_soil$Mean.Tcham[which(df_soil$Mean.Tcham > 100 |
                                df_soil$Mean.Tcham < 0)] <- NA

## 10. Observation.Length [min] -> [s]
# Change from "factor" to "numeric"
df_soil$Observation.Length <- 
  as.character(df_soil$Observation.Length)
# Splitting the minute from the seconds, e.g., 1:00 to 1
temp <- as.numeric(nrow(df_soil))
for (i in 1:nrow(df_soil)){
  temp[i] <- strsplit(df_soil$Observation.Length,":")[[i]][1]
}
rm(i)

# Changing from "char" to "numeric"
temp <- as.numeric(temp)
# Adding to df_soil in [s]
df_soil$Observation.Length <- temp * 60
rm(temp)

## 11. Tmux temperature of the multiplexer [deg C]
# Already in numeric

## 12. Vcham soil chamber volume [cm3]
# Already in "numeric"
# Need to change from "0" to NA
df_soil$Vcham[df_soil$Vcham == 0] <- NA

## 13. Area chamber area [cm2]
# Already in "numeric"
# Need to change from "0" to NA
df_soil$Area[df_soil$Area == 0] <- NA

## 14. Vtotal total volume sampled [cm3]
# Already in "numeric"
# Lower volumes than 5000 cm3 is not from the chambers

## IV.V2 - soil temperature [deg C]

# Change name from IV.V2 to "soil_temp"
colnames(df_soil)[which(colnames(df_soil) 
                             == "IV.V2")] <- "soil_temp"
# Remove "??"
df_soil$soil_temp[which(df_soil$soil_temp == "??")] <- NA
# Change from factor to char to numeric
df_soil$soil_temp <- as.character(df_soil$soil_temp)
df_soil$soil_temp <- as.numeric(df_soil$soil_temp)
