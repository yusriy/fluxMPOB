####################################################################
# TITLE: Analysis of soil chamber flux data for MPOB
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
soil_chamber <- read.csv(file.choose(),header = TRUE)

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

## 2. Port - need to changer to factor
soil_chamber$Port. <- as.factor(soil_chamber$Port.)

## 3. Date - need to change to POSIXct - Singapore Time (SGT)
soil_chamber$ObsDateTime <- strptime(soil_chamber$ObsDateTime,
                                     "%m/%d/%y %H:%M")

## 4. CO2 soil flux estimated using exponential fit [umol m-2 s-1]
# Already in numeric
# Need to remove 0 values from samplers other than soil chamber
soil_chamber$Exp_Flux[which(soil_chamber$Exp_Flux == 0.00)] <- NA
## 5. CO2 soil flux estimated using linear fit [umol m-2 s-1]
# Already in numeric
# Need to remove 0 values from samplers other than soil chamber
soil_chamber$Lin_Flux[which(soil_chamber$Lin_Flux == 0.00)] <- NA

## 6. Curve fit status, either linear or exponential
# Already in factor but need to exchange the " " with NA
soil_chamber$CrvFitStatus[which(soil_chamber$CrvFitStatus == "")] <- NA

## 7. Port#, no need to use since the first "Port" is the same as this one

## 8. MeanRH [%]
# Need to change from "factor" to "numeric"
# A lot of missing/improbable values, e.g., >100% and "??"
# Changing "??" to NA first
soil_chamber$Mean.RH[which(soil_chamber$Mean.RH ==  "??")] <- NA
# Changing to character then to numeric
soil_chamber$Mean.RH <- as.character(soil_chamber$Mean.RH)
soil_chamber$Mean.RH <- as.numeric(soil_chamber$Mean.RH)
# Remove improbable values
soil_chamber$Mean.RH[which(soil_chamber$Mean.RH > 100)] <- NA

## 9. Mean.Tcham [deg C]
# Already in "numeric"
# But a lot of improbable values T > 100 C or T < 0 C
soil_chamber$Mean.Tcham[which(soil_chamber$Mean.Tcham > 100 |
                                soil_chamber$Mean.Tcham < 0)] <- NA

## 10. Observation.Length [min] -> [s]
# Change from "factor" to "numeric"
soil_chamber$Observation.Length <- 
  as.character(soil_chamber$Observation.Length)
# Splitting the minute from the seconds, e.g., 1:00 to 1
temp <- as.numeric(nrow(soil_chamber))
for (i in 1:nrow(soil_chamber)){
  temp[i] <- strsplit(soil_chamber$Observation.Length,":")[[i]][1]
}
rm(i)

# Changing from "char" to "numeric"
temp <- as.numeric(temp)
# Adding to soil_chamber in [s]
soil_chamber$Observation.Length <- temp * 60
rm(temp)

## 11. Tmux temperature of the multiplexer [deg C]
# Already in numeric

## 12. Vcham soil chamber volume [cm3]
# Already in "numeric"
# Need to change from "0" to NA
soil_chamber$Vcham[soil_chamber$Vcham == 0] <- NA

## 13. Area chamber area [cm2]
# Already in "numeric"
# Need to change from "0" to NA
soil_chamber$Area[soil_chamber$Area == 0] <- NA

## 14. Vtotal total volume sampled [cm3]
# Already in "numeric"
# Lower volumes than 5000 cm3 is not from the chambers

## IV.V2 - soil temperature [deg C]

# Change name from IV.V2 to "soil_temp"
colnames(soil_chamber)[which(colnames(soil_chamber) 
                             == "IV.V2")] <- "soil_temp"
# Remove "??"
soil_chamber$soil_temp[which(soil_chamber$soil_temp == "??")] <- NA
# Change from factor to char to numeric
soil_chamber$soil_temp <- as.character(soil_chamber$soil_temp)
soil_chamber$soil_temp <- as.numeric(soil_chamber$soil_temp)
