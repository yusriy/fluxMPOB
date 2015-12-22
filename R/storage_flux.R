####################################################################
# TITLE: Analysis of storage flux data
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

