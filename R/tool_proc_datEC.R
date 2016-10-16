# This function to import and process EC data
#
#
# IMPORTANT:
# The name of date and time columns must be 'date' and 'time'
#########################################
proc_datEC <- function(df_name){
  # Insert insert dataframe into dummy dataframe
  df <- df_name
  
  # Removing the first column and first row
  df <- df[,-1]
  
  # Changing all the '-9999.0' or '-9999' (missing data) to NA
  for (i in 3:length(df)){
    df[i][df[i] <= -9999] <- NA
  }
  
  # Formatting time
  time_stamp <- paste(df$date,df$time)
  
  # Might need to change format of date 1/1/2014 or 2014-1-1
  time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
  df$time <- time_stamp
  df <- df[,c(-1)]
  colnames(df)[1] <-'time_stamp'
  
  # Changing all relevant columns to factors
  #df$daytime <- as.factor(df$daytime)
  #df$file_records <- as.factor(df$file_records)
  #df$used_records <- as.factor(df$used_records)
  #df$qc_Tau <- as.factor(df$qc_Tau)
  #df$qc_H <- as.factor(df$qc_H)
  #df$qc_LE <- as.factor(df$qc_LE)
  #df$qc_co2_flux <- as.factor(df$qc_co2_flux)
  #df$qc_h2o_flux <- as.factor(df$qc_h2o_flux)
  #df$co2_def_timelag <- as.factor(df$co2_time_lag)
  #df$h2o_def_timelag <- as.factor(df$h2o_def_timelag)
  #df$spikes <- as.factor(df$spikes)
  #df$amplitude_resolution <- as.factor(df$amplitude_resolution)
  #df$drop_out <- as.factor(df$drop_out)
  #df$absolute_limits <- as.factor(df$absolute_limits)
  #df$skweness_kurtosis <- as.factor(df$skweness_kurtosis)
  #df$skweness_kurtosis.1 <- as.factor(df$skweness_kurtosis.1)
  #df$discontinuities <- as.factor(df$discontinuities)
  #df$discontinuities.1 <- as.factor(df$discontinuities.1)
  #df$timelag <- as.factor(df$timelag)
  #df$timelag.1 <- as.factor(df$timelag.1)
  #df$attack_angle <- as.factor(df$attack_angle)
  #df$non_steady_wind <- as.factor(df$non_steady_wind)
  #df$model <- as.factor(df$model)
  
  # Change column name of (z-d)/L to Z.L
  colnames(df)[which(colnames(df) == 'X.z.d..L')] <- 'Z.L'
  
  return(df)
  
}


