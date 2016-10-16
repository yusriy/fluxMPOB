# This function to import and process Biomet data
#
# IMPORTANT:
# The name of date and time columns must be 'date' and 'time'
##############################
proc_datBio <- function(df_name) {
  
  df_biomet <- df_name

  # Changing all the '-9999.0' or '-9999' (missing data) to NA
  for (i in 3:length(df_biomet)) { # starts at index 2 to exclude time_stamp
    df_biomet[i][df_biomet[i] <= -9999] <- NA
    }
  
  # Configuring the date and time
  time_stamp <- paste(df_biomet$date,df_biomet$time)
  time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
  
  # Remove the 'date' and 'time' column
  df_biomet <- df_biomet[,c(-1,-2)]
  # Combine time_stamp with biomet data
  df_biomet <- cbind(time_stamp, df_biomet)
  return(df_biomet)
}
