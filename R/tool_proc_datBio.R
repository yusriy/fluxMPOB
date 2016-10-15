# This function to import and process Biomet data

##############################
proc_datBio <- function(df_name, h_name) {
  
  df_biomet <- df_name
  
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
}
