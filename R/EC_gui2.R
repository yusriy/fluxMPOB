
#### Function to create a GUI to import EC and Biomet data ####
# Author: Yusri Yusup, PhD
# Date created: 2016-10-15
# 
#
importData <- function(){
  library(gWidgets)
  options(guiToolkit = "tcltk")
  
  # Create a window
  win <- gwindow("Import and Process Data in R", visible = FALSE)
  required_label <- glabel('* Required', container = win)
  # Create a subsection in window for EC
  csv.frame_ec <- gframe("Import EC data", horizontal = FALSE,
                         container = win)
  # Check box for file details EC
  check_ec <- gcheckbox('* Tick if header is present', 
                        container = csv.frame_ec)
  num_lin_skip_ec_label <- glabel('* Which rows to skip (must be integer), separate values by comma', 
                                  container = csv.frame_ec)
  num_lin_skip_ec <- gedit('1,3', container = csv.frame_ec)
  
  name_ec_label <- glabel('* Name of EC data frame', 
                          container = csv.frame_ec)
  # Name of EC dataframe
  name_ec <- gedit('df_ec', container = csv.frame_ec)
  
  # Browse for EC data
  ec_label <- glabel('* Eddy covariance data', container = csv.frame_ec)
  dest_ec <- gfilebrowse("Upload EC csv file", container = csv.frame_ec)
  
  # Create a subsection in window for Biomet
  csv.frame_bio   <- gframe("Import Biomet data", container = win,
                            horizontal = FALSE)
  # Check box for file details EC
  check_bio <- gcheckbox('* Tick if header is present', 
                         container = csv.frame_bio)
  num_lin_skip_bio_label <- glabel('* Which rows to skip (must be integer), separate values by comma', 
                                   container = csv.frame_bio)
  num_lin_skip_bio <- gedit('2', container = csv.frame_bio)
  
  name_bio_label <- glabel('* Name of Biomet data frame', 
                           container = csv.frame_bio)
  name_biomet <- gedit('df_biomet', container = csv.frame_bio)
  # Browse for Biomet data
  biomet_label <- glabel('* Biomet data', container = csv.frame_bio)
  dest_biomet <- gfilebrowse('Upload Biomet csv file', 
                             container = csv.frame_bio,
                             handler = function(h, ...) {
                             })
  
  
  importData_btn <- gbutton(text = "Import data", container = win,
                            handler   = function(h,...){
                              
                              # Read all EC data
                              # To assign the values given to integer
                              num_ec <- unlist(strsplit(svalue(num_lin_skip_ec), 
                                                        split = ','))
                              num_lin_skip_ec <- as.integer(num_ec)
                              
                              df_ec <- readLines(svalue(dest_ec))
                              
                              if (num_lin_skip_ec[1] > 0) {
                                df_ec <- df_ec[-num_lin_skip_ec]
                              } else {
                                df_ec <- df_ec
                              }
                              
                              # Import EC data
                              df_ec <- read.csv(textConnection(df_ec),
                                                header = svalue(check_ec))
                              # To rename the data frame to user specified name
                              do.call('<<-', list(svalue(name_ec), df_ec))
                              
                              # Read all Biomet data
                              # To assign the values given to integer
                              num_bio <- unlist(strsplit(svalue(num_lin_skip_bio), 
                                                         split = ','))
                              
                              num_lin_skip_bio <- as.integer(num_bio)
                              
                              df_biomet <- readLines(svalue(dest_biomet))
                              
                              if (num_lin_skip_bio[1] > 0) {
                                df_biomet <- df_biomet[-num_lin_skip_bio]
                              } else {
                                df_biomet <- df_biomet
                              }
                              
                              # Import Biomet data
                              df_biomet <- read.csv(textConnection(df_biomet),
                                                    header = svalue(check_bio))
                              # To rename the data frame to user specified name
                              do.call('<<-', list(svalue(name_biomet), df_biomet))
                              
                            })
  visible(win) <- TRUE
  
  #path.frame   <- gframe("Output Directory ", container = win)
  
  #brow         <- gfilebrowse(text = "Select folder...", type = "selectdir", 
  #                            container = path.frame,
  #                            handler = function(h,...){
  #                              out.dir <<- svalue(brow)
  #                            })
  
}
importData()
