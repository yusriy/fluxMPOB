# Function to create a GUI to import data

importData <- function(){
  options(guiToolkit = "tcltk")
  
  # Create a window
  win         <- gwindow("Import and Process Data in R", visible = FALSE)
  required_label <- glabel('* Required', container = win)
  # Create a subsection in window for EC
  csv.frame_ec   <- gframe("Import EC data", horizontal = FALSE,
                           container = win)
  # Check box for file details EC
  check_ec <- gcheckbox('Tick if header is present', 
                        container = csv.frame_ec)
  num_lin_skip_ec_label <- glabel('* Number of lines to skip (from the top)', 
                                  container = csv.frame_ec)
  num_lin_skip_ec <- gspinbutton(value = 1, from = 0, to = 20, by = 1, 
                                 container = csv.frame_ec)
  # Browse for EC data
  ec_label <- glabel('* Eddy covariance data', container = csv.frame_ec)
  dest_ec <- gfilebrowse("Upload EC csv file", container = csv.frame_ec,
                         handler = function(h,...) {
                           #f.csv(svalue(dest_ec)) # What is this?
                         })
  # Create a subsection in window for Biomet
  csv.frame_bio   <- gframe("Import Biomet data", container = win,
                            horizontal = FALSE)
  # Check box for file details EC
  check_bio <- gcheckbox('Tick if header is present', 
                        container = csv.frame_bio)
  num_lin_skip_bio_label <- glabel('* Number of lines to skip (from the top)', 
                                  container = csv.frame_bio)
  num_lin_skip_bio <- gspinbutton(value = 1, from = 0, to = 20, by = 1, 
                                 container = csv.frame_bio)
  # Browse for Biomet data
  biomet_label <- glabel('* Biomet data', container = csv.frame_bio)
  dest_biomet <- gfilebrowse('Upload Biomet csv file', 
                             container = csv.frame_bio,
                             handler = function(h, ...) {
                               #f.csv(svalue(dest_ec)) # What is this?
                             })
  
  
  #path.frame   <- gframe("Output Directory ", container = win)
  
  #brow         <- gfilebrowse(text = "Select folder...", type = "selectdir", 
  #                            container = path.frame,
  #                            handler = function(h,...){
  #                              out.dir <<- svalue(brow)
  #                            })
  
  importData_btn <- gbutton(text = "Import data", container = win,
                            handler   = function(h,...){
                              df_ec <<- read.csv(svalue(dest_ec), 
                                                 skip = 
                                                   svalue(num_lin_skip_ec),
                                                 header = svalue(check_ec))
                              df_biomet <<- read.csv(svalue(dest_biomet),
                                                     skip = 
                                                       svalue(num_lin_skip_bio),
                                                     header = svalue(check_bio))
                            })
  visible(win) <- TRUE
}
importData()
