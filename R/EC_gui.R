## GUI to import eddy covariance data

#### Preliminaries ####
library(gWidgets)
library(gWidgetsRGtk2)




# Create a window
win <- gwindow("Tab delimited file upload")
# Create the group to add widgets
grp_name <- ggroup(container = win)
# Add a label
lbl_data_frame_name <- glabel("Data frame to save data to: ", container = grp_name)
# Add text to edit box
txt_data_frame_name <- gedit("dfr", container = grp_name)

# Another group for the upload button
grp_upload <- ggroup(container = win)

# Create a handler to respond to button presses
btn_upload <- gbutton(
  text      = "Upload csv file", container = grp_upload,
  handler   = function(h, ...) {
    gfile(
      text    = "Upload csv file",
      type    = "open",
      action  = ifelse("read.csv"),
      handler = function(h, ...)
      {
        tryCatch(
          {
            data_frame_name <- make.names(svalue(txt_data_frame_name))
            the_data <- do.call(h$action, list(h$file))
            assign(data_frame_name, the_data, envir = globalenv())
            svalue(status_bar) <-
              paste(nrow(the_data), "records saved to variable", data_frame_name)
          },
          error = function(e) svalue(status_bar) <- "Could not upload data"
        )
      },
      filter = list(
        "Tab delimited" = list(patterns = c("*.csv")),
        "All files" = list(patterns = c("*"))
      )
    )
  })

# If we want to create a checkbox to configure file settings
#use_comma_for_decimal <- function() {
#  unname(Sys.localeconv()["decimal_point"] == ",")
#}
#chk_eurostyle <- gcheckbox(
#  text      = "Use comma for decimal place",
#  checked   = use_comma_for_decimal(),
#  container = grp_upload
#)

# To add a status bar
status_bar <- gstatusbar("", container = win)


# To create generic widget for some functions in R
lmwidget <- ggenericwidget(lm)
