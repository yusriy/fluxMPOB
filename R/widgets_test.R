#install.packages('gWidgetsRGtk2', dep = TRUE, type ='source')
require(gWidgets)
options('guiToolkit' = 'tcltk')


obj <- gbutton('Hello World!', container = gwindow())

obj <- glabel('Hello World!', container = gwindow())

obj <- gedit('Hello World!', container = gwindow())

obj <- gtext('Hello World!', container = gwindow())

obj <- gradio(c('Hello','World'), container = gwindow())

obj <- gcombobox(c('Hello','World'), container = gwindow())

obj <- gcombobox(c('Hello','World'), 
                 editable = TRUE,
                 container = gwindow())

obj <- gtable(c('Hello','World'), container = gwindow())

obj <- gcheckboxgroup(c('Hello','World'), container = gwindow())

obj <- gslider(from = 0, to = 7734, by = 100, value = 0, container = gwindow())

obj <- gspinbutton(from = 0, to = 7734, by = 100, value = 0, container = gwindow())

