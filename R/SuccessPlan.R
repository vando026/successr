#' @title The Ultimate Success Plan
#' 
#' @description  A diagnostic tool for quantifying time wastage.
#' 
#' @details You cannot be successful if you waste time. The Ultimate Success Plan is a
#' tool for diagnosing the amount of time you waste during the day. Its aim is to finally
#' convince you that you don't really work 8 hours a day.  At a metaphysical level, time
#' can be divided into two categories: work time and wasted time. Work time is effectively
#' used when you focus on at most two projects during the day that you think are relevant.
#' \code{Wasted Time} includes anything that does not fall in \code{Project 1} and/or
#' \code{Project 2}.  Culprits for \code{Wasted Time} are typically
#' toilet/cigarette/watercooler excursions, lunch breaks, unannounced visits by the boss,
#' any meeting, internet surfing, chit chat with colleagues, or courtesy calls by
#' telemarketers, friends/family, and partner(s).
#' 
#' Clicking \code{Project 1}, \code{Project 2}, and \code{Wasted Time} starts the timer,
#' and \code{Stop} stops the timer. \code{Report} gives a daily, weekly, monthly breakdown
#' of work time. \code{Edit} allows you to edit only the time recorded during the day and
#' the Task (from a drop-down menu).  \code{DayData.csv} allows you to edit the total time
#' for a given day (see below). 
#' 
#' Upon installation, successr will setup a data folder (where the data is stored) with a
#' configuration file, called \code{config.yml} (see \code{\link{config}}). The folder can
#' be found in \code{Sys.getenv("HOME")}. If you want to specify an alternative folder
#' path, then you must put in your .Rprofile (see \code{\link{Startup}}) the following
#' line: \code{Sys.setenv(R_SUCCESS="my/folder/path")}. In my Rprofile set-up, I have
#' \code{Sys.setenv("~/Dropbox/vando026/successr")} so I can sync between my work and home
#' computers. Labels for \code{Project 1}, \code{Project 2}, \code{Wasted Time} and
#' the \code{Window Title} can be changed in the \code{config.yml} file. But don't
#' waste time doing this, just use the default settings.  
#'
#' Requires \code{GTK} libraries to work that you may (or may not) have to manually install
#' yourself (which will waste some time). The Ultimate Success Plan project is in the
#' development phase, please report bugs to me by typing in the \code{R} console
#' \code{packageDescription("successr")}.
#'
#' @param verbose prints out configuration settings.
#' 
#' @param sanitize clears out unused labels from the Task dropdown menu in the \code{Edit}
#' window. This issue often merges when you waste time tinkering with button labels. 
#'
#' @import config 
#' @importFrom gWidgets2 gbutton gaction gnotebook gimage svalue gtable addSpace gwindow
#' size<- ggroup dispose size gseparator glabel svalue<- visible<- font<-
#' addHandlerChanged gdf
#' @importFrom grDevices dev.off png
#' @importFrom graphics par
#' @importFrom stats aggregate na.omit
#' @importFrom utils capture.output read.csv write.csv
#' 
#' @export

successr <- function(verbose=FALSE, sanitize=FALSE) {

  options(guiToolkit="RGtk2" )
  pkg_path  <- system.file(package='successr')
  data_path <- file.path(Sys.getenv("HOME"), "successr")
  env_path  <- Sys.getenv("R_SUCCESS")

  if (env_path=="" & !dir.exists(data_path)) {
    dir.create(data_path)
    message(paste0('Data and config files for successr are in "', 
      file.path(Sys.getenv("HOME"),"successr"), 
      '".\n See the help file to change default settings. ')) 
  } else if (env_path != "") {
    data_path <- env_path
    if (!dir.exists(data_path)) dir.create(data_path)
  }

  # Get configuration settings
  file.copy(file.path(pkg_path,"config.yml"), data_path) 
  config <- config::get(file=file.path(data_path, "config.yml"))
  button_labels <- config[grep("^button", names(config))]
  if (any(grepl('[^A-z0-9 ]|^$', button_labels)))
    stop("Button labels must be valid (non-empty) strings\n")
  ggNames <- unlist(button_labels)
  
  time_file <- file.path(data_path, "TimeSheet.Rdata")
  day_file <- file.path(data_path, "DayData.csv")
  today <- as.Date(Sys.time())

  # Create files if they do not exist
  if(!file.exists(time_file)) {
    spData <- data.frame(Time=Sys.time(), Task="Stop", 
      stringsAsFactors=FALSE)
    save("spData", file=time_file)
  }
  if(!file.exists(day_file)) {
    DayData <- data.frame(Date=today, Hour=0)
    write.csv(DayData, file=file.path(day_file),
      row.names=FALSE)
  }

  # Shorthand names for GUI setup
  group_i <- paste0("G",1:3)
  button_i <- paste0("B",1:3)
  label_i <- paste0("L", 1:3)

  ## LAYOUT 
  try(dispose(SuccessWindow), silent=TRUE)
  SuccessWindow <<- gwindow(config$window_title, 
    width=620, height=240, visible=FALSE)

  # This makes the tabs
  notebook <- gnotebook (container = SuccessWindow )
  sp_g0 <- ggroup(label='Main', horizontal=TRUE, 
    spacing=10, container=notebook)

  sp_f0 <- ggroup(horizontal=FALSE, spacing=0, container=sp_g0)
  sp_f1 <- ggroup(horizontal=TRUE, expand=TRUE, fill='x', 
    spacing=10, container=sp_g0)

  # Set the paramaters of the ggGroups
  ggList <- list(horizontal = FALSE, spacing=5, 
    expand=TRUE, fill='x', container = sp_f1)

  # Now do settings for three main buttons 
  for(i in seq(3)) {
    assign(group_i[i], do.call("ggroup", ggList))
    gi <- base::get(group_i[i], envir=environment())
    addSpace(gi, 5)
    assign(button_i[i], gbutton(ggNames[i], 
      container=gi, expand=TRUE, fill="y",
      handler=doButton, action=ggNames[i]))
    size(gi) <- c(70, 100)
    sep <- gseparator(container=gi)
    assign(label_i[i], glabel("", container=gi))
    addSpace(gi, 2)
  }

  sp_f2 <- ggroup(horizontal=FALSE, spacing=8, container=sp_g0)
  addSpace(sp_f2, 3)
  ST <- gbutton("Stop", container=sp_f2, expand=TRUE, fill='y',
    handler=doButton, action="Stop")
  r_act <- gaction("Report", icon="overview", 
    handler=function(...) lastWkUpdate(day_file))
  rweek <- gbutton(action=r_act, container=sp_f2, expand=TRUE, fill='y')
  e_act <- gaction("Edit", icon="editor", 
    handler=function(...) gEditButton(time_file))
  Edit <- gbutton(action=e_act, container=sp_f2, expand=TRUE, fill='y')
  addSpace(sp_f2, 0.0)
  f3 <- ggroup(horizontal=FALSE, spacing=10, container=sp_g0)

  sp_out <- ggroup(label='Last Week', horizontal=TRUE, 
    spacing=10, container=notebook)
  out0 <- ggroup(horizontal=TRUE, container=sp_out)
  sp_o1 <- gtable(calcWeek(day_file), container = out0, expand=FALSE)
  size(sp_o1) <- list(width=250, height=220, 
    column.widths=c(90, 70, 50, 40))
  addSpace(sp_out, 1)

  # Plot
  sp_gr <- ggroup(container=out0, horizontal=TRUE, spacing=0)
  img <- doPlot(day_file) 
  img_out <- gimage(basename(img),dirname(img), container = out0)

  if (verbose==TRUE) {
    message('Your configuration settings are:')
    print(config)
  }
  
  svalue(notebook) <- 1
  visible(SuccessWindow) <- TRUE
}


