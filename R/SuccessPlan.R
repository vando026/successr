#' @title The Ultimate Success Plan
#' 
#' @description  A diagnostic tool for quantifying time wastage.
#' 
#' @details You cannot be successful if you waste time. The Ultimate Success Plan is a
#' tool for diagnosing the amount of time you waste during the day. Its aim is to finally
#' convince you that you don't really work 8 hours a day.  At a metaphysical level, time
#' can be divided into two categories: work time and wasted time. Work time is effectively
#' used when you focus on one or two projects in the day. \code{Project 1} and/or
#' \code{Project 2} are whatever projects you think are relevant.  \code{Wasted Time}
#' includes anything that does not fall in \code{Project 1} and/or \code{Project 2}.
#' Culprits for \code{Wasted Time} are typically toilet/cigarette/watercooler excursions,
#' lunch breaks, unannounced visits by the boss, any meeting, internet surfing, chit chat
#' with colleagues, or courtesy calls by telemarketers, friends/family, and partner(s).
#' 
#' Labels for \code{Project 1}, \code{Project 2}, and \code{Wasted Time} can only be
#' changed in the config.yml file. But just use the default settings rather than waste
#' time tinkering with button labels.  There is an option to set a personal
#' \code{data_path} to where you want your data stored. In my set-up, I put this package
#' in my \code{R} library folder (see \code{print(.libPaths())} but write the data to my
#' Dropbox folder so I can sync between my work and home computers. Things like the Window
#' Title can be changed in the config.yml file as well, but don't waste time doing this.  
#' 
#' Clicking \code{Project 1}, \code{Project 2}, and \code{Wasted Time} starts the timer,
#' and \code{Stop} stops the timer. \code{Report} gives a daily, weekly, monthly breakdown
#' of work time. \code{Edit} allows you to edit only the time recorded during the day and
#' the Task (from a drop-down menu).  \code{DayData.csv} file in \code{data_path} allows
#' you to edit the total time for a given day. 
#'
#' Requires \code{GTK} libraries to work, which you may or may not have to manually
#' install yourself. The Ultimate Success Plan project is in the development phase,
#' please report bugs to me by typing in the \code{R} console
#' \code{packageDescription("successr")}.
#'
#' @param verbose prints out configuration settings, default is \code{\link{FALSE}}
#' 
#' @param sanitize clears out unused labels from the Task dropdown menu in the \code{Edit}
#' window, default is \code{\link{FALSE}}. This issue often merges when you waste time
#' tinkering with button labels. 
#'
#' @return none
#' 
#' @import config 
#' 
#' @importFrom gWidgets2 gbutton gaction gnotebook gimage svalue gtable addSpace gwindow
#' size ggroup
#' 
#' @export

successr <- function(verbose=FALSE, sanitize=FALSE, ...) {

  options(guiToolkit="RGtk2" )

  pkg_path <- dirname(getSrcDirectory(function(x) {x}))
  data_path <- file.path(pkg_path, 'data')
  if (!dir.exists(data_path)) dir.create(data_path)

  # Get configuration settings
  config_path <- file.path(pkg_path, "config.yml") 
  config <- config::get(file=config_path)

  button_labels <- config[grep("^button", names(config))]
  if (any(grepl('[^A-z0-9 ]|^$', button_labels)))
    stop("Button labels must be valid (non-empty) strings\n")
  ggNames <- unlist(button_labels)
  
  check_path <- function(config) {
    if (config$data_path=="") stop() 
    if(!dir.exists(config$data_path)) {
      stop(paste('Warning:', config$data_path, 'directory does not exist,
        using default setting...\n'))
    }
    config$data_path
  }
  tryCatch(data_path <- check_path(config),
    error = function(e) message(e))
  
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

  #### Format Time 
  sp_fmt <- function(x) {
    Min <- round((x %% 1)*60)
    sprintf("%.1d:%.02d", x %/% 1, ifelse(Min==60, 59, Min))
  }

  # This is the main time calc function
  calcTime <- function(dat) {
      if(is.null(dat) || nrow(dat) %in% c(0,1)) 
        return(NULL)
      dat <- transform(dat, Time2=c(Time[2:nrow(dat)],NA))
      dat <- transform(dat, 
        Hour=as.numeric(difftime(Time2,Time, units='hours')),
        Date=as.Date(Time))
      dat <- dat[which(dat$Hour>=0 & dat$Task!="Stop"), ]
      if(nrow(dat)==0 || (nrow(dat)==1 & is.na(dat$Hour)))  
        return(NULL)
      dat <- aggregate(Hour ~ Task + Date, data=dat,
        sum, na.action=na.omit)
      dat <- transform(dat, 
        HourP=round((Hour/sum(Hour))*100, 1))
      dat 
  }

  # Format time for GUI
  updateGuiTime <- function(dat) {
    getTime <- function(dat, task) {
      try(list2env(
        subset(dat, Task %in% task, c("Hour", "HourP")),
        envir=environment()), silent=TRUE)
      if(is.null(dat) || length(Hour)==0 || Hour < 0.01) {
        Hour <- HourP <- 0 
      } 
      sprintf("%sHrs (%s%%)", sp_fmt(Hour), HourP)
    }
    for(i in seq(3)) {
      Label <- base::get(label_i[i], envir=environment()) 
      svalue(Label) <- getTime(dat, ggNames[i])
    } 
  }

  writeDay <- function(dat, day_file) {
    DayData <- read.csv(day_file,
      colClasses=c("Date", "numeric"))
    isToday <- any(today %in% DayData$Date) 
    if(!is.null(dat)) { 
      dat <- subset(dat, Task!=ggNames[3])
      if(nrow(dat)==0) {
        Hour <- 0 
      } else {
        Hour <- aggregate(Hour ~ Date, dat, sum)$Hour
      }
      if (isToday==TRUE) {
        DayData$Hour[DayData$Date==today] <- Hour
      } else {
        newLine <- data.frame(Date=today, Hour=Hour)
        DayData <-  rbind(DayData, newLine) 
      }
    } else if(is.null(dat)) {
      if (isToday==TRUE) {
        DayData$Hour[DayData$Date==today] <- 0
      } else {
        newLine <- data.frame(Date=today, Hour=0)
        DayData <-  rbind(DayData, newLine) 
      } 
    }
    write.csv(DayData, file=file.path(day_file), 
      row.names=FALSE)
  }

  calcWeek <- function(day_file) {
    dat <- read.csv(day_file,
      colClasses=c("Date", "numeric"))
    dat <- subset(dat, Date > (today-7))
    if(is.null(dat) || nrow(dat)==0) {
      dat <- data.frame(Date=today, Hour=0.00) 
    }
    dat <- transform(dat, Day = format(Date, "%a"))
    dat <- transform(dat, Week=paste0("Week",
      as.numeric(format(Date, "%U"))))
    dat <- dat[, c("Date", "Week", "Day", "Hour")] 
    dat <- transform(dat, Hour=sp_fmt(Hour))
    dat
  }

  calcMonth <- function(dat) {
    dat <- transform(dat, Week=as.numeric(format(Date, "%U")))
    dat <- subset(dat, Date > (today-28))
    dat <- aggregate(Hour ~ Week, data=dat, sum)
    if(nrow(dat)<4) {
      firstMnth <- min(dat$Week)
      dat4 <- data.frame(Week =c(firstMnth:(firstMnth+3)))
    } else {
      lastMnth <- max(dat$Week)
      dat4 <- data.frame(Week =c((lastMnth-3):lastMnth))
    }
    dat <- base::merge(dat4, dat, by="Week", all.x=TRUE)
    dat <- transform(dat, 
      Hour=ifelse(!is.na(Hour), Hour, 0.00),
      HourF=ifelse(!is.na(Hour), sp_fmt(Hour), "0:00"),
      Week=paste0("Week", Week))
    dat <- transform(dat, Hour60=as.numeric(sub(":", ".", HourF)))
    dat
  }

  doPlot <- function(day_file) {
    DayData <- read.csv(day_file,
      colClasses=c("Date", "numeric"))
    out <- calcMonth(DayData)
    f <- tempfile( )
    png(f, units="in",
         width=3.6, height=1.8, pointsize=8, res=100, type="cairo")
    par(mar=c(2.6, 4.4, 0.3, 1.2))
    maxHour <- max(out$Hour)
    with(out, barplot(Hour60, horiz=TRUE, 
      xlim=c(0, ifelse(maxHour<0.01, 1, maxHour)),
      col=rep(c("seagreen1", "seagreen3"), 2),
      names.arg=Week, las=1))
    label_pos <- ifelse((out$Hour/maxHour < 0.2) | maxHour==0, 4, 2)
    with(out, text(Hour60 , c(0.7, 1.8, 3, 4.2), 
      labels=HourF, pos=label_pos, adj=1))
    dev.off()
    f
  }

  doButton <- function(h, ...) {
    sapply(button_i, function(i) {
      ii <- base::get(i, envir=environment())
      font(ii) <- list(weight="normal", size=10, color="black")})
    if(h$action!="Stop")  
      font(h$obj) <- list(weight="bold", size=12, color="red")
     
    # Calc and Write time 
    load(time_file, envir=environment())
    aLine <- data.frame(Time=Sys.time(), Task=h$action)
    spData <- rbind(spData, aLine)
    spData <- subset(spData, as.Date(Time)==today)
    if (sanitize==TRUE) {
      spData$Task <- factor(spData$Task,
        levels=c(ggNames, "Stop"))
    }
    rownames(spData) <- seq(nrow(spData))
    save("spData", file=time_file)
    time_dat <- calcTime(spData) 
    updateGuiTime(time_dat)
    writeDay(time_dat, day_file)
  }

  check_dat <- function(dat) {
    print_dat <- function(x) {
      paste(capture.output(print(x)), collapse = "\n")
    }
    dat <- data.frame(dat)
    back_time <- which(diff(dat$Time)<0)
    if (length(back_time)>0) {
      stop('Warning: ignoring illegal time entry in: \n',
        print_dat(dat[back_time+1, c("Time", "Task") ]),'\n')
    }
    dat
  }

  gEditButton <- function(time_file) {
    Gedit <- gwindow("Data Editor") 
    size(Gedit) <- list(width=80, 
      height=300, column.widths=c(70, 30))
    load(time_file)
    DF <- gdf(spData, cont=Gedit)
    addHandlerChanged(DF, handler = function(h ,...) {
      spData <- spData
      tryCatch(spData <- check_dat(DF[]),
       error = function(e) message(e)) 
      if (sanitize==TRUE) {
        spData$Task <- factor(spData$Task,
          levels=c(ggNames, "Stop"))
      }
      time_dat <- calcTime(spData) 
      updateGuiTime(time_dat)
      save("spData", file=time_file)})
  }

  lastWkUpdate <- function(day_file) {
    sp_DF <- calcWeek(day_file)
    sp_o1[] <- sp_DF
    svalue(img_out) <- doPlot(day_file)
    svalue(notebook) <- 2
  }

  ## LAYOUT 
  try(dispose(SuccessWindow), silent=TRUE)
  SuccessWindow <<- gwindow(config$window_title, 
    width=620, height=240, visible=FALSE)

  # This makes the tabs
  notebook <- gnotebook (cont = SuccessWindow )
  sp_g0 <- ggroup(label='Main', horizontal=TRUE, 
    spacing=10, cont=notebook)

  sp_f0 <- ggroup(horizontal=FALSE, spacing=0, cont=sp_g0)
  sp_f1 <- ggroup(horizontal=TRUE, expand=TRUE, fill='x', 
    spacing=10, cont=sp_g0)

  # Set the paramaters of the ggGroups
  ggList <- list(horizontal = FALSE, spacing=5, 
    expand=TRUE, fill='x', cont = sp_f1)

  # Now do settings for three main buttons 
  for(i in seq(3)) {
    assign(group_i[i], do.call("ggroup", ggList))
    gi <- base::get(group_i[i], envir=environment())
    addSpace(gi, 5)
    assign(button_i[i], gbutton(ggNames[i], 
      cont=gi, expand=TRUE, fill="y",
      handler=doButton, action=ggNames[i]))
    size(gi) <- c(70, 100)
    sep <- gseparator(cont=gi)
    assign(label_i[i], glabel("", cont=gi))
    addSpace(gi, 2)
  }

  sp_f2 <- ggroup(horizontal=FALSE, spacing=8, cont=sp_g0)
  addSpace(sp_f2, 3)
  ST <- gbutton("Stop", cont=sp_f2, expand=TRUE, fill='y',
    handler=doButton, action="Stop")
  r_act <- gaction("Report", icon="overview", 
    handler=function(...) lastWkUpdate(day_file))
  rweek <- gbutton(action=r_act, cont=sp_f2, expand=TRUE, fill='y')
  e_act <- gaction("Edit", icon="editor", 
    handler=function(...) gEditButton(time_file))
  Edit <- gbutton(action=e_act, cont=sp_f2, expand=TRUE, fill='y')
  addSpace(sp_f2, 0.0)
  f3 <- ggroup(horizontal=FALSE, spacing=10, cont=sp_g0)

  sp_out <- ggroup(label='Last Week', horizontal=TRUE, 
    spacing=10, cont=notebook)
  out0 <- ggroup(horizontal=TRUE, cont=sp_out)
  sp_DF <- calcWeek(day_file)
  sp_o1 <- gtable(sp_DF, cont = out0, expand=FALSE)
  size(sp_o1) <- list(width=250, height=220, 
    column.widths=c(90, 70, 50, 40))
  addSpace(sp_out, 1)

  # Plot
  sp_gr <- ggroup(cont=out0, horizontal=TRUE, spacing=0)
  img <- doPlot(day_file) 
  img_out <- gimage(basename(img),dirname(img), cont = out0)

  if (verbose==TRUE) {
    message('Your configuration settings are:')
    print(config)
  }
  
  svalue(notebook) <- 1
  visible(SuccessWindow) <- TRUE
}


