#' @title successr
#' 
#' @description  A diagnostic tool for tracking work and wasted time.
#' 
#' @details \code{successr} is a
#' tool for tracking the amount of time you work (and waste) during the day. At a metaphysical level, time
#' can be divided into two categories: work time and wasted time. Work time is effectively
#' used when you focus on at most two relevant projects during the day.
#' \code{Wasted Time} includes anything that does not fall in \code{Project 1} and/or
#' \code{Project 2}.  Culprits for \code{Wasted Time} are typically
#' toilet/cigarette/watercooler excursions, lunch breaks, unannounced visits by the boss,
#' any meeting, internet surfing, chit chat with colleagues, or courtesy calls by
#' telemarketers, friends/family, and partner(s).
#' 
#' Clicking \code{Project 1}, \code{Project 2}, and \code{Wasted Time} starts the timer,
#' and \code{Stop} stops the timer. \code{Report} gives a daily, weekly, monthly breakdown
#' of work time. \code{Edit} allows you to edit only the time recorded during the day and
#' the Task (from a drop-down menu).  \code{DayData.txt} allows you to edit the total time
#' for a given day (see below). 
#' 
#' Upon installation, \code{successr} will setup a data folder (where the data is stored) with a
#' configuration file, called \code{config.yml} (see \code{\link{config}}). The folder can
#' be found in \code{Sys.getenv("HOME")}. If you want to specify an alternative folder
#' path, then you must put in your .Rprofile (see \code{\link{Startup}}) the following
#' line: \code{Sys.setenv(R_SUCCESS="my/folder/path")}. In my Rprofile set-up, I have
#' \code{Sys.setenv("~/Dropbox/successr")} so I can sync between my work and home
#' computers. Labels for \code{Project 1}, \code{Project 2}, \code{Wasted Time} and
#' the \code{Window Title} can be changed in the \code{config.yml} file. But don't
#' waste time doing this, just use the default settings.  
#'
#'
#' @param verbose prints out configuration settings.
#' 
#' @param sanitize clears out unused labels from the Task dropdown menu in the \code{Edit}
#' window. 
#'
#' @import config 
#' @import gWidgets2 
#' @import grDevices 
#' @import graphics 
#' @import utils 
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
    if (!dir.exists(data_path)) 
      dir.create(data_path)
  }

  # Get configuration settings
  file.copy(file.path(pkg_path,"config.yml"), data_path) 
  config <- config::get(file=file.path(data_path, "config.yml"))
  button_labels <- config[grep("^button", names(config))]
  if (any(grepl('[^A-z0-9 ]|^$', button_labels)))
    stop("Button labels must be valid (non-empty) strings\n")
  ggNames <- unlist(button_labels)
  
  time_file <- file.path(data_path, "TimeSheet.Rdata")
  day_file <- file.path(data_path, "DayData.txt")

  # Create files if they do not exist
  if(!file.exists(time_file)) {
    spData <- data.frame(Time=Sys.time(), Task="Stop", 
      stringsAsFactors=FALSE)
    save("spData", file=time_file)
  }
  if(!file.exists(day_file)) {
    DayData <- data.frame(Date=today(), Hour=0)
    write.table(DayData, file=file.path(day_file),
      col.names=TRUE, row.names=FALSE, sep=",")
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
      dat$Time2 <- c(dat$Time[2:nrow(dat)],NA)
      dat$Hour <- as.numeric(difftime(dat$Time2,dat$Time, units='hours'))
      dat$Date <- asDate(dat$Time)
      dat <- dat[which(dat$Hour>=0 & dat$Task!="Stop"), ]
      # This is needed for the first time initialising TimeSheet
      if(nrow(dat)==0 || (nrow(dat)==1 & is.na(dat$Hour[1])))  {
        return(NULL)
      }
      dat <- stats::aggregate(Hour ~ Task + Date, data=dat,
        sum, na.action=stats::na.omit)
      dat$HourP <- round((dat$Hour/sum(dat$Hour))*100, 1)
      dat 
  }

  calcWeek <- function(day_file) {
    dat <- readCSV(day_file)
    dat <- dat[which(dat$Date > (today()-7)), ]
    if(is.null(dat) || nrow(dat)==0) 
      dat <- data.frame(Date=today(), Hour=0.00) 
    dat$Day <- format(dat$Date, "%a")
    dat$Week <- paste0("Week",as.numeric(format(dat$Date, "%U")))
    dat <- dat[, c("Date", "Week", "Day", "Hour")] 
    dat$Hour <- sp_fmt(dat$Hour)
    dat
  }

  calcMonth <- function(day_file) {
    dat <- readCSV(day_file)
    dat <- dplyr::filter(dat, format(.data$Date, "%Y")==format(today(), "%Y"))
    if (nrow(dat)==0) dat <- data.frame(Date=today(), Hour=0)
    # R defines first week as week 0
    dat$Week <- as.numeric(format(dat$Date, "%U")) + 1
    dat <- dat[which(dat$Date > (today()-28)), ]
    dat <- stats::aggregate(Hour ~ Week, data=dat, sum)
    if(nrow(dat)<4) {
      firstMnth <- min(dat$Week)
      dat4 <- data.frame(Week = c(firstMnth:(firstMnth+3)))
    } else {
      lastMnth <- max(dat$Week)
      dat4 <- data.frame(Week = c((lastMnth-3):lastMnth))
    }
    dat <- base::merge(dat4, dat, by="Week", all.x=TRUE)
    dat$Hour <- ifelse(!is.na(dat$Hour), dat$Hour, 0.00)
    dat$HourF <- ifelse(!is.na(dat$Hour), sp_fmt(dat$Hour), "0:00")
    dat$Week <- paste0("Week", dat$Week)
    dat$Hour60 <- as.numeric(sub(":", ".", dat$HourF))
    dat
  }

  doPlot <- function(day_file) {
    out <- calcMonth(day_file)
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

  lastWkUpdate <- function(day_file) {
    sp_DF <- calcWeek(day_file)
    sp_o1[] <- sp_DF
    svalue(img_out) <- doPlot(day_file)
    svalue(notebook) <- 2
  }

  doButton <- function(h, ...) {
    sapply(button_i, function(i) {
      ii <- base::get(i, envir=environment())
      font(ii) <- list(weight="normal", size=10, color="black")})
    if(h$action != "Stop")  
      font(h$obj) <- list(weight="bold", size=12, color="red")
     
    # Calc and Write time 
    load(time_file, envir=environment())
    aLine <- data.frame(Time=Sys.time(), Task=h$action)
    spData <- rbind(spData, aLine)
    spData <- spData[which(asDate(spData$Time)==today()), ]
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

  gEditButton <- function(time_file) {
    Gedit <- gwindow("Data Editor") 
    size(Gedit) <- list(width=80, 
      height=300, column.widths=c(70, 30))
    load(time_file)
    DF <- gdf(spData, container=Gedit)
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


  # Format time for GUI
  updateGuiTime <- function(dat) {
    getTime <- function(dat, task) {
      try(list2env(
        dat[which(dat$Task %in% task), c("Hour", "HourP")],
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
    DayData <- readCSV(day_file)
    isToday <- any(today() %in% DayData$Date) 
    if(!is.null(dat)) { 
      dat <- dat[!(dat$Task %in% ggNames[3]), ]
      if(nrow(dat)==0) {
        Hour <- 0 
      } else {
        Hour <- stats::aggregate(Hour ~ Date, dat, sum)$Hour
      }
      if (isToday==TRUE) {
        DayData$Hour[DayData$Date==today()] <- Hour
      } else {
        newLine <- data.frame(Date=today(), Hour=Hour)
        DayData <-  rbind(DayData, newLine) 
      }
    } else if(is.null(dat)) {
      if (isToday==TRUE) {
        DayData$Hour[DayData$Date==today()] <- 0
      } else {
        newLine <- data.frame(Date=today(), Hour=0)
        DayData <-  rbind(DayData, newLine) 
      } 
    }
    write.table(DayData, file=file.path(day_file), 
       col.names=TRUE, row.names=FALSE, sep=",")
  }

  ## LAYOUT 
  try(dispose(sp_env$win), silent=TRUE)

  sp_env$win <- gwindow(config$window_title, 
    width=620, height=240, visible=FALSE)

  # This makes the tabs
  notebook <- gnotebook (container = sp_env$win )
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
  visible(sp_env$win) <- TRUE
}
sp_env <- new.env(parent = emptyenv())

asDate <- function(x) 
  as.Date(x, origin="1970-01-01", tz = Sys.timezone(location = TRUE))

today <- function() asDate(Sys.time())

readCSV <- function(day_file) {
  dat <- read.delim(day_file, sep=",")
  if (class(dat$Date)=="character"){
    if (!is.na(asDate(dat$Date[1]))) {
      dat$Date <- asDate(dat$Date) 
      return(dat)
    } else {
      f1 <- "%Y-%m-%d"
      stop(paste("All dates in", day_file, 
      "must be in ", f1, "format."))
    }
  } else {
   stop(paste("Dates in", day_file, "must be in character format."))
 }
}

#' @title success_plot
#' 
#' @description  Plots the amount of time you have worked each month for the year.
#' 
#' @param data_path Path to your DayData.txt file. The default path is 
#' \code{Sys.getenv("R_SUCCESS")}.
#'
#' @param Year The default is to plot the data for the current year if data exists for
#' less than one year. If there is more than one year of data, then all years are plotted
#' by month. Plot the data by a single year by providing a numeric value. 
#' @import dplyr 
#' @export
#' @examples
#' success_plot(Year=2019)

success_plot <- function(
  data_path=file.path(Sys.getenv("R_SUCCESS"), "DayData.txt"), 
  Year=NULL) {
  if (!file.exists(data_path)) 
    stop("You must set the R_SUCCESS environment variable or provide a file path to the data.")
  dat <- readCSV(data_path)
  dat$Years <- as.integer(format(dat$Date, "%Y"))
  dat$Month <- as.integer(format(dat$Date, "%m"))
  dat$MonthLab <- format(dat$Date, "%b")
  Labs <- dat[!duplicated(dat$Month), c("Month", "MonthLab")]
  #
  if (is.null(Year) & length(unique(dat$Years)) >= 2) {
    byYearFun(dat, Labs) 
  } else {
    if (is.null(Year)) Year <- as.integer(format(today(), "%Y"))
    dat <- dat[dat$Years==Year, ]
    adat <- stats::aggregate(Hour ~ Month, data=dat, FUN=sum) 
    adat <- base::merge(adat, Labs, by = "Month")
    dev.new(width=7, height=7, unit="in")
    xx = with(adat, barplot(Hour, names.arg=MonthLab, 
      ylab="Hours", xlab="Month", ylim=c(0, max(Hour)+20),
      col = terrain.colors(length(Month)), font.lab=2,
      main = paste("Year", Year)))
    with(adat, text(x = xx, y= Hour, labels=round(Hour), pos=3))
  }
}

#' @title byYearFun
#' 
#' @description  Plots the amount of time you have worked each month by year.
#' 
#' @param dat Data from 
#' @param Lab Argument received from success_plot()
#'
#' @import dplyr 
#' @keywords internal 

byYearFun <- function(dat, Lab=NULL) {
  dat <- group_by(dat, .data$Years, .data$Month) %>% 
    summarize(Total = sum(.data$Hour))
  dat <- arrange(dat, .data$Month, .data$Years)
  dat <- left_join(dat, Lab, by="Month")
  dat <- stats::reshape(as.data.frame(dat), v.names="Total", idvar = "Month", 
    timevar="Years", direction="wide")
  rownames(dat) <- dat$MonthLab
  dat <- t(dat[, -c(1, 2)])
  dat <- dat[order(rownames(dat)), ]
  dev.new(width=10, height=5, unit="in")
  barplot(dat, beside=TRUE, ylab="Hours", 
    col=terrain.colors(nrow(dat)), font.lab=2,
    legend.text=as.character(gsub("Total.", "", rownames(dat))))
}



#' @title load_daydata
#' 
#' @description Load the DayData.txt dataset.
#' 
#' @param data_path Path to your DayData.txt file. The default path is 
#' \code{Sys.getenv("R_SUCCESS")}.
#'
#' @return data.frame
#'
#' @export 
load_daydata <- function(
  data_path=file.path(Sys.getenv("R_SUCCESS"), "DayData.txt")) {
  if (!file.exists(data_path)) 
    stop("You must set the R_SUCCESS environment variable to your data")
  dat <- readCSV(data_path)
  dat
}


#' @title success_summary
#' 
#' @description  Provides summary statistics for time worked by year. 
#' 
#' @param year Year to report; default = NULL to report for all available years.
#' @param more_than A vector of integers to report number of days worker for more than x
#' hours. 
#' 
#' @export 

success_summary <- function(year=NULL, more_than=c(3, 5)) {
  dat <- load_daydata()
  dat <- dplyr::mutate(dat, 
    Year = as.integer(format(.data$Date, "%Y")),
    Month = as.integer(format(.data$Date, "%m")))
  if (is.null(year)) year <- unique(dat$Year)
  stopifnot(more_than %in% 0:24) 
  lft <- "|"
  rgt <- "|\n"
  # Add padding to table
  w4 <- function(x) formatC(x, width=4)
  # Print out more_than lines
  mfun <- function(dat) {
    function(x) {
      xdays <- length(unique(dat$Date[dat$Hour > x])) 
      xdays <- ifelse(is.null(xdays), 0, xdays)
      cat(paste0(lft, " Days worked >", formatC(x, width=2), " hour(s): ", w4(xdays), " ",  rgt))
    }
  }
  getDat <- function(iyear) {
    dat <- dplyr::filter(dat, .data$Year == iyear)
    sum_hour <- round(sum(dat$Hour, na.rm=TRUE))
    days <- as.numeric(format(Sys.time(), "%j"))
    if (iyear == as.integer(format(Sys.time(), "%Y"))) {
      ave_mnth <- as.character(round((sum_hour/days) * (365.25/12)))
    } else {
      mnths <- length(unique(dat$Month))
      ave_mnth <- as.character(round(sum_hour/mnths))
      todate <- sum(dat$Hour[as.numeric(format(dat$Date, "%j"))<=days])
      todate <- as.character(round(todate))
    }
    sum_hour <- as.character(sum_hour)
    cat(paste(lft, "======== Year:", iyear, "=========", rgt))
    cat(paste(lft, "Total hours worked     :", w4(sum_hour), rgt))
    if (iyear != as.integer(format(Sys.time(), "%Y"))) 
        cat(paste(lft, "Total hours to date    :", w4(todate),  rgt))
    cat(paste(lft, "Ave. monthly hours     :", w4(ave_mnth),  rgt))
    mfun1 <- mfun(dat)
    lapply(more_than, mfun1)
  }
  cat("\n")
  cat(paste(rep("-", 33), collapse=""), "\n")
  for (i in year) getDat(i)
  cat(paste(rep("-", 33), collapse=""), "\n")
}
