
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
      dat$Date <- as.Date(dat$Time)
      dat <- dat[which(dat$Hour>=0 & dat$Task!="Stop"), ]
      if(nrow(dat)==0 || (nrow(dat)==1 & is.na(dat$Hour)))  
        return(NULL)
      dat <- aggregate(Hour ~ Task + Date, data=dat,
        sum, na.action=na.omit)
      dat$HourP <- round((dat$Hour/sum(dat$Hour))*100, 1)
      dat 
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
    DayData <- read.csv(day_file,
      colClasses=c("Date", "numeric"))
    isToday <- any(today %in% DayData$Date) 
    if(!is.null(dat)) { 
      dat <- dat[!(dat$Task %in% ggNames[3]), ]
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
    dat <- dat[which(dat$Date > (today-7)), ]
    if(is.null(dat) || nrow(dat)==0) 
      dat <- data.frame(Date=today, Hour=0.00) 
    dat$Day <- format(dat$Date, "%a")
    dat$Week <- paste0("Week",as.numeric(format(dat$Date, "%U")))
    dat <- dat[, c("Date", "Week", "Day", "Hour")] 
    dat$Hour <- sp_fmt(dat$Hour)
    dat
  }

  calcMonth <- function(dat) {
    dat$Week <- as.numeric(format(dat$Date, "%U"))
    dat <- dat[which(dat$Date > (today-28)), ]
    dat <- aggregate(Hour ~ Week, data=dat, sum)
    if(nrow(dat)<4) {
      firstMnth <- min(dat$Week)
      dat4 <- data.frame(Week =c(firstMnth:(firstMnth+3)))
    } else {
      lastMnth <- max(dat$Week)
      dat4 <- data.frame(Week =c((lastMnth-3):lastMnth))
    }
    dat <- base::merge(dat4, dat, by="Week", all.x=TRUE)
    dat$Hour <- ifelse(!is.na(dat$Hour), dat$Hour, 0.00)
    dat$HourF <- ifelse(!is.na(dat$Hour), sp_fmt(dat$Hour), "0:00")
    dat$Week <- paste0("Week", dat$Week)
    dat$Hour60 <- as.numeric(sub(":", ".", dat$HourF))
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
    spData <- spData[which(as.Date(spData$Time)==today), ]
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

  lastWkUpdate <- function(day_file) {
    sp_DF <- calcWeek(day_file)
    sp_o1[] <- sp_DF
    svalue(img_out) <- doPlot(day_file)
    svalue(notebook) <- 2
  }
