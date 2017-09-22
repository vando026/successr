## Description: The Ultimate Success Plan
## Project: 
## Author: AV / Created: 11Jan2017 

# Packages
require(gWidgets2, quietly=TRUE, warn.conflicts = TRUE)
require(gWidgets2RGtk2, quietly=TRUE, warn.conflicts = TRUE)
options (guiToolkit="RGtk2" )

# Get configuration settings
pkg_path <- dirname(getSrcDirectory(function(x) {x}))
config_path <- file.path(pkg_path, "config.R") 
source(config_path, local=TRUE)

# Set data path
if(data_path=="") 
  data_path <- file.path(pkg_path, 'data')
time_file <- file.path(data_path, "TimeSheet.Rdata")
day_file <- file.path(data_path, "DayData.csv")

today <- as.Date(Sys.time())

# Create files if they do not exist
if(!file.exists(time_file)) {
  spData <- data.frame(Time=Sys.time(), Task="ST")
  save("spData", file=time_file)
}
if(!file.exists(day_file)) {
  DayData <- data.frame(Date=today, Hour=0)
  write.csv(DayData, file=file.path(day_file),
    row.names=FALSE)
}

#### Format Time 
sp_fmt <- function(x) {
  fhour <- x %/% 1
  fmin <- round((x %% 1)*0.6, 2) 
  fmin <- sub("(\\d+).(\\d+)", "\\2", format(fmin, nsmall=2))
  out <- paste(fhour, fmin, sep=':')
  out 
}

# This is the main time calc function
calcTime <- function(dat) {
    if(is.null(dat) || nrow(dat)==0) return(NULL)
    Seconds <- as.numeric(dat$Time)
    Hour <- round(diff(Seconds)/(60*60), 2)
    dat <- transform(dat, Hour=c(Hour, NA),
      Date=as.Date(Time))
    dat <- subset(dat, Task!="ST")
    if(nrow(dat)==0 || (nrow(dat)==1 & is.na(dat$Hour))) {
      return(NULL)
    }
    dat <- aggregate(Hour ~ Task + Date, dat, sum, 
      na.action=na.omit)
    dat <- transform(dat, 
      HourP=round((Hour/sum(Hour))*100, 1))
    dat 
}
# debugonce(calcTime)
# dat=calcTime(spData)

# Format time for GUI
updateGuiTime <- function(dat) {
  getTime <- function(dat, i) {
    if(is.null(dat) || any(dat$Task %in% i)==FALSE) {
      Hour <- HourP <- 0 
    } else {
      Hour <- dat[dat$Task %in% i, "Hour"] 
      HourP <- dat[dat$Task %in% i, "HourP"] 
    }
    paste0(sp_fmt(Hour),"Hrs (",HourP,"%)")
  }
  for(i in ggNames) {
    Label <- get(paste0(i,"L"), envir=globalenv()) 
    svalue(Label) <- getTime(dat, i)
  } 
}
# getTime(tt, "P1" )


writeDay <- function(dat, day_file) {
  DayData <- read.csv(day_file,
    colClasses=c("Date", "numeric"))
  isToday <- any(today %in% DayData$Date) 
  if(!is.null(dat)) { 
    dat <- subset(dat, Task!="WT")
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
# debugonce(writeDay)
# yes <- writeDay(tt, DayData)

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
# debugonce(calcWeek)
# spDayData1 <- calcWeek(DayData)

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
  dat <- merge(dat4, dat, by="Week", all.x=TRUE)
  dat <- transform(dat, 
    Hour=ifelse(!is.na(Hour), Hour, 0.00),
    HourF=ifelse(!is.na(Hour), sp_fmt(Hour), "0:00"),
    Week=paste0("Week", Week))
  dat <- transform(dat, Hour60=as.numeric(sub(":", ".", HourF)))
  dat
}
# debugonce(calcMonth)
# tt <- calcMonth(DayData)

doPlot <- function(day_file) {
  DayData <- read.csv(day_file,
    colClasses=c("Date", "numeric"))
  out <- calcMonth(DayData)
  f <- tempfile( )
  png(f, units="in",
       width=3.6, height=1.8, pointsize=8, res=100, type="cairo")
  par(mar=c(2.7, 5.0, 0, 0.4))
  with(out, barplot(Hour60, horiz=TRUE, 
    col=rep(c("seagreen1", "seagreen3"), 2),
    names.arg=Week, las=1))
  maxHour <- max(out$Hour)
  label_pos <- ifelse((out$Hour/maxHour < 0.2) || maxHour==0, 4, 2)
  with(out, text(Hour60 , c(0.7, 1.8, 3, 4.2), 
    labels=HourF, pos=label_pos, adj=1))
  dev.off()
  f
}
# debugonce(doPlot)
# doPlot(day_file)


doButton <- function(h, ...) {
  sapply(ggNames, function(i) {
    ii <- get(i, envir=globalenv())
    font(ii) <- list(weight="normal", size=10, color="black")})
  if(h$action!="Stop") font(h$obj) <- list(weight="bold", size=12, color="red")
   
  # Calc and Write time 
  load(time_file, envir=environment())
  aLine <- data.frame(Time=Sys.time(), Task=h$action)
  spData <- rbind(spData, aLine)
  spData <- subset(spData, as.Date(Time)==today)
  save("spData", file=time_file)
  time_dat <- calcTime(spData)
  updateGuiTime(time_dat)
  writeDay(time_dat, day_file)
}
# debugonce(doButton)

gEditButton <- function(time_file) {
  Gedit <- gwindow("Data Editor") 
  size(Gedit) <- list(width=80, 
    height=300, column.widths=c(70, 30))
  load(time_file)
  rownames(spData) <- seq(nrow(spData))
  DF <- gdf(spData, cont=Gedit)
  addHandlerChanged(DF, handler = function(h ,...) {
    spData <- data.frame(DF[])
    updateGuiTime(time_dat)
    save("spData", file=time_file)})
}
# debugonce(gEditButton)
# gEditButton(time_file)


lastWkUpdate <- function(day_file) {
  sp_DF <- calcWeek(day_file)
  sp_o1[] <- sp_DF
  svalue(img_out) <- doPlot(day_file)
  svalue(notebook) <- 2
}

###############################################################################################
######################################## LAYOUT ###############################################
###############################################################################################
window <- gwindow(Window_title, 
  width=620, height=240, visible=FALSE)

# This makes the tabs
notebook <- gnotebook (cont = window )
sp_g0 <- ggroup(label='Main', horizontal=TRUE, 
  spacing=10, cont=notebook)

sp_f0 <- ggroup(horizontal=FALSE, spacing=0, cont=sp_g0)
sp_f1 <- ggroup(horizontal=TRUE, expand=TRUE, fill='x', 
  spacing=10, cont=sp_g0)

# Set the paramaters of the ggGroups
ggList <- list(horizontal = FALSE, spacing=5, 
  expand=TRUE, fill='x', cont = sp_f1)

# Iterate through names
ggNames <- c("P1", "P2", "WT")
ggLabels <- c(Button_label_1, 
  Button_label_2, "Wasted Time")

# Now do all settings for buttons
for(i in seq(3)) {
  assign(paste0(ggNames[i],"G"), 
    do.call("ggroup", ggList))
  gi <- get(paste0(ggNames[i],"G"), envir=environment())
  addSpace(gi, 5)
  assign(ggNames[i], gbutton(ggLabels[i], 
    cont=gi, expand=TRUE, fill="y",
    handler=doButton, action=ggLabels[i]))
  size(gi) <- c(70, 100)
  sep <- gseparator(cont=gi)
  assign(paste0(ggNames[i],"L"), 
    glabel("", cont=gi))
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

# Plots
sp_gr <- ggroup(cont=out0, horizontal=TRUE, spacing=0)
img <- doPlot(day_file) 
img_out <- gimage(basename(img),dirname(img), cont = out0)


svalue(notebook) <- 1
visible(window) <- TRUE

###############################################################################################
###############################################################################################
###############################################################################################


