## Description: 
## Project: 
## Author: AV / Created: 11Jan2017 

# Packages
require(gWidgets2, quietly=TRUE, warn.conflicts = TRUE)
require(gWidgets2RGtk2, quietly=TRUE, warn.conflicts = TRUE)
options (guiToolkit="RGtk2" )

# Files
sp_fname<- file.path(Sys.getenv("USERPROFILE"), "Dropbox/R/SuccessPlan")
sp_tfile <- file.path(sp_fname, "TimeSheetR.csv")
sp_rfile <- file.path(sp_fname, "TimeSheet.Rdata")

today <- as.Date(Sys.time())
load(sp_rfile)

# spData <- read.csv(sp_tfile, header=TRUE)
# spData <- transform(spData, Time=as.POSIXlt(Time, origin='1970-01-01') )
# spData <- transform(spData, Task=factor(Task, levels=c("P1", "P2", "WT", "ST")))
# dat=calcTime(spData)
# dat <- subset(dat, Task!="WT")
# spDayData <- aggregate(Hour ~ Date, dat, sum)
# save(list=c("spData", "spDayData"), file=sp_rfile)

#### Format Time 
sp_fmt <- function(x) {
  fhour <- x %/% 1
  fmin <- round((x %% 1)*0.6, 2) 
  fmin <- sub("(\\d+).(\\d+)", "\\2", format(fmin, nsmall=2))
  out <- paste(fhour, fmin, sep=':')
  out 
}
# debugonce(sp_fmt)
# x=sp_fmt(c(1.2, 3.8))

calcTime <- function(dat) {
    dat <- subset(dat, as.Date(Time)==today)
    if(nrow(dat)<=1) return(NULL)
    Seconds <- as.numeric(dat$Time)
    Hour <- round(diff(Seconds)/(60*60), 2)
    dat <- transform(dat, Hour=c(Hour, NA),
      Date=as.Date(Time))
    dat <- subset(dat, Task!="ST")
    dat <- aggregate(Hour ~ Task + Date, dat, sum, na.action=na.omit)
    dat <- transform(dat, HourP=round((Hour/sum(Hour))*100, 1))
    dat 
}
# debugonce(calcTime)
dat=calcTime(spData)

getTime <- function(out, x) {
  if(is.null(out) || any(out$Task %in% x)==FALSE) {
    Hour <- HourP <- 0 
  } else {
    Hour <- out[out$Task %in% x, "Hour"] 
    HourP <- out[out$Task %in% x, "HourP"] 
  }
  paste0(sp_fmt(Hour),"Hrs (",HourP,"%)")
}
# getTime(tt, "P1" )


writeDay <- function(dat, spDayData) {
  isToday <- any(today %in% spDayData$Date) 
  if(isToday==FALSE) { 
    if(is.null(dat)) {
      newLine <- data.frame(Date=today, Hour=0)
    } else if(!is.null(dat)) {
      newLine <- data.frame(Date=today, Hour=dat$Hour)
    }
    spDayData <-  rbind(spDayData, newLine) 
  } else if (!is.null(dat) && isToday==TRUE) {
    dat <- subset(dat, Task!="WT")
    dat <- aggregate(Hour ~ Date, dat, sum)
    spDayData$Hour[spDayData$Date==today] <- dat$Hour
  } 
  spDayData 
}
# debugonce(writeDay)
# yes <- writeDay(tt, spDayData)


calcWeek <- function(dat) {
  dat <- subset(dat, Date > (today-7))
  dat <- transform(dat, Day = format(Date, "%a"))
  dat <- transform(dat, Week=paste0("Week",
    as.numeric(format(Date, "%U"))))
  dat <- dat[, c("Date", "Week", "Day", "Hour")] 
  dat <- transform(dat, Hour=sp_fmt(Hour))
  dat
}
# debugonce(calcWeek)
spDayData1 <- calcWeek(spDayData)

calcMonth <- function(dat) {
  dat <- subset(dat, Date > (today-31))
  dat <- transform(dat, Week=as.numeric(format(Date, "%U")))
  maxWk <- max(dat$Week)
  dat <- subset(dat, Week %in% c((maxWk-3):maxWk))
  dat <- aggregate(Hour ~ Week, data=dat, sum)
  dat <- transform(dat, HourF=sp_fmt(Hour), 
    Week=paste0("Week", Week))
  dat
}
# debugonce(calcMonth)
# tt <- calcMonth(spDayData)

doPlot <- function(sp_rfile) {
  load(sp_rfile)
  out <- calcMonth(spDayData)
  f <- tempfile( )
  png(f, units="in",
       width=3.6, height=1.8, pointsize=8, res=100, type="cairo")
  par(mar=c(2.7, 5.0, 0, 0.4))
  with(out, barplot(Hour, horiz=TRUE, col=rep(c("seagreen1", "seagreen3"), 2), 
    names.arg=Week, las=1))
  Bar4 <- ifelse(out[4, "Hour"] < 10, 4, 2)
  with(out, text(Hour , c(0.7, 1.8, 3, 4.2), labels=HourF, pos=c(rep(2,3),Bar4), adj=1))
  dev.off()
  f
}
# debugonce(doPlot)
doPlot(sp_rfile)


callCalc <- function(spData) {
  # print(spData)
  # cat("\n In doButton\n")
  cdat <- calcTime(spData)
  # print(cdat)
  spDayData <- writeDay(cdat, spDayData)
  spData <- subset(spData, as.Date(Time)==today)
  # Update GUI
  sapply(ggNames, function(i) {
    ii <- get(paste0(i,"L"), envir=globalenv()) 
    svalue(ii) <- getTime(cdat, i)}) 
  save(list=c("spData", "spDayData"), file=sp_rfile)
  write.csv(spDayData, 
    file=file.path(sp_fname, "spDayData.csv"), 
    row.names=FALSE)
}
# debugonce(callCalc)
# callCalc(spData)


doButton <- function(h, ...) {
  toggleOff <- function(obj) {
    sapply(obj, function(i) { ii <- get(i, envir=globalenv())
      font(ii) <- list(weight="normal", size=10, color="black")})
  }
  ifelse(h$action=="ST", toggleOff(ggNames), toggleOff(setdiff(ggNames, h$action)))
  if(h$action!="ST") font(h$obj) <- list(weight="bold", size=12, color="red")
   
  # Write to time data file
  load(sp_rfile)
  aLine <- data.frame(Time=Sys.time(), Task=h$action)
  spData <- rbind(spData, aLine)
  callCalc(spData)
}

gEditButton <- function(sp_rfile) {
# browser()
  load(sp_rfile)
  Gedit <- gwindow("Data Editor") 
  size(Gedit) <- list(width=80, 
    height=300, column.widths=c(70, 30))
  DF <- gdf(tail(spData), cont=Gedit)
  addHandlerChanged(DF, handler = function(h ,...) {
    newDF <- DF[]
    dfn <- nrow(spData)
    if(dfn<5) {
      spData <- newDF 
    } else {
      spData <- rbind(spData[1:(dfn-6), ], newDF)
    }
    callCalc(spData)
  })
}
# debugonce(gEditButton)
# gEditButton(sp_rfile)


lastWkUpdate <- function(sp_rfile) {
  load(sp_rfile)
  sp_DF <- calcWeek(spDayData)
  sp_o1[] <- sp_DF
  svalue(img_out) <- doPlot(sp_rfile)
  svalue(notebook) <- 2
}

###############################################################################################
######################################## LAYOUT ###############################################
###############################################################################################
window <- gwindow("The Ultimate Success Plan", 
  width=620, height=230, visible=FALSE)

# This makes the tabs
notebook <- gnotebook (cont = window )
sp_g0 <- ggroup(label='Main', horizontal=TRUE, spacing=10, cont=notebook)

sp_f0 <- ggroup(horizontal=FALSE, spacing=0, cont=sp_g0)
sp_f1 <- ggroup(horizontal=TRUE, expand=TRUE, fill='x', 
  spacing=10, cont=sp_g0)

# Set the paramaters of the ggGroups
ggList <- list(horizontal = FALSE, spacing=5, 
  expand=TRUE, fill='x', cont = sp_f1)

# Iterate through names
ggNames <- c("P1", "P2", "WT")
ggLabels <- c(paste("Project",1:2), "Wasted Time")

# Now do all settings for buttons
for(i in seq(3)) {
  assign(paste0(ggNames[i],"G"), 
    do.call("ggroup", ggList))
  gi <- get(paste0(ggNames[i],"G"), envir=environment())
  addSpace(gi, 5)
  assign(ggNames[i], gbutton(ggLabels[i], cont=gi, expand=TRUE, fill="y",
    handler=doButton, action=ggNames[i]))
  size(gi) <- c(70, 100)
  sep <- gseparator(cont=gi)
  assign(paste0(ggNames[i],"L"), 
    glabel("", cont=gi))
  addSpace(gi, 2)
}

sp_f2 <- ggroup(horizontal=FALSE, spacing=8, cont=sp_g0)
addSpace(sp_f2, 3)
ST <- gbutton("Stop", cont=sp_f2, expand=TRUE, fill='y',
  handler=doButton, action="ST")
r_act <- gaction("Report", icon="overview", handler=function(...) lastWkUpdate(sp_rfile))
rweek <- gbutton(action=r_act, cont=sp_f2, expand=TRUE, fill='y')
e_act <- gaction("Edit", icon="editor", handler=function(...) gEditButton(sp_rfile))
Edit <- gbutton(action=e_act, cont=sp_f2, expand=TRUE, fill='y')
addSpace(sp_f2, 0.0)
f3 <- ggroup(horizontal=FALSE, spacing=10, cont=sp_g0)


sp_out <- ggroup(label='Last Week', horizontal=TRUE, 
  spacing=10, cont=notebook)
out0 <- ggroup(horizontal=TRUE, cont=sp_out)
sp_DF <- calcWeek(spDayData)
sp_o1 <- gtable(sp_DF, cont = out0, expand=FALSE)
size(sp_o1) <- list(width=220, height=220, column.widths=c(90, 60, 50, 20))

# Plots
sp_gr <- ggroup(cont=out0, horizontal=TRUE, spacing=0)
img <- doPlot(sp_rfile) 
img_out <- gimage(basename(img),dirname(img), cont = out0)

svalue(notebook) <- 1
visible(window) <- TRUE

