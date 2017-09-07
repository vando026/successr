## Description: The Ultimate Success Plan
## Project: 
## Author: AV / Created: 11Jan2017 

# Packages
library(gWidgets2, quietly=TRUE, warn.conflicts = TRUE)
library(gWidgets2RGtk2, quietly=TRUE, warn.conflicts = TRUE)
options (guiToolkit="RGtk2" )


# Files
# This is where R will write the files to: change to your folder
sp_fname<- file.path(Sys.getenv("USERPROFILE"), "Dropbox/R/SuccessPlan")
sp_rfile <- file.path(sp_fname, "TimeSheet.Rdata")
# save(list=c("spData", "spDayData"), file=sp_rfile)
today <- as.Date(Sys.time())

load(sp_rfile)

#### Format Time 
sp_fmt <- function(x) {
  fhour <- x %/% 1
  fmin <- round((x %% 1)*0.6, 2) 
  fmin <- sub("(\\d+).(\\d+)", "\\2", format(fmin, nsmall=2))
  out <- paste(fhour, fmin, sep=':')
  out 
}

calcTime <- function(dat) {
    if(is.null(dat) || nrow(dat)==0) return(NULL)
    dat <- subset(dat, as.Date(Time)==today)
    Seconds <- as.numeric(dat$Time)
    Hour <- round(diff(Seconds)/(60*60), 2)
    dat <- transform(dat, Hour=c(Hour, NA),
      Date=as.Date(Time))
    dat <- subset(dat, Task!="ST")
    if(nrow(dat)<2) return(NULL)
    dat <- aggregate(Hour ~ Task + Date, dat, sum, na.action=na.omit)
    dat <- transform(dat, HourP=round((Hour/sum(Hour))*100, 1))
    dat 
}
# debugonce(calcTime)
# dat=calcTime(spData)

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
  if(!is.null(dat)) { 
    dat <- subset(dat, Task!="WT")
    Hour <- aggregate(Hour ~ Date, dat, sum)$Hour
    if (isToday==TRUE) {
      spDayData$Hour[spDayData$Date==today] <- Hour
    } else {
      newLine <- data.frame(Date=today, Hour=Hour)
      spDayData <-  rbind(spDayData, newLine) 
    }
  } else if(is.null(dat)) {
    if (isToday==TRUE) {
      spDayData$Hour[spDayData$Date==today] <- 0
    } else {
      newLine <- data.frame(Date=today, Hour=0)
      spDayData <-  rbind(spDayData, newLine) 
    } 
  }
  spDayData 
}
# debugonce(writeDay)
# yes <- writeDay(tt, spDayData)

calcWeek <- function(dat) {
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
# spDayData1 <- calcWeek(spDayData)

calcMonth <- function(dat) {
  dat <- transform(dat, Week=as.numeric(format(Date, "%U")))
  lastMnth <- max(dat$Week)
  dat4 <- data.frame(Week =c((lastMnth-3):lastMnth))
  dat <- subset(dat, Date > (today-28))
  dat <- aggregate(Hour ~ Week, data=dat, sum)
  dat <- merge(dat4, dat, by="Week", all.x=TRUE)
  dat <- transform(dat, 
    Hour=ifelse(!is.na(Hour), Hour, 0.00),
    HourF=ifelse(!is.na(Hour), sp_fmt(Hour), "0:00"),
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
# doPlot(sp_rfile)


callCalc <- function(spData) {
  cdat <- calcTime(spData)
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
  toggleOff(ggNames)
  if(h$action!="ST") font(h$obj) <- list(weight="bold", size=12, color="red")
   
  # Write to time data file
  load(sp_rfile)
  aLine <- data.frame(Time=Sys.time(), Task=h$action)
  spData <- rbind(spData, aLine)
  callCalc(spData)
}

gEditButton <- function(sp_rfile) {
  load(sp_rfile)
  Gedit <- gwindow("Data Editor") 
  size(Gedit) <- list(width=80, 
    height=300, column.widths=c(70, 30))
  dfi <- 30
  rownames(spData) <- seq(nrow(spData))
  DF <- gdf(spData, cont=Gedit)
  addHandlerChanged(DF, handler = function(h ,...) {
    spData <- data.frame(DF[])
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
  width=620, height=240, visible=FALSE)

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
size(sp_o1) <- list(width=250, height=220, column.widths=c(90, 70, 50, 40))
addSpace(sp_out, 1)

# Plots
sp_gr <- ggroup(cont=out0, horizontal=TRUE, spacing=0)
img <- doPlot(sp_rfile) 
img_out <- gimage(basename(img),dirname(img), cont = out0)

# doButton("Stop", action="ST")

svalue(notebook) <- 1
visible(window) <- TRUE

