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

# spData <- read.csv(sp_tfile, header=TRUE)
# spData <- transform(spData, Time=as.POSIXlt(Time, origin='1970-01-01') )
# spData <- transform(spData, Task=factor(Task, levels=c("P1", "P2", "WT", "ST")))

##### Bring in the Data
load(sp_rfile)

##### Format Time 
sp_fmt <- function(x) {
  fhour <- x %/% 1
  fmin <- round((x %% 1)*0.6, 2) 
  fmin <- sub("(\\d+).(\\d+)", "\\2", format(fmin, nsmall=2))
  out <- paste(fhour, fmin, sep=':')
  ifelse(is.na(x), "0:00", as.character(out))
}
# x=sp_fmt(c(0, 0))

# Calc hours 
calcTime <- function(dat) {
    today <- as.Date(Sys.time()) 
    dat <- subset(dat, as.Date(Time)==today)
    if(nrow(dat)<=1) {
      dat <- data.frame(Task="P1", Date=today, Hour=0, HourP=0)
      return(dat) 
    } 
    Seconds <- as.numeric(dat$Time)
    Hour <- round(diff(Seconds)/(60*60), 2)
    dat <- transform(dat, Hour=c(Hour, NA),
      Date=as.Date(Time))
    dat <- aggregate(Hour ~ Task + Date, dat, sum, na.action=na.omit)
    dat <- dat[dat$Task!="ST", ]
    dat <- transform(dat, HourP=round((Hour/(sum(Hour)))*100, 1))
    as.data.frame(dat)
}
# debugonce(calcTime)
# tt=calcTime(spData)

writeDay <- function(dat, sp_rfile) {
  load(sp_rfile)
  today <- as.Date(Sys.time()) 
  dat <- subset(dat, !(Task %in% c("WT", "ST")))
  dat <- aggregate(Hour ~ Date, dat, sum, na.action=na.omit)
  if(any(today %in% spDayData$Date)) {
    spDayData$Hour[spDayData$Date==today] <- dat$Hour
  } else {
    spDayData = rbind(spDayData, dat)
  }
  return(spDayData)
}
# debugonce(writeDay)
# yes <- writeDay(tt, spDayData)


### Select days to backcalc
sp_select <- function(dat, 
  days=eval.parent(quote(days))) {
  ds <- as.Date(Sys.time()) - days
  dat <- subset(dat, Date  > ds)
  dat
}
# debugonce(sp_select)
# sp_select(yes, 7)

calcWeek <- function(sp_rfile, sp_fmt=TRUE) {
  load(sp_rfile)
  dat <- sp_select(spDayData, 7)
  dat <- aggregate(Hour ~ Date, dat, sum, na.action=na.omit)
  if (sp_fmt==TRUE)
    dat <- transform(dat, Hour=sp_fmt(Hour))
  dat <- transform(dat, Day = format(Date, "%a"))
  dat <- transform(dat, Week=paste0("Week",
    as.numeric(format(Date, "%U"))))
  dat <- as.data.frame(dat[, c("Date", "Week", "Day", "Hour")])
  dat
}
# debugonce(calcWeek)
# spDayData1 <- calcWeek(sp_rfile, sp_fmt=FALSE)

calcMonth <- function(sp_rfile) {
  load(sp_rfile)
  dat <- sp_select(spDayData, 31)
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
  out <- calcMonth(sp_rfile)
  f <- tempfile( )
  png(f, units="in",
       width=3.6, height=1.8, pointsize=8, res=100, type="cairo")
  par(mar=c(2.7, 5.0, 0, 0.4))
  with(out, barplot(Hour, horiz=TRUE, col=rep(c("seagreen1", "seagreen3"), 2), 
    names.arg=Week, las=1))
  with(out, text(Hour , c(0.7, 1.8, 3, 4.2), labels=HourF, pos=2, adj=1))
  dev.off()
  f
}
# doPlot(sp_rfile)


doButton <- function(h, ...) {
  other <- setdiff(paste0(ggNames, "B"), h$action)
  dflt <- list(weight="normal", size=10, color="black")
  Act <- list(weight="bold", size=12, color="red")
  if (h$action=="ST") {
    font(h$obj) <- dflt
  } else {
    font(h$obj) <- Act
    for(i in other) {
      ii <- get(i, envir=globalenv())
      font(ii) <- dflt
    }
  } 

  getLab <- function(out, gLabel) {
    out <- subset(out, Task==gLabel)
    if(nrow(out)==0) {
      Hour=sp_fmt(0); HourP="0" 
    } else {
      Hour=sp_fmt(out["Hour"]); HourP=out["HourP"]
    }
    paste0(Hour,"Hrs (",HourP,"%)")
  }
  load(sp_rfile)
  # Write to time data file
  aLine <- c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
    substr(h$action,1,2))
  spData <- rbind(spData, aLine)
  # Write to day data file
  cdat <- calcTime(spData)
  spDayData <- writeDay(cdat, sp_rfile)
  # Update GUI
  for(i in seq(3)) {
    ii <- get(paste0(ggNames[i],"L"), envir=globalenv()) 
    svalue(ii) <- getLab(cdat, ggNames[i])
  }
  save(list=c("spData", "spDayData"), file=sp_rfile)
}

gEditButton <- function() {
  load(sp_rfile)
  Gedit <- gwindow("Data Editor") 
  size(Gedit) <- list(width=80, height=300, column.widths=c(70, 30))
  DF <- gdf(tail(spData), cont=Gedit)
  addHandlerChanged(DF, handler = function(h ,...) {
    print("YESSSS")
  })
  # svalue(notebook) <- 2
}

lastWkUpdate <- function(sp_rfile) {
  sp_DF <- calcWeek(sp_rfile)
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
  assign(paste0(ggNames[i],"B"),
    gbutton(ggLabels[i], cont=gi, expand=TRUE, fill="y"))
  bi <- get(paste0(ggNames[i],"B"), envir=environment())
  addHandlerChanged(bi, handler=doButton, action=paste0(ggNames[i],"B"))
  size(gi) <- c(70, 100)
  sep <- gseparator(cont=gi)
  assign(paste0(ggNames[i],"L"), 
    glabel("", cont=gi))
  addSpace(gi, 2)
}
# AllG <- mget(ls(ggNames))

###############################################################################################
######################################## HANDLERS #############################################
###############################################################################################

sp_f2 <- ggroup(horizontal=FALSE, spacing=8, cont=sp_g0)
addSpace(sp_f2, 3)
ST <- gbutton("Stop", cont=sp_f2, expand=TRUE, fill='y')
addHandlerChanged(ST, handler=doButton, action="ST")
r_act <- gaction("Report", icon="overview", handler=function(...) lastWkUpdate(sp_rfile))
rweek <- gbutton(action=r_act, cont=sp_f2, expand=TRUE, fill='y')
e_act <- gaction("Edit", icon="editor", handler=function(...) gEditButton())
Edit <- gbutton(action=e_act, cont=sp_f2, expand=TRUE, fill='y')
addSpace(sp_f2, 0.0)
f3 <- ggroup(horizontal=FALSE, spacing=10, cont=sp_g0)


sp_out <- ggroup(label='Last Week', horizontal=TRUE, 
  spacing=10, cont=notebook)
out0 <- ggroup(horizontal=TRUE, cont=sp_out)
# doButton(h=list(obj=WTB, action="WT"))
sp_DF <- calcWeek(sp_rfile, sp_fmt=TRUE)
sp_o1 <- gtable(sp_DF, cont = out0, expand=FALSE)
size(sp_o1) <- list(width=240, height=220, column.widths=c(90, 50, 50, 50))

# Plots
sp_gr <- ggroup(cont=out0, horizontal=TRUE, spacing=0)
img <- doPlot(sp_rfile) 
img_out <- gimage(basename(img),dirname(img), cont = out0)

svalue(notebook) <- 1
visible(window) <- TRUE


