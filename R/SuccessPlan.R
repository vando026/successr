## Description: 
## Project: 
## Author: AV / Created: 11Jan2017 

# Packages
require(gWidgets2, quietly=TRUE, warn.conflicts = TRUE)
require(gWidgets2RGtk2, quietly=TRUE, warn.conflicts = TRUE)
require(RGtk2Extras, quietly=TRUE, warn.conflicts = FALSE)
options (guiToolkit="RGtk2" )

# Files
sp_fname<- file.path(Sys.getenv("USERPROFILE"), "Dropbox/R/SuccessPlan")
sp_tfile <- file.path(sp_fname, "TimeSheetR.csv")
sp_rfile <- file.path(sp_fname, "TimeSheet.Rdata")

today <- as.Date(Sys.time())

# load(sp_rfile)
# spData <- read.csv(sp_tfile, header=TRUE)
# spData <- transform(spData, Time=as.POSIXlt(Time, origin='1970-01-01') )
# spData <- transform(spData, Task=factor(Task, levels=c("P1", "P2", "WT", "ST")))
# save(list=c("spData", "spDayData"), file=sp_rfile)

#### Format Time 
sp_fmt <- function(x) {
  fhour <- x %/% 1
  fmin <- round((x %% 1)*0.6, 2) 
  fmin <- sub("(\\d+).(\\d+)", "\\2", format(fmin, nsmall=2))
  out <- paste(fhour, fmin, sep=':')
  ifelse(is.na(x), "0:00", as.character(out))
}
# x=sp_fmt(c(0, 0))

fmtData <- function(dat, F1="Hour~Task", byVar, days) {
  dat1 <- subset(dat, Date > (today-days))
  if(nrow(dat1)==0) {
    dat1 <- data.frame(Date=as.Date(Sys.time()), Hour=0) 
  } else {
    dat1 <- aggregate(as.formula(F1), dat1, sum, na.action=na.omit)
  }
  return(dat1)
}
# fmtData(spDayData, F1="Hour ~ Date", days=1)



calcTime <- function(dat) {
    dat <- subset(dat, as.Date(Time)==today)
    if(nrow(dat)<=1) {
      dat <- data.frame(Task="WT", Date=today, Hour=0, HourP=0)
      return(dat) 
    } 
    Seconds <- as.numeric(dat$Time)
    Hour <- round(diff(Seconds)/(60*60), 2)
    dat <- transform(dat, Hour=c(Hour, NA),
      Date=as.Date(Time))
    dat <- dat[dat$Task!="ST", ]
    dat <- aggregate(Hour ~ Task, dat, sum, na.action=na.omit)
    dat <- transform(dat, HourP=round((Hour/(sum(Hour)))*100, 1))
    as.data.frame(dat)
}


writeDay <- function(dat, spDayData) {
  dat <- subset(dat, !(Task %in% c("WT", "ST")))
  if(nrow(dat)<1) return(spDayData)
  dat <- aggregate(Hour ~ Date, dat, sum, na.action=na.omit)
  if(any(today %in% spDayData$Date)) {
    spDayData$Hour[spDayData$Date==today] <- dat$Hour
  } else {
    spDayData = rbind(spDayData, dat)
  }
  spDayData 
}
# debugonce(writeDay)
# yes <- writeDay(tt, spDayData)


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
  toggleOff <- function(obj) {
    sapply(obj, function(i) { ii <- get(i, envir=globalenv())
      font(ii) <- list(weight="normal", size=10, color="black")})
  }
  ifelse(h$action=="ST", toggleOff(ggNames), toggleOff(setdiff(ggNames, h$action)))
  if(h$action!="ST") font(h$obj) <- list(weight="bold", size=12, color="red")
   
  getLab <- function(out, gLabel) {
    out <- subset(out, Task==gLabel)
    if(nrow(out)==0) {
      Hour=sp_fmt(0); HourP="0" 
    } else {
      Hour=sp_fmt(out["Hour"]); HourP=out["HourP"]
    }
    paste0(Hour,"Hrs (",HourP,"%)")
  }
  # Write to time data file
  load(sp_rfile)
  aLine <- c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), h$action)
  spData <- rbind(spData, aLine)
  cdat <- calcTime(spData)
  spDayData <- writeDay(cdat, spDayData)
  # Update GUI
  for(i in seq(3)) {
    ii <- get(paste0(ggNames[i],"L"), envir=globalenv()) 
    svalue(ii) <- getLab(cdat, ggNames[i])
  }
  save(list=c("spData", "spDayData"), file=sp_rfile)
}

# gEditButton <- function(sp_rfile) {
#   load(sp_rfile)
#   Gedit <- gwindow("Data Editor") 
#   size(Gedit) <- list(width=80, 
#     height=300, column.widths=c(70, 30))
#   DF <- gdf(tail(spData), cont=Gedit)
#   addHandlerChanged(DF, handler = function(h ,...) {
#     newDF <- DF[]
#     dfn <- nrow(spData)
#     if(dfn<5) {
#       spData <- newDF 
#     } else {
#       spData <- rbind(spData[1:(dfn-6), ], newDF)
#     }
#   save(list=c("spData", "spDayData"), file=sp_rfile)
#   })
# }
# debugonce(gEditButton)
# gEditButton(sp_rfile)


gEditButton <- function(sp_rfile) {
  load(sp_rfile)
  win <- gtkWindowNew("Toplevel", show=FALSE)
  print(tail(spData))
  obj <- gtkDfEdit(spData, size = c(500, 500), 
    update=FALSE, modal=FALSE, autosize=FALSE, col.width=c(150, 40))
  win$setTitle("Editor")
  spData <- obj[]
  win$add(obj)
  win$show()
  spData <- transform(spData, Time=as.POSIXlt(Time, origin='1970-01-01'))
  print(tail(spData))
  save(list=c("spData", "spDayData"), file=sp_rfile)
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


