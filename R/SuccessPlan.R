## Description: 
## Project: 
## Author: AV / Created: 11Jan2017 

# Packages
require(gWidgets2, quietly=TRUE, warn.conflicts = TRUE)
require(gWidgets2RGtk2, quietly=TRUE, warn.conflicts = TRUE)
options (guiToolkit="RGtk2" )

# Files
sp_fname<- file.path(Sys.getenv("USERPROFILE"), "Dropbox/R/SuccessPlan")
sp_tfile <- file.path(sp_fname, "TimeSheet.csv")
sp_rfile <- file.path(sp_fname, "TimeSheet.Rdata")

##### Bring in the Data
load(sp_rfile)

##### Format Time 
sp_fmt <- function(x) {
  if(is.numeric(x)==FALSE) stop("Must be numeric")
  fhour <- x %/% 1
  fmin <- round((x %% 1)*0.6, 2) 
  fmin <- sub("(\\d+).(\\d+)", "\\2", format(fmin, nsmall=2))
  out <- paste(fhour, fmin, sep=':')
  out
}
# sp_fmt(2.5)

# Calc hours 
calcTime <- function(dat) {
    today <- as.Date(Sys.time()) 
    dat <- subset(dat, as.Date(Time)==today)
    Seconds <- as.numeric(dat$Time)
    Hour <- round(diff(Seconds)/(60*60), 2)
    dat <- transform(dat, Hour=c(Hour, NA),
      Date=as.Date(Time))
    dat <- aggregate(Hour ~ Task + Date, dat, sum, na.action=na.omit)
    dat <- dat[dat$Task!="Stop", ]
    dat <- transform(dat, HourF=sp_fmt(Hour))
    dat <- transform(dat, HourP=round((Hour/(sum(Hour)))*100, 1))
    as.data.frame(dat)
}
# debugonce(calcTime)
# tt=calcTime(spData)

writeDay <- function(dat, spDayData) {
  today <- as.Date(Sys.time()) 
  dat <- subset(dat, !(Task %in% c("wt", "Stop")))
  dat <- aggregate(Hour ~ Date, dat, sum, na.action=na.omit)
  if(any(today %in% spDayData$Date)) {
    spDayData$Hour[spDayData$Date==today] <- dat$Hour
  } else {
    spDayData = rbind(spDayData, day)
  }
  # spData <- subset(spData, as.Date(Time)==today)
  # save(list=c("spData", "spDayData"), 
    # file=sp_rfile)
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

calcWeek <- function(dat, sp_fmt=TRUE) {
  dat <- sp_select(dat, 7)
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
# qp_DF <- calcWeek(spDayData, sp_fmt=FALSE)

calcMonth <- function(dat) {
  dat <- sp_select(dat, 31)
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

doPlot <- function(spDayData) {
  out <- calcMonth(spDayData)
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
# doPlot(spDayData)


doButton <- function(h, ...) {
  print(h)
  return(h)
  if(x==ggLabels[1] )
  other <- setdiff(ggLabels, x)
  dflt <- list(weight="normal", size=10, color="black")
  Act <- list(weight="bold", size=12, color="red")
  font(xn) <- Act
  sapply(other, function(i) font(i) <- dflt)
  if (x=="Stop") 
    sapply(All, function(i) font(i) <- dflt)
  
  ### Update GUI
  updateData <- function(out) {
    svalue(outP1) <- paste0(out[out$Task=="proj1", "HourF"], " Hrs (", 
      out[out$Task=="proj1", "HourP"], "%)")
    svalue(outP2) <- paste0(out[out$Task=="proj2", "HourF"], " Hrs (",
      out[out$Task=="proj2", "HourP"], "%)")
    svalue(outWT) <- paste0(out[out$Task=="wt", "HourF"], " Hrs (",
      out[out$Task=="wt", "HourP"], "%)")
  }

  # Run functions
  load(sp_rfile)
  aLine <- c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), x)
  spData <- rbind(spData, aLine)
  cdat <- calcTime(spData)
  updateData(cdat)
  spDayData <- writeDay(cdat, spDayData)
  print(tail(spData))
  print(tail(spDayData))
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
  svalue(notebook) <- 2
}

lastWkUpdate <- function(spDayData) {
  sp_DF <- calcWeek(spDayData)
  sp_o1[] <- sp_DF
  svalue(img_out) <- doPlot(spDayData)
  svalue(notebook) <- 3
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
ggNames <- paste0("P", 1:3 )
ggLabels <- list(P1="Project 1", P2 = "Project 2", P3="Wasted Time")
doButtonList <- list()

# Now do all settings for buttons
for(i in seq(3)) {
  assign(paste0(ggNames[i],"G"), 
    do.call("ggroup", ggList))
  bi <- get(ggNames[i], envir=environment())
  doButtonList[[ggNames[i]]] <-
    gbutton(ggLabels[[i]], cont=bi, expand=TRUE, fill="y")
  addHandlerChanged(bi, handler=doButton)
  size(bi) <- c(70, 100)
  sep <- gseparator(cont=bi)
  assign(paste0(ggNames[i],"L"), 
    glabel("", cont=bi))
  addSpace(bi, 5)
}

###############################################################################################
######################################## HANDLERS #############################################
###############################################################################################
# sapply(c(1:3), function(x)
  # addHandlerChanged(, handler = doButton, action=list(doName="Yes")))

# addSpace(sp_g1, 5)
# sp_g1
# doButtonList$P1
# size(sp_g1) <- c(70, 100)
# sep <- gseparator(cont=sp_g1)
# addSpace(sp_g1, 5)

# sp_g2 <- ggroup(horizontal = FALSE, spacing=5, expand=TRUE, fill='x', cont = sp_f1)
# addSpace(sp_g2, 5)
# proj2 <- gbutton("Project 2", cont = sp_g2, expand=TRUE, fill='y',
#   handler=function(h, ...) doButton("proj2"))
# size(sp_g2) <- c(70, 100)
# sep <- gseparator(cont=sp_g2)
# outP2 <- glabel("", cont=sp_g2)
# addSpace(sp_g2, 5)

# sp_g3 <- ggroup(horizontal = FALSE, spacing=5, expand=TRUE, fill='x',  cont = sp_f1)
# addSpace(sp_g3, 5)
# wt <- gbutton("Wasted Time", cont= sp_g3, expand=TRUE, fill='y',
#   handler=function(h, ...) doButton("wt")) 
# size(sp_g3) <- c(70, 100)
# sep <- gseparator(cont=sp_g3)
# outWT <- glabel("", cont=sp_g3)
# addSpace(sp_g3, 5)

# sp_f2 <- ggroup(horizontal=FALSE, spacing=8, cont=sp_g0)
# addSpace(sp_f2, 3)
# Stop <- gbutton("Stop", cont=sp_f2, expand=TRUE, fill='y',
#   handler=function(h, ...) doButton("Stop"))
# r_act <- gaction("Report", icon="overview", handler=function(...) lastWkUpdate(sp_tfile))
# rweek <- gbutton(action=r_act, cont=sp_f2, expand=TRUE, fill='y')
# e_act <- gaction("Edit", icon="editor", handler=function(...) gEditButton())
# Edit <- gbutton(action=e_act, cont=sp_f2, expand=TRUE, fill='y')
# addSpace(sp_f2, 1.3)
f3 <- ggroup(horizontal=FALSE, spacing=10, cont=sp_g0)


# addHandlerChanged(e_act, handler = function(h ,...) {
#   print("YESSSS")
# })

# sp_out <- ggroup(label='Last Week', horizontal=TRUE, 
#   spacing=10, cont=notebook)
# out0 <- ggroup(horizontal=TRUE, cont=sp_out)
# doButton("wt", days=0)
# sp_DF <- calcWeek(sp_tfile, sp_fmt=TRUE)
# sp_o1 <- gtable(sp_DF, cont = out0, expand=FALSE)
# size(sp_o1) <- list(width=200, height=220, column.widths=c(100, 50, 50))

# # Plots
# sp_gr <- ggroup(cont=out0, horizontal=TRUE, spacing=0)
# img <- doPlot(sp_tfile) 
# img_out <- gimage(basename(img),dirname(img), cont = out0 )

# Trim data
# dumpData(sp_tfile)

svalue(notebook) <- 1
visible(window) <- TRUE


