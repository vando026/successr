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
# spData<- as.data.frame(read.csv(sp_tfile, header=TRUE))
# spData <- transform(spData, Time=as.POSIXlt(Time, origin='1970-01-01') )
# save("spData", file=sp_rfile)

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


### Select days to backcalc
sp_select <- function(dat, 
  days=eval.parent(quote(days))) {
  ds <- as.Date(Sys.time()) - days
  dat <- subset(dat, as.Date(Time) > ds)
  dat
}
# debugonce(sp_select)
sp_select(spData, 2)

# Calc hours 
calcTime <- function(dat, 
    days=eval.parent(quote(days))) {
    dat <- sp_select(dat, days)
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
calcTime(spData, 1)

calcWeek <- function(dat, days=7, sp_fmt=TRUE) {
  dat <- calcTime(dat, days) 
  dat <- subset(dat, Task != "wt")
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
# qp_DF <- calcWeek(spData, 7, sp_fmt=FALSE)

calcMonth <- function(dat, days=31) {
  dat <- calcWeek(dat, days, sp_fmt=FALSE)
  dat <- transform(dat, Wk=as.numeric(format(Date, "%U")))
  maxWk <- max(dat$Wk)
  dat <- subset(dat, Wk %in% c((maxWk-3):maxWk))
  dat <- aggregate(Hour ~ Week, data=dat, sum)
  dat <- transform(dat, HourF=sp_fmt(Hour))
  dat
}
# debugonce(calcMonth)
tt <- calcMonth(spData)

doPlot <- function(sp_tfile) {
  tt <- calcMonth(sp_tfile)
  f <- tempfile( )
  png(f, units="in",
       width=3.6, height=1.8, pointsize=8, res=100, type="cairo")
  par(mar=c(2.7, 5.0, 0, 0.4))
  with(tt, barplot(Hour, horiz=TRUE, col=rep(c("seagreen1", "seagreen3"), 2), 
    names.arg=Week, las=1))
  with(tt, text(Hour , c(0.7, 1.8, 3, 4.2), labels=HourF, pos=2, adj=1))
  dev.off()
  f
}
# doPlot(sp_tfile)


dumpData <- function(sp_tfile) {
    dat <- sp_getData(sp_tfile)
    dat <- sp_select(dat, 31)
    write.csv(dat[, c("Time", "Task")], 
      file=sp_tfile,
      row.names=FALSE,
      quote=FALSE) 
}
# debugonce(dumpData)
# dumpData(sp_tfile)



doButton <- function(x, days=0) {
  All <- c("proj1", "proj2", "wt")
  other <- setdiff(All, x)
  dflt <- list(weight="normal", size=10, color="black")
  Act <- list(weight="bold", size=12, color="red")
  A1 <- get(x, envir=environment())
  N1 <- get(other[1], envir=environment())
  N2 <- get(other[2], envir=environment())
  font(A1) <- Act
  font(N1)<- dflt
  font(N2)<- dflt
  if (x=="Stop") {
    font(Stop) <- dflt
    font(proj1)<- dflt
    font(proj2) <-dflt
    font(wt)<- dflt
  }
  aLine <- paste(Sys.time(), x, sep=',')
  outfile <- file(sp_tfile, "a")
  writeLines(aLine, con=outfile)
  close(outfile)
  
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
  cdat <- calcTime(dat)
  updateData(cdat)
}

gEditButton <- function() {
  system(paste("gvim --remote ", sp_tfile))
}

lastWkUpdate <- function(sp_tfile) {
  sp_DF <- calcWeek(sp_tfile)
  sp_o1[] <- sp_DF
  svalue(img_out) <- doPlot(sp_tfile)
  svalue(notebook) <- 2
}

###############################################################################################
######################################## GUI ##################################################
###############################################################################################
window <- gwindow("The Ultimate Success Plan", 
  width=620, height=230, visible=FALSE)

# This makes the tabs
notebook <- gnotebook (cont = window )

sp_g0 <- ggroup(label='Main', horizontal=TRUE, spacing=10, cont=notebook)


sp_f0 <- ggroup(horizontal=FALSE, spacing=0, cont=sp_g0)
sp_f1 <- ggroup(horizontal=TRUE, expand=TRUE, fill='x', 
  spacing=10, cont=sp_g0)

sp_g1 <- ggroup(horizontal = FALSE, expand=TRUE, fill='x', spacing=5,  cont = sp_f1)
addSpace(sp_g1, 5)
proj1 <- gbutton("Project 1", cont = sp_g1, expand=TRUE, fill='y',
  handler=function(h, ...) doButton("proj1", days=0)) 
size(proj1) <- c(70, 100)
sep <- gseparator(cont=sp_g1)
outP1 <- glabel("", cont=sp_g1, expand=FALSE)
addSpace(sp_g1, 5)

sp_g2 <- ggroup(horizontal = FALSE, spacing=5, expand=TRUE, fill='x', cont = sp_f1)
addSpace(sp_g2, 5)
proj2 <- gbutton("Project 2", cont = sp_g2, expand=TRUE, fill='y',
  handler=function(h, ...) doButton("proj2", days=0))
size(proj2) <- c(70, 100)
sep <- gseparator(cont=sp_g2)
outP2 <- glabel("", cont=sp_g2)
addSpace(sp_g2, 5)

sp_g3 <- ggroup(horizontal = FALSE, spacing=5, expand=TRUE, fill='x',  cont = sp_f1)
addSpace(sp_g3, 5)
wt <- gbutton("Wasted Time", cont= sp_g3, expand=TRUE, fill='y',
  handler=function(h, ...) doButton("wt", days=0)) 
size(wt) <- c(70, 100)
sep <- gseparator(cont=sp_g3)
outWT <- glabel("", cont=sp_g3)
addSpace(sp_g3, 5)

sp_f2 <- ggroup(horizontal=FALSE, spacing=8, cont=sp_g0)
addSpace(sp_f2, 3)
Stop <- gbutton("Stop", cont=sp_f2, expand=TRUE, fill='y',
  handler=function(h, ...) doButton("Stop"))
r_act <- gaction("Report", icon="overview", handler=function(...) lastWkUpdate(sp_tfile))
rweek <- gbutton(action=r_act, cont=sp_f2, expand=TRUE, fill='y')
e_act <- gaction("Edit", icon="editor", handler=function(...) gEditButton())
Edit <- gbutton(action=e_act, con=sp_f2, expand=TRUE, fill='y')
addSpace(sp_f2, 1.3)
f3 <- ggroup(horizontal=FALSE, spacing=10, cont=sp_g0)

sp_out <- ggroup(label='Last Week', horizontal=TRUE, 
  spacing=10, cont=notebook)
out0 <- ggroup(horizontal=TRUE, cont=sp_out)
doButton("wt", days=0)
sp_DF <- calcWeek(sp_tfile, sp_fmt=TRUE)
sp_o1 <- gtable(sp_DF, cont = out0, expand=FALSE)
size(sp_o1) <- list(width=200, height=220, column.widths=c(100, 50, 50))

# Plots
sp_gr <- ggroup(cont=out0, horizontal=TRUE, spacing=0)
img <- doPlot(sp_tfile) 
img_out <- gimage(basename(img),dirname(img), cont = out0 )

# Trim data
dumpData(sp_tfile)

svalue(notebook) <- 1
visible(window) <- TRUE


