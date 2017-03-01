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


##### Bring in the Data
sp_getData <- function(sp_tfile) {
  dat <- as.data.frame(read.csv(sp_tfile, header=FALSE))
  colnames(dat) <- c("Time", "Task")
  dat <- transform(dat, Time=as.POSIXlt(Time, origin='1970-01-01') )
  dat$Date <- as.character(format(dat$Time,  "%Y-%m-%d"))
  return(dat)
}

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
Select <- function(dat, 
  days=eval.parent(quote(days))) {
  today <- as.Date(format(Sys.time(), "%Y-%m-%d"))
  dat <- transform(dat, Date=as.Date(Date))
  ds <- today - days
  dat <- subset(dat, Date >= ds)
  dat
}
# debugonce(Select)
# Select(dat, 0)

# Calc hours 
calcTime <- function(dat, 
    days=eval.parent(quote(days))) {
    dat <- sp_getData(sp_tfile)
    Seconds <- as.numeric(dat$Time)
    Hour <- round(diff(Seconds)/(60*60), 2)
    hdat <- transform(dat, Hour=c(Hour, NA))
    out <- aggregate(Hour ~ Task + Date, hdat, sum, na.action=na.omit)
    out <- out[out$Task!="Stop", ]
    out <- Select(out, days)
    out <- transform(out, HourF=sp_fmt(Hour))
    out <- transform(out, HourP=round((Hour/(sum(Hour)))*100, 1))
    out <- as.data.frame(out)
    return(out)
}
# calcTime(dat, 0)

calcTimeDF <- function(sp_tfile, days=7, sp_fmt=TRUE) {
  dat <- sp_getData(sp_tfile)
  dat <- calcTime(dat, days) 
  dat <- subset(dat, Task != "wt")
  wkday <- aggregate(Hour ~ Date, dat, sum, na.action=na.omit)
  if (sp_fmt==TRUE)
    wkday <- transform(wkday, Hour=sp_fmt(Hour))
  wkday <- transform(wkday, Day = format(Date, "%a"))
  wkday <- transform(wkday, Date=as.character(Date))
  wkday <- as.data.frame(wkday[, c("Date", "Day", "Hour")])
  wkday
}
# debugonce(calcTimeDF)
# qp_DF <- calcTimeDF(sp_tfile, 7, sp_fmt=FALSE)

weekPlot <- function(sp_tfile, days=31) {
  cur_mnth <- format(Sys.time(), "%m-%Y")
  dat <- calcTimeDF(sp_tfile, days, sp_fmt=FALSE)
  dat <- transform(dat, Date=as.Date(Date, origin="1970-01-01"))
  dat <- subset(dat, format(Date, "%m-%Y")==cur_mnth)
  dat <- transform(dat, Day=as.numeric(format(Date, "%d")))
  dat <- transform(dat, Week=cut(Day, 
    include.lowest=TRUE, breaks=seq(0, days, 7),
    labels=paste0("Week", 1:4)))
  if (nrow(dat)!=0) {
    dat <- aggregate(Hour ~ Week, data=dat, sum)
  } else {
    dat <- data.frame(Week=paste0("Week", 1:4), Hour=rep(0, 4)) 
  }
  dat <- transform(dat, HourF=sp_fmt(Hour))
  dat
}
# debugonce(weekPlot)
# tt <- weekPlot(sp_tfile)

doPlot <- function(sp_tfile) {
  tt <- weekPlot(sp_tfile)
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
  sp_DF <- calcTimeDF(sp_tfile)
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
sp_DF <- calcTimeDF(sp_tfile, sp_fmt=TRUE)
sp_o1 <- gtable(sp_DF, cont = out0, expand=FALSE)
size(sp_o1) <- list(width=200, height=220, column.widths=c(100, 50, 50))

# Plots
sp_gr <- ggroup(cont=out0, horizontal=TRUE, spacing=0)
img <- doPlot(sp_tfile) 
img_out <- gimage(basename(img),dirname(img), cont = out0 )


svalue(notebook) <- 1
visible(window) <- TRUE


