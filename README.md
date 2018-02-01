# successr
R code for the Ultimate Success Plan

You cannot be successful if you waste time. The Ultimate Success Plan is a
tool for diagnosing the amount of time you waste during the day. Its aim is to finally
convince you that you don't really work 8 hours a day.  At a metaphysical level, time
can be divided into two categories: work time and wasted time. Work time is effectively
used when you focus on at most two projects during the day that you think are relevant.
_Wasted Time_ includes anything that does not fall in _Project 1_ and/or
_Project 2_.  Culprits for _Wasted Time_ are typically
toilet/cigarette/watercooler excursions, lunch breaks, unannounced visits by the boss,
any meeting, internet surfing, chit chat with colleagues, or courtesy calls by
telemarketers, friends/family, and partner(s).

Clicking _Project 1_, Project 2, and \code{Wasted Time} starts the timer,
and \code{Stop} stops the timer. \code{Report} gives a daily, weekly, monthly breakdown
of work time. \code{Edit} allows you to edit only the time recorded during the day and
the Task (from a drop-down menu).  \code{DayData.csv} allows you to edit the total time
for a given day (see below). 

Upon installation, successr will setup a data folder (where the data is stored) with a
configuration file, called \code{config.yml} (see \code{\link{config}}). The folder can
be found in \code{Sys.getenv("HOME")}. If you want to specify an alternative folder
path, then you must put in your .Rprofile (see \code{\link{Startup}}) the following
line: \code{Sys.setenv(R_SUCCESS="my/folder/path")}. In my Rprofile set-up, I have
\code{Sys.setenv("~/Dropbox/vando026/successr")} so I can sync between my work and home
computers. Labels for \code{Project 1}, \code{Project 2}, \code{Wasted Time} and
the \code{Window Title} can be changed in the \code{config.yml} file. But don't
waste time doing this, just use the default settings.  

