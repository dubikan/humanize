#' Humanize Time Periods
#'
#' This functions presents a time as its distance from the time the function was called, using the largest possible time period (seconds, minutes, hours, days, weeks, months, or years) that will generate an integer equal to or larger than one.
#' @param t A date or date/time pair of class POSIXct, POSIXlt, Date, or a string that can be coerced into these classes.
#' @keywords time, date, simplify
#' @export
#' @examples
#' humanize.now("1978-12-15 2:15:00 Asia/Jerusalem")

humanize.now <- function(t) {
  now<-Sys.time()
  timediff<-diff(c(as.POSIXct(t), now))
  answer<-as.numeric(timediff)
  class(answer)<- "humanize"
  attributes(answer)<- list(unit=attributes(timediff)$units)
  if (abs(timediff)>=7 & attributes(timediff)$units=="days") {
    answer<-answer/7
    attributes(answer)$unit<-"weeks"
  }
  if (abs(timediff)>=31 & attributes(timediff)$units=="days") {
    monthdiff<-lubridate::month(now)-lubridate::month(as.Date(t))+12*(lubridate::year(now)-lubridate::year(as.Date(t)))+(lubridate::day(now)-lubridate::day(as.Date(t)))/30
    answer<-monthdiff
    class(answer)<-"humanize"
    attributes(answer)$unit<-"months"
  }
  if (abs(timediff)>=365 & attributes(timediff)$units=="days") {
    answer<-as.numeric(timediff)/365
    class(answer)<-"humanize"
    attributes(answer)$unit<-"years"
  }
  answer<-trunc(answer)
  class(answer)<- "humanize"
  return(answer)
}


print.humanize <- function(h) {
  if(h>1) text<-paste0(h," ",attributes(h)$unit," ago")
  if(h<(-1)) text<-paste0(abs(h)," ", attributes(h)$unit, " from now")
  if(h==0) text<-"just now"
  if(h==1) {
    if(attributes(h)$unit=="days") text<-"yesterday" else {
      unit<-sub("s$","",attributes(h)$unit)
      text<-paste0(h," ",unit," ago")
    }
  }
  if(h==-1) {
    if(attributes(h)$unit=="days") text<-"tomorrow" else {
      unit<-sub("s$","",attributes(h)$unit)
      text<-paste0(abs(h)," ",unit," from now")
    }
  }
  print(text)
  invisible(text)
}
