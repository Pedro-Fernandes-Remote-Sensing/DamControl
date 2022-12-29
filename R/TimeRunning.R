#' A gemeral function to collect time spent doing X task. Write StartTime = Sys.time(), then your code, then EndTime = Sys.time(), then
#'
#' @param StartTime A sys.time() time representing starting time;
#' @param EndTime A sys.time() time representing ending time
#'
#' @return prints time spent;
#' @export
#'
#' @examples NULL
TimeRunning <- function(StartTime, EndTime) {
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TimeDiff = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent running (HH:MM:SS): "), noquote(TimeDiff)))
}

