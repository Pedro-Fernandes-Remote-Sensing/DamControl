Create_Albufeiras_AllYears_List = function(Albufeira_Dataframe_List){
  StartTime = Sys.time()
  Iterator = 1
  Iterator2 = 1
  List1 = list()
  List2 = list()
  for (i in 1:length(Albufeira_Dataframe_List[[1]])){
    Iterator2 = 1
    for (Year in Albufeira_Dataframe_List){
      List1[Iterator2] = Year[Iterator]
      Iterator2 = Iterator2 + 1
    }
    List2[[Iterator]] = List1
    Iterator = Iterator + 1 
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent creating Albufeiras_AllYears_List (HH:MM:SS): "), noquote(TotalTime)))
  return(List2)
}
