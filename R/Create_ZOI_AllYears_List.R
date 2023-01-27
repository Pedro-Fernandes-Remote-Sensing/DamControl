#' Organizes the argument into a list of lists. The first element is a list of dataframes that represent all the years of a particular ZOI;
#'
#' @param Albufeira_Dataframe_List a list of all the years of imagery, organized by dataframes of ZOI, of the product, usually of Binary_NDVI or Combo; The first element of that list should be a list of all imagery for Year1,
#' organized by ZOI. So, list first element = ZOI_Dataframe_List_Year1, list second element = ZOI_Dataframe_List_Year2
#'
#' @return returns a list of Dataframe lists, where the first element is a list representing all the dataframes for all the years of the first ZOI;
#' @export
Create_ZOI_AllYears_List = function(Albufeira_Dataframe_List){
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
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent creating ZOI_AllYears_List (HH:MM:SS): "), noquote(TotalTime)))
  return(List2)
}
