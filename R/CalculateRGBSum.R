CalculateRGBSum = function(BrickList){
  StartTime = Sys.time()
  RGB_Sum_List = list()
  Temporary_List = list()
  Iterator = 1
  for (Brick in BrickList){
    Temporary_List[[1]] = Brick[[1]]
    Temporary_List[[2]] = Brick[[2]]
    Temporary_List[[3]] = Brick[[3]]
    Temporary_Stack = stack(Temporary_List)
    RGB_Sum_List[[Iterator]] = calc(Temporary_Stack, sum)
    print(noquote(paste0("Calculated RGB Sum ", Iterator)))
    Iterator = Iterator + 1
  }
  print(noquote(paste0("Calculated All RGB Sums")))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent Calculating Sum of RGB (HH:MM:SS): "), noquote(TotalTime)))
  return(RGB_Sum_List)
}

