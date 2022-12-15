PlotAllYears_All_Albufeiras = function(Dataframe_List_All, Path){
  StartTime = Sys.time()
  Iterator = 1
  for (Albufeira in Dataframe_List_All){
    MegaDataframe = bind_rows(Dataframe_List_All[[Iterator]])
    Melted_MegaDataframe = melt(MegaDataframe, id.vars=c("Year","Month", "WaterPixels", "NotWaterPixels", "TotalPixel", "TotalWaterAreaM2", "TotalAreaM2", "WaterPercentage"))
    png(paste0(Path, "/Albufeira_", Iterator, ".png"), width = 2560, height = 1440, res = 200)
    print(ggplot(Melted_MegaDataframe, aes(x = factor(Month, level = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1)) + geom_area(colour = "black", fill = "lightblue", alpha = 0.5) + geom_line() + geom_point(size = 3, shape = 21, fill = 'black') + ggtitle(paste0("Conteudo de Agua na Albufeira ", Iterator)) + theme(plot.title = element_text(hjust = 0.5, vjust = 1.5), axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1)) + ylab("Ocupacao da Albufeira (%)") + xlab(NULL) + scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 5)) + facet_grid(~ Year))
    dev.off()
    Iterator = Iterator + 1
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent plotting (HH:MM:SS): "), noquote(TotalTime)))
}

PlotAllYears_All_Albufeiras2 = function(Dataframe_List_All, Dataframe_List_All_2, Path){
  StartTime = Sys.time()
  Iterator = 1
  for (Albufeira in Dataframe_List_All){
    MegaDataframe = bind_rows(Dataframe_List_All[[Iterator]])
    MegaDataframe2 = bind_rows(Dataframe_List_All_2[[Iterator]])
    Melted_MegaDataframe = melt(MegaDataframe, id.vars=c("Year","Month", "WaterPixels", "NotWaterPixels", "TotalPixel", "TotalWaterAreaM2", "TotalAreaM2", "WaterPercentage"))
    Melted_MegaDataframe2 = melt(MegaDataframe2, id.vars=c("Year","Month", "WaterPixels", "NotWaterPixels", "TotalPixel", "TotalWaterAreaM2", "TotalAreaM2", "WaterPercentage"))
    png(paste0(Path, "/Albufeira_", Iterator, ".png"), width = 2560, height = 1440, res = 200)
    print(ggplot() +
            geom_area(data = Melted_MegaDataframe2, mapping = aes(x = factor(Month, level = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1), colour = "black", fill = "#85e085", alpha = 0.5) + 
            geom_line(data = Melted_MegaDataframe2, mapping = aes(x = factor(Month, level = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1)) +
            geom_point(data = Melted_MegaDataframe2, mapping = aes(x = factor(Month, level = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1)) +
            geom_area(data = Melted_MegaDataframe, mapping = aes(x = factor(Month, level = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1), colour = "black", fill = "#ff0000", alpha = 0.5) + 
            geom_line(data = Melted_MegaDataframe, mapping = aes(x = factor(Month, level = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1)) +
            geom_point(data = Melted_MegaDataframe, mapping = aes(x = factor(Month, level = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1)) +
            ggtitle(paste0("Conteudo de Agua na Albufeira ", Iterator)) +
            theme(plot.title = element_text(hjust = 0.5, vjust = 1.5), axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1)) +
            ylab("Ocupacao da Albufeira (%)") +
            xlab(NULL) + scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 5)) +
            facet_grid(~ Year))
    dev.off()
    Iterator = Iterator + 1
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent plotting (HH:MM:SS): "), noquote(TotalTime)))
}
