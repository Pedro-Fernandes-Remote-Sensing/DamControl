if(getRversion() >= "2.15.1")  utils::globalVariables(c("Month","WaterPercentage"))
#' Plots and exports Water Percentage of the input Dataframes. Produces several plots of a time series of Water Percentage of all the ZOI included in the arguments;
#'
#' @param Dataframe_List_All a list of lists, with the first element being a list of dataframes. Each dataframe represents a Year of of Product for ZOI 1;
#' @param Path a string representing an export path to export the graphs;
#'
#' @return Doesnt return anything, used to plot graphs and export them.
#' @importFrom dplyr bind_rows
#' @importFrom reshape2 melt
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_area
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 facet_grid
#' @export
PlotAllYears_All_Albufeiras = function(Dataframe_List_All, Path){
  StartTime = Sys.time()
  Iterator = 1
  for (Albufeira in Dataframe_List_All){
    MegaDataframe = dplyr::bind_rows(Dataframe_List_All[[Iterator]])
    Melted_MegaDataframe = reshape2::melt(MegaDataframe, id.vars=c("Year","Month", "WaterPixels", "NotWaterPixels", "TotalPixel", "TotalWaterAreaM2", "TotalAreaM2", "WaterPercentage"))
    grDevices::png(paste0(Path, "/Albufeira_", Iterator, ".png"), width = 2560, height = 1440, res = 200)
    print(ggplot2::ggplot(Melted_MegaDataframe, ggplot2::aes(x = factor(Month, levels = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1)) + ggplot2::geom_area(colour = "black", fill = "lightblue", alpha = 0.5) + ggplot2::geom_line() + ggplot2::geom_point(size = 3, shape = 21, fill = 'black') + ggplot2::ggtitle(paste0("Conteudo de Agua na Albufeira ", Iterator)) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, vjust = 1.5), axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, vjust = 1)) + ggplot2::ylab("Ocupacao da Albufeira (%)") + ggplot2::xlab(NULL) + ggplot2::scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 5)) + ggplot2::facet_grid(~ Year))
    grDevices::dev.off()
    Iterator = Iterator + 1
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent plotting (HH:MM:SS): "), noquote(TotalTime)))
}

#' Plots and exports Water Percentage of the input Dataframes. Produces several plots of a time series of Water Percentage of all the ZOI included in the arguments;
#' This function overlaps the graphs from Dataframe_List_All and Dataframe_List_All_2;
#'
#' @param Dataframe_List_All a list of lists, with the first element being a list of dataframes. Each dataframe represents a Year of of Product for ZOI 1; Usually Binary_NDVI product
#' @param Dataframe_List_All_2 a list of lists, with the first element being a list of dataframes. Each dataframe represents a Year of of Product for ZOI 1; Usually the Combo product
#' @param Path a string representing an export path to export the graphs;
#'
#' @return Doesnt return anything, used to plot graphs and export them.
#' @importFrom dplyr bind_rows
#' @importFrom reshape2 melt
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_area
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 facet_grid
#' @export
PlotAllYears_All_Albufeiras2 = function(Dataframe_List_All, Dataframe_List_All_2, Path){
  StartTime = Sys.time()
  Iterator = 1
  for (Albufeira in Dataframe_List_All){
    MegaDataframe = dplyr::bind_rows(Dataframe_List_All[[Iterator]])
    MegaDataframe2 = dplyr::bind_rows(Dataframe_List_All_2[[Iterator]])
    Melted_MegaDataframe = reshape2::melt(MegaDataframe, id.vars=c("Year","Month", "WaterPixels", "NotWaterPixels", "TotalPixel", "TotalWaterAreaM2", "TotalAreaM2", "WaterPercentage"))
    Melted_MegaDataframe2 = reshape2::melt(MegaDataframe2, id.vars=c("Year","Month", "WaterPixels", "NotWaterPixels", "TotalPixel", "TotalWaterAreaM2", "TotalAreaM2", "WaterPercentage"))
    grDevices::png(paste0(Path, "/Albufeira_", Iterator, ".png"), width = 2560, height = 1440, res = 200)
    print(ggplot2::ggplot() +
            ggplot2::geom_area(data = Melted_MegaDataframe2, mapping = ggplot2::aes(x = factor(Month, levels = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1), colour = "black", fill = "#85e085", alpha = 0.5) +
            ggplot2::geom_line(data = Melted_MegaDataframe2, mapping = ggplot2::aes(x = factor(Month, levels = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1)) +
            ggplot2::geom_point(data = Melted_MegaDataframe2, mapping = ggplot2::aes(x = factor(Month, levels = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1)) +
            ggplot2::geom_area(data = Melted_MegaDataframe, mapping = ggplot2::aes(x = factor(Month, levels = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1), colour = "black", fill = "#ff0000", alpha = 0.5) +
            ggplot2::geom_line(data = Melted_MegaDataframe, mapping = ggplot2::aes(x = factor(Month, levels = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1)) +
            ggplot2::geom_point(data = Melted_MegaDataframe, mapping = ggplot2::aes(x = factor(Month, levels = c("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")), y = WaterPercentage, group = 1)) +
            ggplot2::ggtitle(paste0("Conteudo de Agua na Albufeira ", Iterator)) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, vjust = 1.5), axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, vjust = 1)) +
            ggplot2::ylab("Ocupacao da Albufeira (%)") +
            ggplot2::xlab(NULL) + ggplot2::scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 5)) +
            ggplot2::facet_grid(~ Year))
    grDevices::dev.off()
    Iterator = Iterator + 1
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent plotting (HH:MM:SS): "), noquote(TotalTime)))
}
