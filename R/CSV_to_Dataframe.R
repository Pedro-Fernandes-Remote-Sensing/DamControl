#' Reads a list of CSV files and produce a list of dataframes. CAUTION: You might have to sort the list as it might come unsorted due to file names. Use mixedsort();
#'CSV_List_Year <<- list.files(path = "XXXX", pattern = "XXXX", full.names = TRUE)
#'Sorted_CSV_List_Year <<- mixedsort(CSV_List_Year)
#' @param CSV_List a list of filepaths to CSV files;
#'
#' @return returns a list of dataframes;
#' @importFrom utils read.csv
#' @export
#'
#' @examples NULL
ReadCSVtoDataframe = function(CSV_List) {
  Export_CSV_List = list()
  Iterator = 1
  for (CSV in CSV_List){
    CSV_One = utils::read.csv(CSV_List[[Iterator]], header = TRUE, dec = ",", sep = ";")
    Export_CSV_List[[Iterator]] = CSV_One
    Iterator = Iterator + 1
  }
  return(Export_CSV_List)
}


