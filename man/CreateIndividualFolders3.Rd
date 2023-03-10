% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateIndividualFolders_1_2_3.R
\name{CreateIndividualFolders3}
\alias{CreateIndividualFolders3}
\title{Creates folders for each individual ZOI, with both thresholds imbued in the folder name;}
\usage{
CreateIndividualFolders3(
  FolderDestinationPath,
  ID_list,
  Threshold_List,
  Threshold_List_2
)
}
\arguments{
\item{FolderDestinationPath}{a destination path to the folders being created, as a string;}

\item{ID_list}{a list of IDs representing the amount of unique shapes inside the original shapefile, usually created asa global variable using CropShapefile_ExportToPath;}

\item{Threshold_List}{a list of thresholds, usually just one, representing the threshold for either the NDVI or RGBSum, in order to create properly named folders;}

\item{Threshold_List_2}{a list of thresholds, usually just one, representing the threshold for either the NDVI or RGBSum, in order to create properly named folders; Obviously use different thresholds for these two arguments;}
}
\value{
returns nothing, used to create folders;
}
\description{
Creates folders for each individual ZOI, with both thresholds imbued in the folder name;
}
