% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateIndividualFolders_1_2_3.R
\name{CreateIndividualFolders}
\alias{CreateIndividualFolders}
\title{Creates folders for each individual ZOI;}
\usage{
CreateIndividualFolders(FolderDestinationPath, ID_list)
}
\arguments{
\item{FolderDestinationPath}{a destination path to the folders being created, as a string;}

\item{ID_list}{a list of IDs representing the amount of unique shapes inside the original shapefile, usually created asa global variable using CropShapefile_ExportToPath;}
}
\value{
returns nothing, used to create folders;
}
\description{
Creates folders for each individual ZOI;
}
