% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BuildBrickList.R
\name{BuildBrickList}
\alias{BuildBrickList}
\title{Build a "Brick" object list from a List of FilePaths, and prints time spent;}
\usage{
BuildBrickList(FilePathList)
}
\arguments{
\item{FilePathList}{a list of file paths. This is usually obtained through list.files with path = location_of_files, pattern = string_to_detect_files, full.names = TRUE}
}
\value{
a list of "Bricks", an object usable by R that represents a file with several layers, usually a raster;
}
\description{
Build a "Brick" object list from a List of FilePaths, and prints time spent;
}
