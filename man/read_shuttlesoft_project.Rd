% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_shuttlesoft_project.R
\name{read_shuttlesoft_project}
\alias{read_shuttlesoft_project}
\title{Import all shuttlesoft files in a directory}
\usage{
read_shuttlesoft_project(metadata, directory = getwd())
}
\arguments{
\item{metadata}{A metadata file containing the variables file_name, initial_temp, mass, a_value and b_value for each trial in the directory}

\item{directory}{The directory the function will search in for the .txt files, default is the working directory}
}
\value{
List object containing imported raw shuttlesoft data
}
\description{
This function imports all the .txt shuttlesoft files in a directory according to read_shuttlesoft and compiles them into a list object
}
