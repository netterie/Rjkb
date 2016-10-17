
######################################################################
# setup_hivbackcalc
######################################################################

#' Setup function
#' @param workd Working directory full file path
#' @param datafile Path to data, either full or from the workd
#' @param source_these Vector of paths to other files to source, either full or from the workd
#' @param loadlib Logical indicating whether to load libries
#' @param msm Logical indicating whether this is the KC MSM analysis where the data frame was called msm
#' @param na Character indicating whether NAs are "NA" or spaces ("")
#' @param package_updated DEPRECATED
#' @param load_package Logical indicating whether the HIVBackCalc package 
#'        should be loaded (TRUE). If you want to have some package functions but overwrite others,
#'        specify TRUE here and a file in the 'packagefile' param
#' @param packagefile Within the workd, path to file with newer package functions
setup_hivbackcalc = function(workd, datafile, source_these, loadlib=TRUE,
                             msm=FALSE, na="", load_package=TRUE,
                             package_updated=NULL, packagefile=NULL) {
  
  if (loadlib) {
    cat('Loading libraries...\n')
    # Load libraries
    # Eventually, make these dependencies of the HIBBackCalc package?
    library(reshape2)
    library(plyr)
    library(ggplot2)
    library(scales)
    library(Hmisc)
  }

  if (!is.null(package_updated)) stop('package_updated is deprecated; use load_package instead')
  if (load_package) library(HIVBackCalc)
  if (!is.null(packagefile)) source_these = c(source_these, packagefile)
  
  # Working directory
  workd <<- workd
  
  # Load data
  cat('Loading data and storing it in object msm or dataf...\n')
  if (msm) {
    msm <<- read.csv(file.path(workd,datafile),na.string=na,stringsAsFactor=FALSE) 
  } else {
    dataf <<- read.csv(file.path(workd,datafile),na.string=na,stringsAsFactor=FALSE) 
  }
  
  # Source files
  for (f in source_these) {
    cat('Sourcing', f, '...\n')
    source(file.path(workd,f))
  }
  
}

