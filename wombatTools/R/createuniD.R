#' @title Makes univariate data file
#' @description Generates a univariate data file formatted for wombat
#' @param idVec vector of ids
#' @param randomEffectVec numeric vector of trait values for individuals (should be in the same order as idvec)
#' @param fixedCovVec numeric vector of fixed covariate for individuals (should be in the same order as idvec)
#' @param fixedEffectVec vector of fixed effect for individuals (should be in the same order as idvec)
#' @param dataFileName string, name of datafile
#' @param dir string, directory where .d file will be written
#' @return Makes a univariate datafile for wombat
#' @export
#' @examples
#' idVec<-c(1,2,3)
#' randomEffectVec<-c(4,5,6)
#' fixedCovVec<-c(7,8,9)
#' fixedEffectVec<-c(10,11,12)
#' dataFileName<-"new"
#' dir<-getwd()
#' makeUnivariateData(idVec,randomEffectVec,fixedCovVec, fixedEffectVec, dataFileName, dir)

makeUnivariateData <-
  function(idVec,
           randomEffectVec = NULL,
           fixedCovVec = NULL,
           fixedEffectVec = NULL,
           dataFileName,
           dir) {
    # creates data file variables
    data <-
      as.data.frame(cbind(idVec, randomEffectVec, fixedCovVec, fixedEffectVec))

    ## creates stacked datafiles and writes to dir
    write.table(
      data,
      paste0(file.path(dir, dataFileName), ".d"),
      sep = " ",
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE
    )
  }
