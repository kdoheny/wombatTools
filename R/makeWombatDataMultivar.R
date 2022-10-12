#' @title Makes stacked dataset for multivariate trait analysis
#' @description Makes a stacked multivariate datafile for use in wombat. In particular this function creates a stacked datafile for multiple traits at once for multivariate analysis.
#' @param idVec vector of ids
#' @param randomEffectData dataframe (or matrix) of random effect data
#' @param fixedCovVec numeric vector, fixed covariate variable included in the model
#' @param fixedEffectVec vector, fixed effect variable included in the model
#' @param dataFileName string, name of fixed effect variable to be included in the model
#' @param dir string, directory where parameter file should be output
#' @return outputs multivariate data file for use in wombat run
#' @export
#' @examples
#' dir <- getwd()
#' idVec <- seq(1000,1002)
#' randomEffectData <- matrix(rnorm(9,1,1),3,3)
#' fixedCovVec <- rnorm(3,5,1)
#' fixedEffectVec <- rnorm(3,10,1)
#' dataFileName <- "new"
#' makeMultivariateData(idVec,randomEffectData,fixedCovVec,fixedEffectVec,dataFileName,dir)

makeMultivariateData <-
  function(idVec,
           randomEffectData = NULL,
           fixedCovVec = NULL,
           fixedEffectVec = NULL,
           dataFileName,
           dir) {
    # data creation for stacked datafile
    repId <- rep(idVec, each = ncol(randomEffectData))
    traitNo <- rep(seq(ncol(randomEffectData)), length(idVec))
    param <- as.vector(t(randomEffectData))
    repFixedCov <- rep(fixedCovVec, each = ncol(randomEffectData))
    repFixedEffect <- rep(fixedEffectVec, each = ncol(randomEffectData))
    stackedData <- cbind(traitNo, repId, param, repFixedCov, repFixedEffect)


    ## creates stacked datafile and saves it in the folder specified by the path statement
    write.table(
      stackedData,
      paste0(file.path(dir, dataFileName), ".d"),
      sep = " ",
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE
    )
  }
