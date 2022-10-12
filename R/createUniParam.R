#' @title Makes a univariate parameter file
#' @description Function to make a parameter file for easy use in the Wombat quantitative genetics software
#' @param randomEffect numeric vector, of the random variable to be included in the model
#' @param fixedCovName character(1)string, name of fixed effect covariate
#' @param fixedEffectName character(1)string, name of fixed effect
#' @param datafileName character(1)string, name of the datafile associated with this wombat model
#' @param pedName character(1)string, with the name of the pedigree file (extension excluded)
#' @param startingVariance string, specifying the starting variance from which wombat will employ hill climbing algorithms to improve
#' @param dir character(1) string, specifying the dir where the parameter file should be output
#' @return Makes parameter file for use with wombat software in the form of a .par file.
#' @export
#' @examples
#' randomEffect <- TRUE
#' fixedCovName <- "fc"
#' fixedEffectName <- "fe"
#' datafileName <- "data"
#' pedName <- "ped"
#' startingVariance <- 10
#' dir <- getwd()
#' makeUnivariateParameter(randomEffect, fixedCovName, fixedEffectName, datafileName, pedName, startingVariance, dir)

makeUnivariateParameter <- function(randomEffect, fixedCovName, fixedEffectName, datafileName, pedName, startingVariance, dir) {
  if (randomEffect == T) {
    if (is.null(fixedCovName) == F && is.null(fixedEffectName) == F) {
      parDat <- c("ANAL UNI 1", paste0("PEDS ", pedName, ".d"), paste0("DATA ", datafileName, ".d"), "animal 0", "PAR 0", paste0(fixedCovName, " 0"), paste0(fixedEffectName, " 0"), "END DATA", "MODEL", "RAN animal NRM", "TR PAR", paste0("COV ", fixedCovName, "(1)"), paste0("FIX ", fixedEffectName), "END MODEL", "VAR animal 1", startingVariance / 2, "VAR resid 1", startingVariance / 2)
    } else if (is.null(fixedCovName) == T && is.null(fixedEffectName) == F) {
      parDat <- c("ANAL UNI 1", paste0("PEDS ", pedName, ".d"), paste0("DATA ", datafileName, ".d"), "animal 0", "PAR 0", paste0(fixedEffectName, " 0"), "END DATA", "MODEL", "RAN animal NRM", "TR PAR", paste0("FIX ", fixedEffectName), "END MODEL", "VAR animal 1", startingVariance / 2, "VAR resid 1", startingVariance / 2)
    } else if (is.null(fixedCovName) == F && is.null(fixedEffectName) == T) {
      parDat <- c("ANAL UNI 1", paste0("PEDS ", pedName, ".d"), paste0("DATA ", datafileName, ".d"), "animal 0", "PAR 0", paste0(fixedCovName, " 0"), "END DATA", "MODEL", "RAN animal NRM", "TR PAR", paste0("COV ", fixedCovName, "(1)"), "END MODEL", "VAR animal 1", startingVariance / 2, "VAR resid 1", startingVariance / 2)
    } else {
      parDat <- c("ANAL UNI 1", paste0("PEDS ", pedName, ".d"), paste0("DATA ", datafileName, ".d"), "animal 0", "PAR 0", "END DATA", "MODEL", "RAN animal NRM", "TR PAR", "END MODEL", "VAR animal 1", startingVariance / 2, "VAR resid 1", startingVariance / 2)
    }
  } else {
    if (is.null(fixedCovName) == F && is.null(fixedEffectName) == F) {
      parDat <- c("ANAL UNI 1", paste0("DATA ", datafileName, ".d"), "animal 0", "PAR 0", paste0(fixedCovName, " 0"), paste0(fixedEffectName, " 0"), "END DATA", "MODEL", "TR PAR", paste0("COV ", fixedCovName, "(1)"), paste0("FIX ", fixedEffectName), "END MODEL", "VAR resid 1", startingVariance / 2)
    } else if (is.null(fixedCovName) == T && is.null(fixedEffectName) == F) {
      parDat <- c("ANAL UNI 1", paste0("DATA ", datafileName, ".d"), "animal 0", "PAR 0", paste0(fixedEffectName, " 0"), "END DATA", "MODEL", "TR PAR", paste0("FIX ", fixedEffectName), "END MODEL", "VAR resid 1", startingVariance / 2)
    } else if (is.null(fixedCovName) == F && is.null(fixedEffectName) == T) {
      parDat <- c("ANAL UNI 1", paste0("DATA ", datafileName, ".d"), "animal 0", "PAR 0", paste0(fixedCovName, " 0"), "END DATA", "MODEL", "TR PAR", paste0("COV ", fixedCovName, "(1)"), "END MODEL", "VAR resid 1", startingVariance / 2)
    } else {
      parDat <- c("ANAL UNI 1", paste0("DATA ", datafileName, ".d"), "animal 0", "PAR 0", "END DATA", "MODEL", "TR PAR", "END MODEL", "VAR resid 1", startingVariance / 2)
    }
  }
  write.table(
    parDat,
    paste0(file.path(dir, "Uni"), ".par"),
    row.names = FALSE,
    quote = FALSE,
    col.names = paste0("###Va dim=", 1)
  )
}

