#' @title Make multivariate parameter file
#' @description Makes a multivariate parameter file for use with the wombat program.
#' @param randomEffectData dataframe (or matrix) of random effect data
#' @param fixedCovName string, name of fixed effect covariate
#' @param fixedEffectName string, name of fixed effect variable to be included in the model
#' @param pedName string, with the name of the pedigree file (extension excluded)
#' @param dataFileName string, name of fixed effect variable to be included in the model
#' @param rank integer, rank of the model to be run (wombat accommodates for reduced rank models if desirable)
#' @param dir string, directory where parameter file should be output
#' @return Returns a stacked parameter file that, when accompanied with formatted data and pedigree files, will run the desired mixed model.
#' @export
#' @examples
#' randomEffectData<- matrix(rnorm(9,1,1),3,3)
#' fixedCovName <- "fc"
#' fixedEffectName <- "fe"
#' pedName <- "pedpedped"
#' dataFileName <- "data"
#' rank <- 1
#' dir<-getwd()
#' makeMultivariateParameter(randomEffectData,fixedCovName ,fixedEffectName, pedName, dataFileName, rank, dir)


makeMultivariateParameter <-
  function(randomEffectData,
           fixedCovName = NULL,
           fixedEffectName = NULL,
           pedName,
           dataFileName,
           rank = ncol(randomEffectData),
           dir) {
    paramNum <- sum(!is.null(randomEffectData), !is.null(fixedCovName), !is.null(fixedEffectName), 2)

    if (is.null(randomEffectData) == FALSE) {
      tr <- rep(paste0("TR", seq(ncol(randomEffectData))), each = paramNum)
      an <- rep("animal 0", ncol(randomEffectData))
      trNo <- paste0("trait ", ncol(randomEffectData))
      parVec <- paste0("PAR", seq(ncol(randomEffectData)))
      reMod <- "RAN animal NRM"
      pedMod <- paste0("PEDS ", pedName, ".d")
      matMod <- c(paste0("VAR animal ", ncol(randomEffectData), " ", rank))
      parMod <- c(paste0("TR PAR", seq(ncol(randomEffectData)), " ", seq(ncol(randomEffectData))))
    }

    if (is.null(fixedCovName) == FALSE) {
      fc <- rep(paste0(fixedCovName, " ", 0), ncol(randomEffectData))
      fcMod <- paste0("COV ", fixedCovName, "(1)")
    } else {
      fc <- NULL
      fcMod <- NULL
    }

    if (is.null(fixedEffectName) == FALSE) {
      fe <- rep(paste0(fixedEffectName, " ", 0), ncol(randomEffectData))
      feMod <- paste0("FIX ", fixedEffectName)
    } else {
      fe <- NULL
      feMod <- NULL
    }

    trMod <- paste0("TR ", parVec, " ", seq(ncol(randomEffectData)))
    res <- rbind(trNo, an, parVec, fc, fe)
    attributes(res) <- NULL
    upperTri <- cov(randomEffectData)
    upperTri[upper.tri(upperTri)] <- NA
    upperTri <- na.omit(as.vector(upperTri))
    ## add sample etc
    formatedData <- unlist(c(paste0("#FixedCovariate:", fixedCovName, " FixedEffect:", fixedEffectName, " ", "Number Of Traits: ", ncol(randomEffectData)," Rank ",rank), "#RUNOP --sample1000", paste0("ANAL MUV PC ", ncol(randomEffectData)), pedMod, paste0("DATA ", dataFileName, ".d"), paste0(tr, " ", res), "END DATA", "MODEL", reMod, parMod, fcMod, feMod, "END MODEL", matMod, upperTri, paste0("VAR RESID ", ncol(randomEffectData), " ", ncol(randomEffectData)), upperTri, "#SPECIAL", "#SAMPLEAI animal", "#END"))

    write.table(
      formatedData,
      paste0(file.path(dir, dataFileName), ".par"),
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )
  }

