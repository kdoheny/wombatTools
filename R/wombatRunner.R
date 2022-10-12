#' @title Runs basic wombat run
#' @description Running wombat given the filepath of a parameter (.par) file
#' @param filePath character(1) string, specifying the filepath leading to the parameter file
#' @return returns wombat to standard output in the working directory
#' @export
#' @import stats utils
#' @details This function allows simple runs of wombat with the most limited options.
#' @examples
#' filePath <- "A:/wombatGithub/univariatePackages/Uni.par"
#' wombatRunner(filePath)

wombatRunner <- function(filePath) {
  ## breaking filename into parts
  fileName <- basename(filePath)
  dirName <- dirname(filePath)

  ## set working directory
  setwd(dirName)

  ## run wombat on the correct system (windows or mac)
  machineId <- Sys.info()[1]
  if (machineId == "Windows" &&
      file.exists("wombat.exe") == FALSE) {
    print("Place a copy of wombat.exe in the working directory.")
  } else if (machineId == "Windows" &&
             file.exists("wombat.exe") == TRUE) {
    system(paste0("wombat.exe ", fileName))
  } else {
    system(paste0("wombat_sequential ", fileName))
  }
}

