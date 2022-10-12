#' @title Simulates a Half-sib pedigree
#' @description Generate random Half sib Pedigree given the number of sires, dams mated to each sire and, offspring per dam
#' @param s numeric(1), number of sires to simulate
#' @param dps numeric(1), number of dams mated to each sire
#' @param opd numeric(1), number of offspring produced by each family (dam)
#' @return Returns a 3 column dataframe in order of ID,sire,dam
#' @export
#' @examples
#' s=100;dps=4;opd=4
#' halfSibMaker(s, dps, opd)

halfSibMaker <- function(s, dps, opd) {
  d = dps * s
  o = opd * d
  sires <- seq(1:s)
  dams <- seq(1:d) + length(sires)
  offspring <- seq(1:o) + length(c(sires, dams))
  sireString <- rep(sires, each = dps * opd)
  damString <- rep(dams, each = opd)
  p_one <- cbind(offspring, damString, sireString)
  p_two <- cbind(offspring, sireString, damString)
  g_zero <-
    cbind(c(unique(sires), unique(dams)), rep(NA, s + d), rep(NA, s + d))
  #ped<-rbind(g_zero,p_one)
  ped <- rbind(g_zero, p_two)
  # ped1[is.na(ped1)] <- 0
  colnames(ped) <- c("id", "sire", "dam")
  return(as.data.frame(ped))
}
