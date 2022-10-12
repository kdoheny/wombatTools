#' @title Simulates phenotypes for individuals in a pedigree
#' @description Simulates random phenotypes for individuals in a pedigree given a pedigree and G and E matrices. Generates breeding values described in the methods of Morrissey et al(2007)
#' where ai=mi +C(G/2) + N
#' where ai is the vector of breeding values of the focal individual, mi is the vector of mid-parent values,
#' C(G/2) is the Cholesky decomposition of the matrix describing the segregational variance (i.e. G is the variance–covariance matrix),
#' and N is a vector of standard random normal deviates.
#' @param G N dimenstional covariance matrix representing additive genetic variance (G)
#' @param E N dimenstional covariance matrix representing Environmental variance (E)
#' @param ped pedigree (Nx3 dataframe), where columns are in the order of (and labled) ID,sire,dam. Founders should have parentage of NA
#' @return Returns a dataframe of the size NxM where N is the number of individuals in the pedigree and M is the number of traits considered.
#' @export
#' @import MASS
#' @examples
#' #phens <- phenSim(G, E, ped)
#' G <- cov(matrix(sample(1:20, 100, replace = TRUE), ncol = matrixNumber))
#' E <- cov(matrix(sample(1:20, 100, replace = TRUE), ncol = matrixNumber))
#' s=100;dps=4;opd=4
#' ped <- halfSibMaker(s,dps,opd)
#' phens <- phenSim(G,E,ped)
phenSim <- function(G, E, ped) {
  parentalIDs <- max(na.omit(ped$dam))
  #First, the breeding values of founding individuals are assigned from Gaussian distributions with
  #(co)variances specified by G
  founder_sampling <- mvrnorm(parentalIDs, rep(0, dim(G)[1]), Sigma = G)
  #environmetal vaiance of all individuals are assigned from Gaussian distributions with
  #(co)variances specified by E
  E_sampling <- mvrnorm(nrow(ped), rep(0, dim(G)[1]), Sigma = E)
  offspringNum <- nrow(ped) - parentalIDs
  noFounders <- na.omit(ped)
  #initiates matrix
  midparentValues <- matrix(nrow = offspringNum, ncol = dim(G)[1])
  #segregational variance generated from G matrix cholesky decomopsiton
  segregationalVariance <- chol(G / 2)
  #vector of standard random normal deviates
  standNormalDeviates <-
    mvrnorm(offspringNum,
            mu = rep(0, dim(G)[1]),
            Sigma = diag(1, dim(G)[1]))
  #calculates midparent values
  for (i in 1:offspringNum) {
    midparentValues[i, ] <-
      ((founder_sampling[noFounders[i, 2], ] + founder_sampling[noFounders[i, 3], ]) /
         2)
  }
  #generates breeding values described in the methods of Morrissey et al(2007)
  #ai=mi +C(G/2) + N
  #where ai is the vector of breeding values of the focal individual, mi is the vector of mid-parent values,
  #C(G/2) is the Cholesky decomposition of the matrix describing the segregational variance (i.e. G is the variance–covariance matrix),
  #and N is a vector of standard random normal deviates.
  offspringBV <- midparentValues + standNormalDeviates %*% segregationalVariance
  allBV <- rbind(founder_sampling, offspringBV)

  #breeding values plus environmental values
  phenotpes <- allBV + E_sampling

}

