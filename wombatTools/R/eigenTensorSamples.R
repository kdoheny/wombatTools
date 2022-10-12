#' @title Performs 4th order Eigentensor analysis on m-number of n x n matrices, sampled SAMPLES times
#' @details Eigentensor analysis originally published in Hine et al. (2009) to calculate the fourth-order covariance tensor of a set of second-order G matrices.
#' @param Gs an array of the order n x n x m x SAMPLES
#' @import gdata matrixcalc MCMCglmm
#' @return returns a list with summary statistics of the eigenTensor with 6 slots
#' $tensor.summary	Summary of the covariance tensor for the posterior mean S From left-to-right the columns of $tensor.summary are the eigenvalues of the tensor (S.eigval), the eigenvalues of eigentensors (eT.val), and the eigenvectors of eigentensors. Eigenvectors of eigentensors are stored as rows with the trait loadings identified by the column names. The eigenvectors of eigentensors are ordered in terms of the absolute value of eT.val
#' $av.S	Posterior mean S
#' $eTmat	Eigentensors of the posterior mean S. The rows and columns of $eTmat identify the elements of an eigentensor. The third dimension of $eTmat identifies the eigentensor.
#' $avG.coord	Coordinates of the posterior mean Gs in the space of the eigentensors of the posterior mean S.
#' $S	S for each sample of the set of G
#' $S.val	Variance of the eigentensors for each sample of the second-order representation of the fourth-order tensor
#' $G.coord	Coordinates of each sample of G in the space of the eigentensors of the posterior mean S.
#' @export
#' @examples
#' covTensor(Gs)
covTensor <- function(Gs) {
  if (dim(Gs)[[1]] != dim(Gs)[[2]]) {
    stop("G array must be of order n x n x m x SAMPLES")
  }
  if (is.na(dim(Gs)[4])) {
    stop("There are no SAMPLES")
  }
  neigten <- n * (n + 1) / 2
  #Number of eigentensors
  MCMC.S <- array(, c(neigten, neigten, MCMCsamp))
  dimnames(MCMC.S) <-
    list(paste("e", 1:neigten, sep = ""), paste("e", 1:neigten, sep = ""))
  for (k in 1:MCMCsamp) {
    MCMCG <- Gs[, , , k]
    MCMCvarmat <- t(apply(MCMCG, 3, diag))
    #find the variances of the kth G and store them
    MCMCcovmat <- t(apply(MCMCG, 3, lowerTriangle))
    #find the covariances of the kth G and store them
    MCMC.S[1:n, 1:n, k] <- cov(MCMCvarmat, MCMCvarmat)
    #fill the upper left quadrant of the kth S
    MCMC.S[(n + 1):neigten, (n + 1):neigten, k] <-
      2 * cov(MCMCcovmat, MCMCcovmat)
    #fill the lower right quadrant of the kth S
    MCMC.S[1:n, (n + 1):neigten, k] <-
      sqrt(2) * cov(MCMCvarmat, MCMCcovmat)
    #fill the upper right quadrant of the kth S
    MCMC.S[(n + 1):neigten, 1:n, k] <-
      sqrt(2) * cov(MCMCcovmat, MCMCvarmat)
    #fill the lower left quadrant of the kthS
  }
  av.S <- apply(MCMC.S, 1:2, mean)
  #posterior mean S
  av.S.val <- eigen(av.S)$values
  #eigenvalues of posterior mean S
  av.S.vec <- eigen(av.S)$vectors
  #eigenvalues of posterior mean S
  eTmat <- array(, c(n, n, neigten))
  dimnames(eTmat) <-
    list(traitnames, traitnames, paste("E", 1:neigten, sep = ""))
  for (i in 1:neigten) {
    emat <- matrix(0, n, n)
    lowerTriangle(emat) <- 1 / sqrt(2) * av.S.vec[(n + 1):neigten, i]
    emat <- emat + t(emat)
    diag(emat) <- av.S.vec[1:n, i]
    eTmat[, , i] <- emat
  }
  #construct the second-order eigentensors of posterior mean S
  eT.eigen <- array(, c(n + 1, n, neigten))
  for (i in 1:neigten) {
    eT.eigen[1, , i] <- t(eigen(eTmat[, , i])$values)
    #Eigenvalues of the ith eigentensor
    eT.eigen[2:(n + 1), , i] <- eigen(eTmat[, , i])$vectors
    #Eigenvectors of the ith eigentensor
    eT.eigen[, , i] <-
      eT.eigen[, order(abs(eT.eigen[1, , i]), decreasing = T), i]
  }
  MCMC.S.val <- matrix(, MCMCsamp, neigten)
  colnames(MCMC.S.val) <- paste("E", 1:neigten, sep = "")
  for (i in 1:MCMCsamp) {
    for (j in 1:neigten) {
      MCMC.S.val[i, j] <- t(av.S.vec[, j]) %*% MCMC.S[, , i] %*% av.S.vec[, j]
    }
  }
  #posterior distribution of the genetic variance for the eigenvectors of posterior mean S
  av.G.coord <- array(, c(m, neigten, 1))
  dimnames(av.G.coord) <-
    list(Gnames, paste("E", 1:neigten, sep = ""))
  for (i in 1:neigten) {
    av.G.coord[, i, ] <-
      apply((apply(Gs, 1:3, mean)) , 3, frobenius.prod, y = eTmat[, , i])
  }
  #Coordinates of the jth avG for the eigentensors of posterior mean S
  MCMC.G.coord <- array(, c(m, neigten, MCMCsamp))
  dimnames(MCMC.G.coord) <-
    list(Gnames, paste("E", 1:neigten, sep = ""))
  for (i in 1:neigten) {
    MCMC.G.coord[, i, ] <-
      apply(Gs, 3:4, frobenius.prod, y = eTmat[, , i])
  }
  #Coordinates of the kth MCMC sample of the jth G for the eigentensors of posterior mean S
  tensor.summary <-
    data.frame(rep(av.S.val, each = n), t(data.frame(eT.eigen)))
  colnames(tensor.summary) <- c("S.eigval", "eT.val", traitnames)
  rownames(tensor.summary) <-
    paste(paste("e", rep(1:neigten, each = n), sep = ""), rep(1:n, neigten), sep =
            ".")
  list(
    tensor.summary = tensor.summary,
    av.S = av.S,
    eTmat = eTmat,
    av.G.coord = av.G.coord,
    MCMC.S = MCMC.S,
    MCMC.S.val = MCMC.S.val,
    MCMC.G.coord = MCMC.G.coord
  )
}
