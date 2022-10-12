# wombatTools
Tools to create and format data such that they may be run in the Quantitative Genetics Software Wombat(Houle and Meyer 2015). This software accommodates for mostly simple cases and should act as a jumping off point to more complex and question driven analysis in Wombat.

Additioinally there are functions that allow for simulations to be integrated into the wombat software.

Finally there are functions that allow the analysis of multiple G matrices at once using the fourth order eigentensor (Aguire et al 2014). This code in particular is an adaption of Aguire et al 2014 and Hine et al 2009 such that it accomodates input from wombat.

For ease of use and installation of this package please refer to: https://kbroman.org/pkg_primer/pages/github.html
Notably, to install the package you can 
1) install.packages("devtools")
2) library(devtools)
3) install_github("kdoheny/wombatTools")

# Referances 

Aguirre JD, Hine E, McGuigan K, Blows MW. Comparing G: multivariate analysis of genetic variation in multiple populations. Heredity (Edinb). 2014 Jan;112(1):21-9. doi: 10.1038/hdy.2013.12.

Hine E, Chenoweth SF, Rundle HD, Blows MW (2009). Characterizing the evolution of genetic variance using genetic covariance tensors. Phil Trans R Soc Lond B Biol Sci 364: 1567–1578.

Houle D, Meyer K. Estimating sampling error of evolutionary statistics based on genetic covariance matrices using maximum likelihood. Journal of Evolutionary Biology 28:1542-1549. doi: 10.1111/jeb.12674.
