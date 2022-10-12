# wombatTools
Tools to create and format data such that they may be run in the Quantitative Genetics Software Wombat(Houle and Meyer 2007; Houle and Meyer 2015). This software accommodates for mostly simple cases and should act as a jumping off point to more complex and question driven analysis in Wombat.

Additioinally there are functions that allow for simulations to be integrated into the wombat software.

Finally there are functions that allow the analysis of multiple G matrices at once using the fourth order eigentensor (Aguire et al 2014). This code in particular is an adaption of Aguire et al 2014 and Hine et al 2009 such that it accomodates input from wombat.

For ease of use and installation of this package please refer to: https://kbroman.org/pkg_primer/pages/github.html
Notably, to install the package you can 
1) install.packages("devtools")
2) library(devtools)
3) install_github("kdoheny/wombatTools")

