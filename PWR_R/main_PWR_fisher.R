##################################################################
# A polynomial piecewise regression model for the optimal segmentation of a
# time series with regime changes. It uses dynamic programming for the segmentation 
# and the LSE for the estimation of the regression parameters.
#
# by Faicel Chamroukhi Decembre 2008.
#
## Please cite the following papers for this code:
#
# @article{chamroukhi_et_al_NN2009,
# 	Address = {Oxford, UK, UK},
# 	Author = {Chamroukhi, F. and Sam\'{e}, A. and Govaert, G. and Aknin, P.},
# 	Date-Added = {2014-10-22 20:08:41 +0000},
# 	Date-Modified = {2014-10-22 20:08:41 +0000},
# 	Journal = {Neural Networks},
# 	Number = {5-6},
# 	Pages = {593--602},
# 	Publisher = {Elsevier Science Ltd.},
# 	Title = {Time series modeling by a regression approach based on a latent process},
# 	Volume = {22},
# 	Year = {2009},
# 	url  = {https://chamroukhi.users.lmno.cnrs.fr/papers/Chamroukhi_Neural_Networks_2009.pdf}
# 	}
# 
# @INPROCEEDINGS{Chamroukhi-IJCNN-2009,
#   AUTHOR =       {Chamroukhi, F. and Sam\'e,  A. and Govaert, G. and Aknin, P.},
#   TITLE =        {A regression model with a hidden logistic process for feature extraction from time series},
#   BOOKTITLE =    {International Joint Conference on Neural Networks (IJCNN)},
#   YEAR =         {2009},
#   month = {June},
#   pages = {489--496},
#   Address = {Atlanta, GA},
#  url = {https://chamroukhi.users.lmno.cnrs.fr/papers/chamroukhi_ijcnn2009.pdf}
# }
# 
#@article{Chamroukhi-FDA-2018,
#	Journal = {Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery},
#	Author = {Faicel Chamroukhi and Hien D. Nguyen},
#	Note = {DOI: 10.1002/widm.1298.},
#	Volume = {},
#	Title = {Model-Based Clustering and Classification of Functional Data},
#	Year = {2018},
#	Month = {Dec},
#	url =  {https://chamroukhi.com/papers/MBCC-FDA.pdf}
#	}
#
#
#
# Faicel Chamroukhi Decembre 2008.
##################################################################
rm(list=ls()) # remove and rm can be used to remove objects

#put your folder directory here
#setwd("....")

library(matlib)
library(matrixcalc)
library(R.matlab)

source("fit_PWR_fisher.R")
source("designmatrix.R")
source("cost_matrix_PPWR.R")
source("dynamic_prog.R")
source("show_PWR_results.R")
source("tril.R")


simulated_time_series = readMat("simulated_time_series.mat")
x = simulated_time_series$x
y = simulated_time_series$y

# model specification
K = 5# number of segments
p = 3 # polynomial degree

pwr = fit_PWR_fisher(x, y, K, p)

cat(sprintf("elapsed time = %1.1f", pwr$stats$cputime), "s")

show_PWR_results(x, y, pwr)


