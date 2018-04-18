\name{DiffNetFDR-package}
\alias{DiffNetFDR-package}
\alias{DiffNetFDR}
\docType{package}
\title{
\packageTitle{DiffNetFDR}
}
\description{
DiffNetFDR is an R package for differential network analysis with false discovery rate control.
}
\details{
We develop a user-friendly R package, named DiffNetFDR, to implement two differential network analysis methods. Compared methods implemented in existing tools, the methods included in DiffNetFDR have the following features: (i) they are based on Gaussian graphical models which can capture the conditional dependencies; (ii) they can determine the tuning parameters in a data-driven manual; (iii) they take a multiple testing method to control the overall false discovery rate; and (iv) one of them defines the differential network based on partial correlations so that the spurious differential edges caused by variants of conditional variances can be excluded. We also develop a Shiny application to provide easier analysis and visualization.
}
\author{
\packageAuthor{DiffNetFDR}

Maintainer: \packageMaintainer{DiffNetFDR}
}

\references{
Xiao-Fei Zhang, Le Ou-Yang, Shuo Yang, Xiaohua Hu and Hong Yan, DiffNetFDR: Differential network analysis with false discovery rate control, 2018.\cr
Yin Xia, Tianxi Cai and T. Tony Cai, Testing differential networks with applications to the detection of gene-gene interactions, Biometrika, 102(2): 247-266, 2015.\cr
Weidong Liu, Structural similarity and difference testing on multiple sparse Gaussian graphical models, Annals of Statistics, 45(6): 2680-2707, 2017.
}

\seealso{
\code{\link{DiffNet.FDR}}, \code{\link{TCGA.BRCA}}, \code{\link{GSE13159.AML}}
}
