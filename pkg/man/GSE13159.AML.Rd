\name{GSE13159.AML}
\alias{GSE13159.AML}
\docType{data}
\title{
Acute myeloid leukemia data (GSE13159)}
\description{
Gene expression datasets for acute myeloid leukemia (GSE13159). The data is obtained from  http://discern-leelab.cs.washington.edu/. It includes expression measurments for 541 AML cancers and 73 normal samples. The data only include expression measurements of genes that overlap with the acute myeloid leukemia pathway (hsa05221) collected from the Kyoto Encyclopedia of Genes and Genomes databas. It includes an expression matrix for which the rows represent the samples and the columns represent the genes, and a vector denoting the group membership of each sample.
}

\usage{data("GSE13159.AML")}


\references{
Xiao-Fei Zhang, Le Ou-Yang, Shuo Yang, Xiaohua Hu and Hong Yan, DiffNetFDR: An R package for differential network analysis with false discovery rate control, 2018.\cr

Maxim Grechkin, Benjamin A. Logsdon, Andrew J. Gentles and Su-In Lee, Identifying network perturbation in cancer, PLoS Computational Biology, 12(5): e1004888, 2016.
}

\author{
\packageAuthor{DiffNetFDR}
}

\seealso{
\code{\link{DiffNet.FDR}}
}


\examples{
data(GSE13159.AML)
## maybe str(GSE13159.AML) ...
}

