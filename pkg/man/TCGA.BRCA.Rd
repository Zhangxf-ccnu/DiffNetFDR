\name{TCGA.BRCA}
\alias{TCGA.BRCA}
\docType{data}
\title{
TCGA breast cancer data}
\description{
The TCGA breast cancer gene expression dataset used in our case study. The dataset (level 3, Agilent G450 microarray, version: May 6 2017) is obtained from the TCGA database using the TCGA2STAT R package. It includes gene expression measurements for 231 luminal A cancers and 95 basal-like cancers.
The data only includes expression measurement of genes that overlap with the breast cancer pathway (hsa05224) collected from the Kyoto Encyclopedia of Genes and Genomes database. It includes an expression matrix for which the rows represent the samples and the columns represent the genes, and a vector denoting the group membership of each sample.
}

\usage{data("TCGA.BRCA")}


\references{
Xiao-Fei Zhang, Le Ou-Yang, Shuo Yang, Xiaohua Hu and Hong Yan, DiffNetFDR: Differential network analysis with false discovery rate control, 2018. \cr

The Cancer Genome Atlas Research Network, Comprehensive molecular portraits of human breast tumors, Nature, 490(7418): 61-70, 2012. (http://cancergenome.nih.gov/)
}


\author{
\packageAuthor{DiffNetFDR}
}

\seealso{
\code{\link{DiffNet.FDR}}
}


\examples{
data(TCGA.BRCA)
## maybe str(TCGA.BRCA) ...
}

