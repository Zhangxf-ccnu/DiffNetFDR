README file for R package supporting the paper "DiffNetFDR: Differential network analysis with false discovery rate control".


Contents of this archive
------------------------
This archive contains 
(1) DiffNetFDR_1.0.tar: Package source. 
(2) pkg: subdirectory that contains the R package.
(3) shiny: subdirectory that contains the Shiny application.
(4) DiffNetFDR-manual.pdf Reference manual.
(5) example.R Examples for step by step usages for the DiffNetFDR package.

The DiffNetFDR package has the following R-package dependencies: glmnet, igraph, parallel.
The depends are automatically installed along with DiffNetFDR. You can use the following commands to install DiffNetFDR from GitHub.


# Step 1. Install the devtools package. Invoke R and then type
    
install.packages("devtools")



# Step 2. Load the devtools package.
    
library("devtools")



# Step 3. Install the DiffNetFDR package from GitHub.
    
install_github("Zhangxf-ccnu/DiffNetFDR", subdir="pkg")


Useage
Load the library DiffNetFDR in R console, by running
library(DiffNetFDR)

Simply run the one of diffential analysis methods on your favorite datasets. For example,
out = DiffNet.FDR(X, group, alpha = 0.2, test.type = "pcor", parallel = F, nCpus = 1)

For detialed usages, please refer to DiffNetFDR-manual.pdf.
For more examples, please refer to DiffNetDFR-demo.R

Please do not hesitate to contact Dr. Xiao-Fei Zhang at zhangxf@mail.ccnu.edu.cn to 
seek any clarifications regarding any contents or operation of the archive.