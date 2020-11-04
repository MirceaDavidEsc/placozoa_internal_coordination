# Install standard CRAN packages
install.packages(c("tidyverse", "forcats", "cowplot", "jpeg", "grid", "alphahull", "modelr", "polynom", "stringr",
                   "pracma", "sp", "latex2exp", "polynom", "zoo"))

# Install Bioconductor packages
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("rhdf5")


# Install packages developed by Mircea Davidescu on GitHub
install.packages("devtools")
library(devtools)

install_github("MirceaDavidEsc/mover")
install_github("MirceaDavidEsc/bigh5")
install_github("MirceaDavidEsc/collective")
# install_github("MirceaDavidEsc/scalefree") # BROKEN ON GITHUB? Need to figure out why
install_github("MirceaDavidEsc/mover")

# Install multidplyr for parallel processing
devtools::install_github("tidyverse/multidplyr")
