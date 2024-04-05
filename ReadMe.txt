Data Analytics project - Shop clustering :

The present project contains a variety of files with the following structure:
1) The Code Directory:
	- Project Ravey & Clouet: Full Rmarkdown code which has been used in order to the report in HTML
	- Functions : We save the function code used in the 2 other file in order to do have a consise code
	- App : It is an Rshiny App which will permit to the stackeholders to have an idea of their data

2) Data Directory: 
	- Customers : contains the data from Kaggle
	- dfCluster : It is the data after our work
3) Documentation-Paper Directory: 2 files containings the report
4) Model: Save our ML Model in order to directly load it and do not need to fit it once again

DATABASE CONFIGURATION: All databases, the initial one and the ones produced in this simulation are csv files

VERSION: The version used is R 4.2.3

DEPENDENCIES: 

The following libraries have been used:

# Package to parallelize computing :
library(parallel)

# Packages to manipulate data :
library(dplyr)
library(reshape2)
library(purrr)

# Packages graphs/plots :
library(ggplot2)
library(GGally)
library(ggplot2)
library(gridExtra)
library(plotly)
library(ggcorrplot)
library(patchwork)
library(RColorBrewer)

# Packages for unsupervised Machine Learning :
library(factoextra)
library(cluster)
library(dbscan)

