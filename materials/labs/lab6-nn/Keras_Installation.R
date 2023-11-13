# install the development version of packages, in case the
# issue is already fixed but not on CRAN yet.
install.packages("remotes")
remotes::install_github(sprintf("rstudio/%s", c("reticulate", "tensorflow", "keras")))
reticulate::miniconda_uninstall() # start with a blank slate
reticulate::install_miniconda()
keras::install_keras()


##############################################################
install.packages("remotes")
remotes::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow(version = "2.0.0b1", method = "conda", envname = "r-reticulate")
library(keras)