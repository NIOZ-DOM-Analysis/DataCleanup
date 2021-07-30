"updateR_Rstudio.R


Goal:  Update R and R studio to the latest version!

Written by:
Milou Arts, NIOZ, NL, 2019

List of alterations:


"
if(!suppressPackageStartupMessages(require(installr))) {
  install.packages("installr"); 
  suppressPackageStartupMessages(library(installr))
} #load / install+load installr



# we have to reinstall the packages if there was a R update
if (check.for.updates.R(notify_user = FALSE)){
  updateR()
  ## get packages installed
  packs = as.data.frame(installed.packages(.libPaths()[1]), stringsAsFactors = F)
  ## and now re-install install packages using install.packages()
  install.packages(packs$Package)
  rm(packs)}





