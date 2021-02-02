# Run these every time you re-start R:
rm(list = ls())
library(foreign)
library(survey)
library(tidyverse)
library(readr)
library(MEPS)
library(parallel)

yr_range <- c(2002:2018)
data_path <- "MEPS data/"

#### Download files
for (type in c("FYC", "Conditions", "OB", "OP", "IP", "HH", "PMED", "CLNK", "RXLK", "ER", "OM", "DV")) {
  print(type)
  aa <- Sys.time()
  if (type == "PMED") type <- "RX"
  save_path <- paste0(data_path, type, "/")
  dir.create(save_path)
  mclapply(yr_range, function(x) {
    tmp_df <- read_MEPS(year = x, type = type)
    saveRDS(tmp_df, paste0(save_path, "yr", x, ".rds"))
  }, mc.cores = 4)
  print(Sys.time() - aa) 
}
