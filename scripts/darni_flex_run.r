#!/usr/bin/env Rscript

library(dplyr)
library(doMC); registerDoMC(cores = 10)

## Edit the repository if needed
dir_dardar <- file.path(Sys.getenv("DARNI_WORK", file.path(Sys.getenv("HOME"), "storage", "work")), "data/satellite/DARDAR-Nice/DARNI_L2_PRO.v2.0/2010") # Directory where the DARDAR files are stored
dir_flex <- file.path(Sys.getenv("DARNI_SCRATCH", file.path(Sys.getenv("HOME"), "storage", "scratch")), "data/model/FLEXPART") # Directory where the FLEXPART files are stored
dir_dardar_origin <- file.path(Sys.getenv("DARNI_SCRATCH", file.path(Sys.getenv("HOME"), "storage", "scratch")), "tmp/dardar_flex") # Directory where the output files will be stored

## List the files
lf_dardar <- list.files(dir_dardar, full.names = TRUE, recursive = TRUE, pattern = ".nc")
lf_flex <- list.files(dir_flex, full.names = TRUE, recursive = TRUE, pattern = "number_number.nc")

## Here is for testing
fn_test <- lf_flex[grepl("20100203185536", lf_flex)]
null <- darflex::load_data(fn_test, lf_dardar = lf_dardar, dir_out = dir_dardar_origin, overwrite = TRUE)

## Here is for the full dataset
## null <- plyr::ldply(lf_flex, darflex::load_data, lf_dardar = lf_dardar, dir_out = dir_dardar_origin, .parallel = TRUE)
