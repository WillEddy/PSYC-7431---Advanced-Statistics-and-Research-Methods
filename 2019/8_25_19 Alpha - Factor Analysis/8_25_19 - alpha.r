
library(rstudioapi); library(readxl); library(tidyverse)
library(haven); library(psych); library(GPArotation); library(readr)
# library(stringr); library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 



#Import data

DATA <- read.fwf("KJ.dat", widths = c(1,1,1,1,1,1,1,1,1,1))

omega(DATA[1:10])
