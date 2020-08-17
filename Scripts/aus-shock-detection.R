# TITLE: aus-shock-detection

# Load packages
library(tidyverse)

# Source functions
source("Scripts/functions.R")

# Load data
df <- read.csv("Data/NSWcephaggregate.csv")

# Check for shocks
prod_shocks <- shock.analysis(df, ts.col.name = "ProductionTOTAL")

val_shocks <- shock.analysis(df, ts.col.name = "ValueTOTAL")