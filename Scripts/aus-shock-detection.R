# TITLE: aus-shock-detection

# Load packages

# Source functions
source("Scripts/functions.R")

# Load data
df <- read.csv("Data/NSWcephaggregate.csv")

# Check for shocks
prod_shocks <- shockid(df$ProductionTOTAL)
df$Year[prod_shocks$shock.event == 1]

val_shocks <- shockid(df$ValueTOTAL)
df$Year[val_shocks$shock.event == 1]