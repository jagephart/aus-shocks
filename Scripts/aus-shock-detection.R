# TITLE: aus-shock-detection

# Load packages
library(tidyverse)

# Source functions
source("Scripts/functions.R")

#____________________________________________________________________________________________#
# Example with single time series
#____________________________________________________________________________________________#
# Load example data (single TS)
df <- read.csv("Data/NSWcephaggregate.csv")

# Check for shocks
prod_shocks <- shock.analysis(df, ts.col.name = "ProductionTOTAL")

val_shocks <- shock.analysis(df, ts.col.name = "ValueTOTAL")


#____________________________________________________________________________________________#
# Use full data set and analyze subsets
#____________________________________________________________________________________________#
df <- read.csv("final_AusShocksData.csv")
df$ValueAUD <- as.numeric(as.character(df$ValueAUD))
df$ProductionT <- as.numeric(as.character(df$ProductionT))

#____________________________________________________________________________________________#
# Loop through sectors
#____________________________________________________________________________________________#
sectors <- unique(df$Sector)

# Create dataframes
j = 1
temp <- df %>%
  filter(Sector == sectors[j]) %>%
  group_by(Year) %>%
  summarise(ValueAUD = sum(ValueAUD, na.rm = TRUE), ProductionT = sum(ProductionT, na.rm = TRUE))

val_shocks <- shock.analysis(temp, ts.col.name = "ValueAUD")
val_shocks$shock.group <- paste(sectors[j])

prod_shocks <- shock.analysis(temp, ts.col.name = "ProductionT")
prod_shocks$shock.group <- paste(sectors[j])

for(j in 2:length(sectors)){
  temp <- df %>%
    filter(Sector == sectors[j]) %>%
    group_by(Year) %>%
    summarise(ValueAUD = sum(ValueAUD, na.rm = TRUE), ProductionT = sum(ProductionT, na.rm = TRUE))
  
  outt <- shock.analysis(temp, ts.col.name = "ValueAUD")
  outt$shock.group <- paste(sectors[j])
  val_shocks <- val_shocks %>% bind_rows(outt)
  
  outt <- shock.analysis(temp, ts.col.name = "ProductionT")
  outt$shock.group <- paste(sectors[j])
  prod_shocks <- prod_shocks %>% bind_rows(outt)
}

#____________________________________________________________________________________________#
# Loop through taxa
#____________________________________________________________________________________________#
# FIX IT: other_aquaculture shows up twice because there is a mispelling "other_aquaculure"
df$Taxa_fishery[df$Taxa_fishery == "other_aquaculure"] <- "other_aquaculture"
taxa <- unique(df$Taxa_fishery)

# Create dataframes
j = 1
temp <- df %>%
  filter(Taxa_fishery == taxa[j]) %>%
  group_by(Year) %>%
  summarise(ValueAUD = sum(ValueAUD, na.rm = TRUE), ProductionT = sum(ProductionT, na.rm = TRUE))

val_shocks <- shock.analysis(temp, ts.col.name = "ValueAUD")
val_shocks$shock.group <- paste(taxa[j])

prod_shocks <- shock.analysis(temp, ts.col.name = "ProductionT")
prod_shocks$shock.group <- paste(taxa[j])

for(j in 2:length(taxa)){
  temp <- df %>%
    filter(Taxa_fishery == taxa[j]) %>%
    group_by(Year) %>%
    summarise(ValueAUD = sum(ValueAUD, na.rm = TRUE), ProductionT = sum(ProductionT, na.rm = TRUE))
  
  outt <- shock.analysis(temp, ts.col.name = "ValueAUD")
  outt$shock.group <- paste(taxa[j])
  val_shocks <- val_shocks %>% bind_rows(outt)
  
  outt <- shock.analysis(temp, ts.col.name = "ProductionT")
  outt$shock.group <- paste(taxa[j])
  prod_shocks <- prod_shocks %>% bind_rows(outt)
}

#____________________________________________________________________________________________#
# Loop through jurisdiction
#____________________________________________________________________________________________#
jurisdiction <- unique(df$Jurisdiction)

# Create dataframes
j = 1
temp <- df %>%
  filter(Jurisdiction == jurisdiction[j]) %>%
  group_by(Year) %>%
  summarise(ValueAUD = sum(ValueAUD, na.rm = TRUE), ProductionT = sum(ProductionT, na.rm = TRUE))

val_shocks <- shock.analysis(temp, ts.col.name = "ValueAUD")
val_shocks$shock.group <- paste(jurisdiction[j])

prod_shocks <- shock.analysis(temp, ts.col.name = "ProductionT")
prod_shocks$shock.group <- paste(jurisdiction[j])

for(j in 2:length(jurisdiction)){
  temp <- df %>%
    filter(Jurisdiction == jurisdiction[j]) %>%
    group_by(Year) %>%
    summarise(ValueAUD = sum(ValueAUD, na.rm = TRUE), ProductionT = sum(ProductionT, na.rm = TRUE))
  
  outt <- shock.analysis(temp, ts.col.name = "ValueAUD")
  outt$shock.group <- paste(jurisdiction[j])
  val_shocks <- val_shocks %>% bind_rows(outt)
  
  outt <- shock.analysis(temp, ts.col.name = "ProductionT")
  outt$shock.group <- paste(jurisdiction[j])
  prod_shocks <- prod_shocks %>% bind_rows(outt)
}