library(dplyr)

#PROJECT: Shocks to Australian seafood

####################################################
#
# INITIAL TASKS
#
#  1) Filter time series into appropriate jurisidiction and taxa groups
#  2) Aggregate (combine) appropriate time series (e.g. cuttlefish + squid + octopus = new cephalopod aggregated dataframe)
#  3) Account for/ remove missing values "NA"
#
#AFTER:
# Transform data when neccessary to fit Gephart code
## 4) Run Gephart code  
#
####################################################


#Load data

data = read.csv("Trialdata.csv")

head(data)
str(data)
data$Year <- as.factor(data$Year) 


#####################################################
#1) Filter into groups (taxa, jurisdiction) as needed
#####################################################

#Cephalopod aggregate filter 

## Attempt 1
######### This cumulative approach didn't work
#data_NSWceph <- data %>% 
#              select(Jurisdiction, Year, Sector, Taxa, Species, ValueAUD, ProductionT) %>% 
#              filter(Jurisdiction == "NSW" 
#                     & Species %in% c("squid", "cuttlefish", "octopus"))   #squid_octopus not in NSW
#
# head(data_NSWceph)
########


# Attempt 2. 
# Task: Separate out into individual dataframes, merge df under the same year and then add totals

#### Separating the individual dataframes

#NSW squid
data_NSWsquid <- data %>% 
  select(Jurisdiction, Year, Sector, Taxa, Species, ValueAUD, ProductionT) %>% 
  filter(Jurisdiction == "NSW" 
         & Species %in% c("squid")) 
#NSW cuttlefish
data_NSWcuttle <- data %>% 
  select(Jurisdiction, Year, Sector, Taxa, Species, ValueAUD, ProductionT) %>% 
  filter(Jurisdiction == "NSW" 
         & Species %in% c("cuttlefish")) 
#NSWoctopus
data_NSWoct <- data %>% 
  select(Jurisdiction, Year, Sector, Taxa, Species, ValueAUD, ProductionT) %>% 
  filter(Jurisdiction == "NSW" 
         & Species %in% c("octopus")) 

### b) merge under the same year and create 1 combined dataframe
NSWceph1 <- merge (data_NSWsquid, data_NSWcuttle, by = 'Year')
NSWceph2 <- merge (NSWceph1, data_NSWoct, by = 'Year')


#####################################################
#2) Aggregate production and values totals. 

# 3) Code is included to remove NAs. NAs considered as 0's but only so addition possible. Won't affect time series length 
#####################################################


NSWceph3 <- NSWceph2 %>%
replace(is.na(.), 0) %>%           #treat NAs as 0 (JUST FOR AGGREGATIONS!)
 mutate(ProductionTOTAL = ProductionT.x + ProductionT.y + ProductionT)    #addition of production columns

NSWceph4 <- NSWceph3 %>%
  replace(is.na(.), 0) %>%           #treat NAs as 0 (JUST FOR AGGREGATIONS!)
  mutate(ValueTOTAL = ValueAUD.x + ValueAUD.y + ValueAUD)    #addition of production columns

head(NSWceph4)
View(NSWceph4)

#### Tidy the dataframe if you would like by removing unneccessary columns 
NSWceph5 <- select(NSWceph4, -c(Jurisdiction.x, Sector.x, Taxa.x, ValueAUD.x, ProductionT.x, 
                                Jurisdiction.y,  Sector.y, Taxa.y, ValueAUD.y, ProductionT.y,
                                ProductionT, ValueAUD))
head(NSWceph5)

write.csv(NSWceph5, "NSWcephaggregate.csv")
