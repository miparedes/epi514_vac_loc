#brfss-influenza vaccine

rm(list = ls())
dataDir <- "/Users/apple/Desktop/2021 spring course/Epi 514/data/"
library(haven)

# data 2012
WA2012_raw <- read_dta (paste0(dataDir, "brfss12_v2.dta"))
write.csv(WA2012_raw, paste0(dataDir, "brfss12_V2.csv"), row.names = FALSE) 
names(WA2012)
names(WA2012) <- tolower(names(WA2012))

# data 2015
WA2015_raw <- read_dta (paste0(dataDir, "BRFSS2015v3a.dta"))
write.csv(WA2015_raw, paste0(dataDir, "BRFSS2015v3a.csv"), row.names = FALSE)  
WA2015 <- read.csv(paste0(dataDir, "BRFSS2015v3a.csv"))
names(WA2015_raw)
names(WA2015) 
WA2015$iyear[WA2015$iyear == 2016] <- "2015"
names(WA2015) <- tolower(names(WA2015))

# data 2018
WA2018_raw <- read_dta (paste0(dataDir, "WA_BRFSS_2018_v2.dta"))
write.csv(WA2018_raw, paste0(dataDir, "WA_BRFSS_2018_v2.csv"), row.names = FALSE)  
WA2018 <- read.csv(paste0(dataDir, "WA_BRFSS_2018_v2.csv"))
names(WA2018_raw)
names(WA2018)   #from _llcpwt to X_llcpwt
WA2018$iyear[WA2018$iyear == 2019] <- "2018"
names(WA2018) <- tolower(names(WA2018))

#weight
WA2011$finalwt <-WA2011$A_LLCPWT
WA2012$finalwt <-WA2012$a_llcpwt
WA2015$finalwt <-WA2015$x_llcpwt
WA2018$finalwt <-WA2018$x_llcpwt

# count overall observiations in data set to determine distribution for final sample
count_11 <- nrow(WA2011)
count_12 <- nrow(WA2012)
count_15 <- nrow(WA2015)
count_18 <- nrow(WA2018)

total <- count_12 + count_15 + count_18

nrow(WA2012)/total
nrow(WA2015)/total
nrow(WA2018)/total

# apply distribution to final weights
WA2012$finalwt <- WA2012$finalwt*0.3437191
WA2015$finalwt <- WA2015$finalwt*0.3617671
WA2018$finalwt <- WA2018$finalwt*0.2945138

# combine all years
library(plyr)
WA12_15_18 <-rbind.fill (WA2012, WA2015, WA2018) 

# keep the variables we need
library (dplyr)
WA_clean <- WA12_15_18 %>% select( "zipcode","zipcode1","flushot5",  "flushot6", "imfvplac", 
                                      "qstver", "x_llcpwt","a_llcpwt","employ", "employ1",
                                      "veteran3","cvdinfr4","cvdcrhd4","cvdstrk3","asthma3",
                                      "chccopd1","diabete3","x_bmi5","a_bmi5","sex","sex1",
                                      "age","x_age65yr","a_age65yr","x_race","race2",
                                      "x_imprace","medcost",
                                      "x_incomg","a_incomg","x_educag","a_educag",
                                      "iyear","chckdny1","chckidny","hlthpln1","finalwt")
                                      # "hlthplan" is for 2011, should be added later.


#subset the variables
WA_clean$zipcode[WA_clean$iyear == "2018"] <- WA_clean$zipcode1[WA_clean$iyear == "2018"]
WA_clean$flushot5[WA_clean$iyear == "2015" | WA_clean$iyear == "2018"] <- WA_clean$flushot6[WA_clean$iyear == "2015" | WA_clean$iyear == "2018"]
WA_clean$flushot <- WA_clean$flushot5
WA_clean$employ[WA_clean$iyear == "2015" | WA_clean$iyear == "2018"] <- WA_clean$employ1[WA_clean$iyear == "2015" | WA_clean$iyear == "2018"]
WA_clean$veteran <- WA_clean$veteran3
WA_clean$cvdinfr <- WA_clean$cvdinfr4
WA_clean$cvdcrhd <- WA_clean$cvdcrhd4
WA_clean$cvdstrk <- WA_clean$cvdstrk3
WA_clean$asthma <- WA_clean$asthma3
WA_clean$chccopd <- WA_clean$chccopd1
WA_clean$diabetes <- WA_clean$diabete3
WA_clean$chckidny[WA_clean$iyear == "2018"] <- WA_clean$chckdny1[WA_clean$iyear == "2018"]
WA_clean$x_bmi5[WA_clean$iyear == "2012"] <- WA_clean$a_bmi5[WA_clean$iyear == "2012"]
WA_clean$x_age65yr[WA_clean$iyear == "2012"] <- WA_clean$a_age65yr[WA_clean$iyear == "2012"]
WA_clean$x_llcpwt[WA_clean$iyear == "2012"] <- WA_clean$a_llcpwt[WA_clean$iyear == "2012"]
WA_clean$x_incomg[WA_clean$iyear == "2012"] <- WA_clean$a_incomg[WA_clean$iyear == "2012"]
WA_clean$x_educag[WA_clean$iyear == "2012"] <- WA_clean$a_educag[WA_clean$iyear == "2012"]
WA_clean$sex[WA_clean$iyear == "2018"] <- WA_clean$sex1[WA_clean$iyear == "2018"]
WA_clean$x_race[WA_clean$iyear == "2012"] <- WA_clean$race2[WA_clean$iyear == "2012"]
WA_clean$hlthpln <- WA_clean$hlthpln1 # need to deal with "HLTHPLAN" in 2011 later
WA_clean$year <- WA_clean$iyear

# keep the variables we need
WA_clean <- WA_clean %>% select( "zipcode","flushot","imfvplac", "qstver", "x_llcpwt","employ",
                                   "veteran","cvdinfr","cvdcrhd","cvdstrk","asthma",
                                   "chccopd","diabetes","chckidny","x_bmi5","sex",
                                   "age","x_age65yr","x_race","hlthpln1","medcost",
                                   "x_incomg","x_educag","year","finalwt")








