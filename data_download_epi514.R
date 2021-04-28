#R code

#brfss: influenza vaccine

rm(list = ls())
dataDir <- "/Users/apple/Desktop/2021 spring course/Epi 514/data/"
library(haven)

brfssRaw2011 <- read_xpt (paste0(dataDir, "LLCP2011.XPT"))
#make the variables' name more comfortable to read, e.g. from _STATE to X_STATE
write.csv(brfssRaw2011, paste0(dataDir, "LLCP2011.csv"), row.names = FALSE) 
brfss2011 <- read.csv(paste0(dataDir, "LLCP2011.csv"))
names(brfssRaw2011)

brfssRaw2013 <- read_xpt (paste0(dataDir, "LLCP2013.XPT"))
write.csv(brfssRaw2013, paste0(dataDir, "LLCP2013.csv"), row.names = FALSE)
brfss2013 <- read.csv(paste0(dataDir, "LLCP2013.csv"))
names(brfssRaw2013)

brfssRaw2015 <- read_xpt (paste0(dataDir, "LLCP2015.XPT "))
write.csv(brfssRaw2015, paste0(dataDir, "LLCP2015.csv"), row.names = FALSE)
brfss2015 <- read.csv(paste0(dataDir, "LLCP2015.csv"))
names(brfssRaw2015)

brfssRaw2018 <- read_xpt (paste0(dataDir, "LLCP2018.XPT "))
write.csv(brfssRaw2018, paste0(dataDir, "LLCP2018.csv"), row.names = FALSE)
brfss2018 <- read.csv(paste0(dataDir, "LLCP2018.csv"))
names(brfssRaw2018)


# Pull out states that used this module in 2011
WA2011 <-brfss2011[brfss2011$X_STATE %in% c(53), ]
WA2011$finalwt <-WA2011$X_LLCPWT

# Pull out states that used this module in 2013
WA2013 <-brfss2013[brfss2013$X_STATE %in% c(53), ]
WA2013$finalwt <-WA2013$X_LLCPWT

# Pull out states that used this module in 2015
WA2015 <-brfss2015[brfss2015$X_STATE %in% c(53), ]
WA2015$finalwt <-WA2015$X_LLCPWT

# Pull out states that used this module in 2018
WA2018 <-brfss2018[brfss2018$X_STATE %in% c(53), ]
WA2018$finalwt <-WA2018$X_LLCPWT

# count overall observations in data set to determine distribution for final sample
count_11 <- nrow(WA2011)
count_13 <- nrow(WA2013)
count_15 <- nrow(WA2015)
count_18 <- nrow(WA2018)

total <- count_11 + count_13 + count_15 + count_18

nrow(WA2011)/total
nrow(WA2013)/total
nrow(WA2015)/total
nrow(WA2018)/total

# apply distribution to final weights
WA2011$finalwt <- WA2011$finalwt*0.2677542
WA2013$finalwt <- WA2013$finalwt*0.2023201
WA2015$finalwt <- WA2015$finalwt*0.2921153
WA2018$finalwt <- WA2018$finalwt*0.2378104

# combine all years
library(plyr)
WA11_13_15_18 <-rbind.fill (WA2011, WA2013, WA2015, WA2018) 


library (dplyr)
names(WA11_13_15_18)
WA_clean <- WA11_13_15_18 %>% select( "FLUSHOT5",  "FLUSHOT6", "IMFVPLAC", 
                                      "QSTVER", "X_LLCPWT", "EMPLOY", "EMPLOY1",
                                      "VETERAN3","CVDINFR4","CVDCRHD4","CVDSTRK3","ASTHMA3",
                                      "CHCCOPD1","DIABETE3","X_BMI5","SEX","SEX1",
                                      "AGE","X_AGE65YR","X_RACE","X_RACEGR2","X_RACEGR3","X_IMPRACE","HLTHPLN1","MEDCOST","X_INCOMG","X_EDUCAG",
                                      "IYEAR","CHCKIDNY","HLTHPLN1")

"ZIPCODE","ZIPCODE1",(???)




