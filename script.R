file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
file_path <- "stormdata.csv.bz2"
download.file(file_url, file_path, method = "curl")
stormdata <- read.csv(file_path, stringsAsFactors = FALSE)
head(stormdata)
summary(stormdata)
str(stormdata)

library(dplyr)
harmful_health <- stormdata %>%
        select(EVTYPE, FATALITIES, INJURIES)

harmful_health$EVTYPE <- as.factor(harmful_health$EVTYPE)

harmful_health$FatalorInjury <- harmful_health$FATALITIES + harmful_health$INJURIES

harmful_health <- harmful_health %>%
        select(EVTYPE, FatalorInjury)

harmful_health_summary <- harmful_health %>%
        group_by(EVTYPE) %>%
                summarise(Harm = sum(FatalorInjury)) %>%
                        arrange(desc(Harm))

harmful_health_summary_head <- head(harmful_health_summary)



harmful_econ <- stormdata %>%
        select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

harmful_econ$EVTYPE <- as.factor(harmful_econ$EVTYPE)

harmful_econ <- harmful_econ %>%
        filter(!(PROPDMG == 0 & CROPDMG == 0)) %>%
                mutate(Harm = ifelse(PROPDMG == 0, 
                                     ifelse(CROPDMGEXP == "K", 
                                            CROPDMG * 1000, 
                                            ifelse(CROPDMGEXP == "M", 
                                                   CROPDMG * 1000000,
                                                   ifelse(CROPDMGEXP == "B", 
                                                          CROPDMG * 1000000000,
                                                          0))),
                                     ifelse(CROPDMG == 0,
                                            ifelse(PROPDMGEXP == "K",
                                                   PROPDMG * 1000,
                                                   ifelse(PROPDMGEXP == "M",
                                                          PROPDMG * 1000000,
                                                          ifelse(PROPDMGEXP == "B",
                                                                 PROPDMG * 1000000000,
                                                                 0))),
                                            ifelse(CROPDMGEXP == "K",
                                                   ifelse(PROPDMGEXP == "K",
                                                          CROPDMG * 1000 + PROPDMG * 1000,
                                                          ifelse(PROPDMGEXP == "M",
                                                                 CROPDMG * 1000 + PROPDMG * 1000000,
                                                                 CROPDMG * 1000 + PROPDMG * 1000000000)),
                                                   ifelse(CROPDMGEXP == "M",
                                                          ifelse(PROPDMGEXP == "K",
                                                                 CROPDMG * 1000000 + PROPDMG * 1000,
                                                                 ifelse(PROPDMGEXP == "M",
                                                                        CROPDMG * 1000000 + PROPDMG * 1000000,
                                                                        CROPDMG * 1000000 + PROPDMG * 1000000000)),
                                                          ifelse(PROPDMGEXP == "K",
                                                                 CROPDMG * 1000000000 + PROPDMG * 1000,
                                                                 ifelse(PROPDMGEXP == "M",
                                                                        CROPDMG * 1000000000 + PROPDMG * 1000000,
                                                                        CROPDMG * 1000000000 + PROPDMG * 1000000000)))))))

harmful_econ_summary <- harmful_econ %>%
        select(EVTYPE, Harm) %>%
                group_by(EVTYPE) %>%
                        summarise(Harm = sum(Harm)) %>%
                                arrange(desc(Harm))

head(harmful_econ_summary)
