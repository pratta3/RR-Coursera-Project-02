
# Download the .csv.bz2 file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, destfile = "stormData.csv.bz2")

# Unzip the bz2 file, store it in a temporary file, read in
# data using fread, and finally delete the temporary file.
library(R.utils)
file <- bunzip2("stormData.csv.bz2", temporary = TRUE, remove = FALSE)
storms <- fread(file)
unlink(file, recursive = TRUE)

sub <- storms[,c(2,7,8,23,24,25,26,27,28)]

# Every date in BGN_DATE has time 0:00:00
sum(str_detect(sub$BGN_DATE, "0:00:00"))

# Convert BGN_DATE to POSIXct
library(lubridate)
sub$BGN_DATE <- mdy_hms(sub$BGN_DATE)

# Break up the data into three periods based on the EVTYPES
# recorded in each period
library(dplyr)
p1 <- sub %>% filter(year(BGN_DATE) %in% 1950:1954) %>% arrange(BGN_DATE) # only tornadoes
p2 <- sub %>% filter(year(BGN_DATE) %in% 1955:1995) %>% arrange(BGN_DATE) # tornadoes, thunderstorm wind, and hail
p3 <- sub %>% filter(year(BGN_DATE) %in% 1996:2011) %>% arrange(BGN_DATE) # all events

# Extract official event names from online database. Note that there
# are actually 49 unique events, not 48.
eventsurl <- url("https://www.ncdc.noaa.gov/stormevents/choosedates.jsp?statefips=-999%2CALL")
event.names <- readLines(eventsurl)[467:515]
event.names <- event.names %>% str_extract(">.+<") %>% str_extract("[A-Z].+[a-z]") %>% toupper
event.names[25] <- "HURRICANE/TYPHOON" # manually correct a typo

# See if there are any EVTYPE categories that are misspelled and could affect
# conclusions if they were spelled correctlly and were included in the analysis.
counts <- p3 %>% 
        filter(FATALITIES > 0 | INJURIES > 0) %>% 
        mutate(EVTYPE = toupper(EVTYPE)) %>% 
        group_by(EVTYPE) %>% 
        summarize(is.correct = ifelse(first(EVTYPE) %in% event.names, "yes", "no"),
                  count = n(),
                  total.fat = sum(FATALITIES),
                  total.inj = sum(INJURIES)) %>% 
        arrange(desc(total.inj))

# "RIP CURRENTS" needs to be changed to "RIP CURRENT"
# "EXTREME COLD" needs to be changed to "EXTREME COLD/WIND CHILL"
# "HURRICANE" needs to be changed to "HURRICANE/TYPHOON"

p3 <- p3 %>% mutate(EVTYPE = toupper(EVTYPE),
                    EVTYPE = ifelse(EVTYPE == "RIP CURRENTS", "RIP CURRENT", EVTYPE),
                    EVTYPE = ifelse(EVTYPE == "EXTREME COLD", "EXTREME COLD/WIND CHILL", EVTYPE),
                    EVTYPE = ifelse(EVTYPE == "HURRICANE", "HURRICANE/TYPHOON", EVTYPE),
                    EVTYPE = as.factor(EVTYPE))



### HEALTH CONSEQUENCES ###
# =========================



# Questions:
# 1) How many total fatalities/injuries have been caused by a given event type?
# 2) How many individual events of that type have cause more than 0 fatalities/injuries?



health <- p3 %>% 
        filter(FATALITIES > 0 | INJURIES > 0) %>% 
        group_by(EVTYPE) %>% 
        summarize(num.events = n(),
                  total.fat = sum(FATALITIES),
                  mean.fat = mean(FATALITIES),
                  median.fat = median(FATALITIES),
                  max.fat = max(FATALITIES),
                  total.inj = sum(INJURIES),
                  mean.inj = mean(INJURIES),
                  median.inj = median(INJURIES),
                  max.inj = max(INJURIES)) %>% 
        arrange(desc(num.events))

fat <- arrange(health, desc(total.fat))[1:10,] # top ten fatalities
inj <- arrange(health, desc(total.inj))[1:10,] # top ten injuries
num <- arrange(health, desc(num.events))[1:10,] # top ten most frequent events

# plot showing top ten total fatalities
par(las = 1, mar = c(5,10,4,2), mfrow = c(1,1))
with(fat[10:1,], 
     barplot(total.fat, 
             names.arg = EVTYPE,
             horiz = TRUE,
             cex.names = .75,
             xlim = c(0,2000),
             main = "Total number of fatalities from 1996-2011",
             xlab = "Number of fatalities"))
# plot showing top ten total injuries
with(inj[10:1,],
     barplot(total.inj, 
             names.arg = EVTYPE,
             horiz = TRUE,
             cex.names = .75,
             xlim = c(0, 21000),
             xaxp = c(0, 21000, 7),
             main = "Total number of injuries from 1996-2011",
             xlab = "Number of injuries"))

# NOTE: the difference between HEAT and EXCESSIVE HEAT is the heat index threshold
# used to define them. A HEAT advisory occurs when the heat index is between 100 and
# 105 degrees. An EXCESSIVE HEAT warning occurs when the heat index reaches or exceeds
# 105 degrees. The heat index is a way of expressing the discomfort felt due the 
# combined effects of temperature and humidity.





### ECONOMIC CONSEQUENCES ###
# ===========================




# Fix typos (or is it a typo after all?)
# p3$PROPDMGEXP[354415] <- "M"

# Look at PROPDMG and CROPDMG

# All cases where PROPDMGEXP is equal to "", PROPDMG is equal to 0.
nrow(p3[p3$PROPDMG == 0 & p3$PROPDMGEXP == "",]) == sum(p3$PROPDMGEXP == "")
# Also the only case where PROPDMGEXP is 0, PROPDMG is also equal to 0.
head(p3[p3$PROPDMGEXP == 0,])
# Same applies for CROPDMGEXP and CROPDMG. (There aren't any 0's in CROPDMGEXP.)
nrow(p3[p3$CROPDMG == 0 & p3$CROPDMGEXP == "",]) == sum(p3$CROPDMGEXP == "")

# dmg.convert is a function that calculates the literal dollar amount
# from PROPDMG and PROPDMGEXP (or CROPDMG and CROPDMGEXP).
dmg.convert <- function(x, y){
        z <- rep(NA, length(x))
        for(i in 1:length(x)){
                if(y[i] == "" | y[i] == "0"){
                        z[i] <- 0
                } else if(y[i] == "K"){
                        z[i] <- x[i]*10^3
                } else if(y[i] == "M"){
                        z[i] <- x[i]*10^6
                } else if(y[i] == "B"){
                        z[i] <- x[i]*10^9
                }
        }
        z
}

# Create two new variables with literal dollar amount for
# property and crop damage.
p3$prop.dmg <- dmg.convert(p3$PROPDMG, p3$PROPDMGEXP)
p3$crop.dmg <- dmg.convert(p3$CROPDMG, p3$CROPDMGEXP)

money <- p3 %>% 
        filter(prop.dmg > 0 | crop.dmg > 0) %>% 
        group_by(EVTYPE) %>% 
        summarize(num.events = n(),
                  total.prop = sum(prop.dmg),
                  mean.prop = mean(prop.dmg),
                  median.prop = median(prop.dmg),
                  max.prop = max(prop.dmg),
                  total.crop = sum(crop.dmg),
                  mean.crop = mean(crop.dmg),
                  median.crop = median(crop.dmg),
                  max.crop = max(crop.dmg)) %>% 
        arrange(desc(total.prop))

# I can't figure out why there are ZERO fatalities and injuries
# listed for Hurricane Katrina. These and the property and crop damage
# numberes do not match up with the data from the online database...




