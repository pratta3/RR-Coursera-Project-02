
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

# Questions:
# 1) How many total fatalities/injuries have been caused by a given event type?
# 2) How many individual events of that type have cause more than 0 fatalities/injuries?

health <- p3 %>% 
        filter(FATALITIES > 0 | INJURIES > 0) %>% 
        mutate(EVTYPE = as.factor(EVTYPE)) %>% 
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

fat <- arrange(health, desc(total.fat))
inj <- arrange(health, desc(total.inj))
num <- arrange(health, desc(num.events))

# plot showing top ten total fatalities
par(mar = c(5,9,4,2), las = 2)
with(fat[10:1,], 
     barplot(total.fat, 
             names.arg = EVTYPE,
             horiz = TRUE,
             xlim = c(0,2000),
             cex.names = .75,
             main = "Total number of fatalities from 1996-2011",
             xlab = "Total fatalities"))
# plot showing top ten total injuries
with(inj[10:1,],
     barplot(total.inj, 
             names.arg = EVTYPE,
             horiz = TRUE,
             # xlim = c(0,2000),
             cex.names = .75,
             main = "Total number of injuries from 1996-2011",
             xlab = "Total injuries"))

# NOTE: the difference between HEAT and EXCESSIVE HEAT is the heat index threshold
# used to define them. A HEAT advisory occurs when the heat index is between 100 and
# 105 degrees. An EXCESSIVE HEAT warning occurs when the heat index reaches or exceeds
# 105 degrees. The heat index is a way of expressing the discomfort felt due the 
# combined effects of temperature and humidity.






# Extract official event names from online database. Note that there
# are actually 49 unique events, not 48.
eventsurl <- url("https://www.ncdc.noaa.gov/stormevents/choosedates.jsp?statefips=-999%2CALL")
event.names <- readLines(eventsurl)[467:515]
event.names <- event.names %>% str_extract(">.+<") %>% str_extract("[A-Z].+[a-z]") %>% toupper
event.names[25] <- "HURRICANE/TYPHOON" # manually correct a typo


library(stringr)
# Pull out unique EVTYPEs from p3 and sort them alphabetically
events <- p3 %>% pull(EVTYPE) %>% str_trim %>% toupper %>% unique %>% sort
# Make TSTM = THUNDERSTORM
p3$EVTYPE <- str_replace(p3$EVTYPE, "TSTM", "THUNDERSTORM")
















