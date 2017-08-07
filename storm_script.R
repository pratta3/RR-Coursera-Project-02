
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


library(stringr)
# Pull out unique EVTYPEs from p3 and sort them alphabetically
events <- p3 %>% pull(EVTYPE) %>% str_trim %>% toupper %>% unique %>% sort
# Make TSTM = THUNDERSTORM
p3$EVTYPE <- str_replace(p3$EVTYPE, "TSTM", "THUNDERSTORM")



# If I just match EVTYPE directly to event.names with %in%, are some categories
# disproportionately affected? This is a difficult question to answer. I'll
# come back to it later. I'm going to try a different approach first: I'll
# try subsetting the data according to the damage that individual events
# caused before looking at the event types.


install.packages("stringdist")
library(stringdist)

a <- p3$EVTYPE %>% unique # unique EVTYPEs in p3
c <- a %in% event.names
c <- a[!c] # subset EVTYPEs in p3 that DO NOT appear in event.names

d <- p3 %>% filter(EVTYPE %in% c)


a <- amatch(events, event.names, maxDist = 1.4, weight = c(d = 0.2, i = 1, s = 1, t = 1))












