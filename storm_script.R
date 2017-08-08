
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



# > Sys.time()
# [1] "2017-08-07 22:43:34 EDT"
# > sessionInfo()
# R version 3.3.3 (2017-03-06)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# locale:
#         [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    
# 
# attached base packages:
#         [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#         [1] ggplot2_2.2.1      stringdist_0.9.4.6 lubridate_1.6.0    bindrcpp_0.2       dplyr_0.7.1        R.utils_2.5.0      R.oo_1.21.0       
# [8] R.methodsS3_1.7.1  stringr_1.2.0      sqldf_0.4-11       RSQLite_2.0        gsubfn_0.6-6       proto_1.0.0        data.table_1.10.4 
# 
# loaded via a namespace (and not attached):
#         [1] Rcpp_0.12.11     plyr_1.8.4       bindr_0.1        tools_3.3.3      digest_0.6.12    bit_1.1-12       memoise_1.1.0   
# [8] tibble_1.3.3     gtable_0.2.0     pkgconfig_2.0.1  rlang_0.1.1      DBI_0.7          parallel_3.3.3   bit64_0.9-7     
# [15] grid_3.3.3       glue_1.1.1       R6_2.2.2         tcltk_3.3.3      blob_1.1.0       magrittr_1.5     scales_0.4.1    
# [22] assertthat_0.2.0 colorspace_1.3-2 stringi_1.1.5    lazyeval_0.2.0   munsell_0.4.3    chron_2.3-50    











