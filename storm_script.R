
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

# NOTE: I didn't bother changing ST to KS (see previous commit
# about figuring out the STATE abbreviations) because I'm not
# particularly interested in the STATE variable at this point
# and I'm also not absolutely certain that it's actually a typo.

# There are only supposed to be 48 unique EVTYPEs but there
# are 985!!
length(unique(sub$EVTYPE))
# Sort events in alphabetical order
library(dplyr)
library(stringr)
events <- sub %>% pull(EVTYPE) %>% unique %>% str_trim %>% sort
# This will take some time. First I'll figure out how I want to
# subset the data for the analysis and then I'll work on fixing
# the event types.






