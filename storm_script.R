
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


# row 203917 has a typo in STATE, should be Miami County, KS
# There are a lot of STATE names that I don't recognize.
# E.g. "LC" appears to be by Lake St. Clair between Michigan and Ontario (Canada).
# I don't know where LC came from.

# Aha, LE looks like it stands for Lake Erie. I wonder if LC stands for Lake [St.] Clair
# This is probably the case. All the other L's correspond to other lakes. (E.g. LS = Lake
# Superior, LM = Lake Michigan, etc.)

# MH = Marshall Islands
# PH = Philippine Islands ?
# PK = somewhere in Alaska
# PM = somewhere near Guam
# PT = California, Pacific coast
# SL = St. Lawrence River in Quebec
# XX = somewhere near Montreal





