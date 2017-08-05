
# Download the .csv.bz2 file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, destfile = "stormData.csv.bz2")

# Read in first few rows and store classes of variables
storms <- read.csv("stormData.csv.bz2", header = TRUE,
                   nrow = 3, stringsAsFactors = FALSE)
classes <- sapply(storms, class)

# Change the integers and logicals to characters because otherwise
# errors will occur reading in the data.

classes[classes == "integer" | classes == "logical"] <- "character"

# Read in full dataset specifying na.string = "".
# Still takes a while (almost 2 minutes!) but it works
storms <- read.csv("stormData.csv.bz2", header = TRUE,
                   colClasses = classes, stringsAsFactors = FALSE,
                   na.strings = "")


# Try making it faster.

# Can't figure out how to make fread work.
library(data.table)
storms <- fread("stormData.csv.bz2")
file <- file("stormData.csv.bz2")
storms <- fread(sprintf("bzcat %s | tr -d '\\000'", file))

file <- bzfile(description = "stormData.csv.bz2")
storms <- fread(file)

library(stringr)
wd <- str_c(getwd(), "stormdata.csv.bz2", sep = "/")

storms <- fread(str_c("bunzip -cq", wd, sep = " "))

# Can't figure out how to get sqldf to work
library(sqldf)
file <- file("stormData.csv.bz2")
storms <- read.csv.sql("stormData.csv.bz2", dbname = NULL)

# Tried using system and shell commands, couldn't figure out how to
# get it to work
system("bzip2 -d stormData.csv.bz2")
shell("bunzip2 stormData.csv.bz2")
shell("bzip2 -d stormData.csv.bz2")

# Back to fread, using bunzip2 from R.utils package. It works!
# system.time reduced to less than 30 seconds overall.
library(R.utils)
bunzip2(filename = "stormData.csv.bz2", destname = "stormData.csv",
        remove = FALSE)
system.time(bunzip2(filename = "stormData.csv.bz2", # user time: 20.24 sec
                    destname = "test.csv",
                    remove = FALSE))
system.time(storms <- fread("stormData.csv")) # user time: 6.47 sec
head(storms, 1)








