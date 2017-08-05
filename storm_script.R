
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

# Read in full dataset. Still takes a while (almost 2 minutes!) but it works
storms <- read.csv("stormData.csv.bz2", header = TRUE,
                   colClasses = classes, stringsAsFactors = FALSE)

# PROBLEM: Now I don't have the NA's like I did before. Let's try and figure that out.

a <- read.csv("stormData.csv.bz2", header = TRUE,
              nrow = 3, stringsAsFactors = FALSE)

a # Looks like missing values are coded with ""

# Try reading in the data again specifying na.strings
storms <- read.csv("stormData.csv.bz2", header = TRUE,
                   colClasses = classes, stringsAsFactors = FALSE,
                   na.strings = "")

head(storms, 3) # Looks like it was successful
