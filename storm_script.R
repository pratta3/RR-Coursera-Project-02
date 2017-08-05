
# Download the .csv.bz2 file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, destfile = "stormData.csv.bz2")

# First attempt reading in data took a long time so let's try speeding it up.


storms <- read.csv("stormData.csv.bz2", header = TRUE, # read in just a few rows
                   nrow = 3, stringsAsFactors = FALSE)

classes <- sapply(storms, class) # store classes of variables

# Try reading in data again. Error occurred: the third variable doesn't look
# like an integer.
storms <- read.csv("stormData.csv.bz2", header = TRUE,
                   stringsAsFactors = FALSE, 
                   colClasses = classes)

classes[3] <- "character" # change class of third variable to character

# Try reading in data again. Another error occurred with the only other
# variable with class "integer".
storms <- read.csv("stormData.csv.bz2", header = TRUE,
                   stringsAsFactors = FALSE, colClasses = classes)

classes[classes == "integer"] <- "character" # change class of other integer variable to character

# Try reading in data again. STILL, an error occurred.
# This time, there was a problem with a variable with
# class "logical".
storms <- read.csv("stormData.csv.bz2", header = TRUE,
                   stringsAsFactors = FALSE, colClasses = classes)

classes[classes == "logical"] <- "character" # change all logicals to characters

# Try reading in data again. Still takes a while (almost two minutes!)
# but it works.
storms <- read.csv("stormData.csv.bz2", header = TRUE,
                   stringsAsFactors = FALSE, colClasses = classes)
