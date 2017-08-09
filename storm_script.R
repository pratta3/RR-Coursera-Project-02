
# Download the .csv.bz2 file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, destfile = "stormData.csv.bz2")

# Unzip the bz2 file, store it in a temporary file, read in
# data using fread, and finally delete the temporary file.
library(R.utils)
library(data.table) # I forgot to include this in past updates. Need it for fread
file <- bunzip2("stormData.csv.bz2", temporary = TRUE, remove = FALSE)
storms <- fread(file)
unlink(file, recursive = TRUE)

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
        return(z)
}

sub <- storms[,c(2,7,8,23,24,25,26,27,28)]
sub2 <- storms[,c(2,6,7,8,23,24,25,26,27,28,36)] # Better subset contains REMARKS

# Convert BGN_DATE to POSIXct
library(lubridate)
library(dplyr)
sub$BGN_DATE <- mdy_hms(sub$BGN_DATE)
sub2 <- sub2 %>% mutate(BGN_DATE = mdy_hms(BGN_DATE),
                        EVTYPE = toupper(EVTYPE),
                        prop.dmg = dmg.convert(PROPDMG, PROPDMGEXP),
                        crop.dmg = dmg.convert(CROPDMG, CROPDMGEXP))

# Subset only the years 1996-2011, when all storm event types were available and
# standardized, fix a few important misspellings, and add variables that calculate
# total number of fatalities and injuries combined and total property and crop damage
# combined.
p3 <- sub %>% 
        filter(year(BGN_DATE) %in% 1996:2011) %>% 
        mutate(EVTYPE = toupper(EVTYPE),
               EVTYPE = ifelse(EVTYPE == "RIP CURRENTS", "RIP CURRENT", EVTYPE),
               EVTYPE = ifelse(EVTYPE == "EXTREME COLD", "EXTREME COLD/WIND CHILL", EVTYPE),
               EVTYPE = ifelse(EVTYPE == "HURRICANE", "HURRICANE/TYPHOON", EVTYPE),
               EVTYPE = ifelse(EVTYPE == "TYPHOON", "HURRICANE/TYPHOON", EVTYPE),
               EVTYPE = ifelse(EVTYPE == "STORM SURGE", "STORM SURGE/TIDE", EVTYPE),
               EVTYPE = ifelse(EVTYPE == "TSTM WIND", "THUNDERSTORM WIND", EVTYPE),
               EVTYPE = ifelse(EVTYPE == "WILD/FOREST FIRE", "WILDFIRE", EVTYPE),
               EVTYPE = as.factor(EVTYPE),
               total.fat.inj = FATALITIES + INJURIES,
               prop.dmg = dmg.convert(PROPDMG, PROPDMGEXP),
               crop.dmg = dmg.convert(CROPDMG, CROPDMGEXP),
               total.dmg = prop.dmg + crop.dmg) %>% 
        select(-c(PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)) %>% 
        arrange(BGN_DATE)

# Extract official event names from online database. Note that there
# are actually 49 unique events, not 48.
library(stringr) # Fort to include this in past updates. Need it for str_extract
eventsurl <- url("https://www.ncdc.noaa.gov/stormevents/choosedates.jsp?statefips=-999%2CALL")
event.names <- readLines(eventsurl)[467:515]
event.names <- event.names %>% str_extract(">.+<") %>% str_extract("[A-Z].+[a-z]") %>% toupper
event.names[25] <- "HURRICANE/TYPHOON" # manually correct a typo
close(eventsurl) # close connection

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
# "STORM SURGE" needs to be changed to "STORM SURGE/TIDE"
# "TSTM WIND" needs to be changed to "THUNDERSTORM WIND"
# "WILD/FOREST FIRE" needs to be changed to "WILDFIRE"
# "TYPHOON" needs to be changed to "HURRICANE/TYPHOON"




### HEALTH CONSEQUENCES ###
# =========================



# Questions:
# 1) How many total fatalities/injuries have been caused by a given event type?
# 2) How many individual events of that type have cause more than 0 fatalities/injuries?



health <- p3 %>% 
        filter(FATALITIES > 0 | INJURIES > 0) %>% 
        group_by(EVTYPE) %>% 
        summarize(num.events = n(),
                  grand.total = sum(FATALITIES + INJURIES),
                  total.fat = sum(FATALITIES),
                  max.fat = max(FATALITIES),
                  total.inj = sum(INJURIES),
                  max.inj = max(INJURIES)) %>% 
        arrange(desc(grand.total))

# All top ten storm types are official storm types
all(health$EVTYPE[1:10] %in% event.names)

health.levels <- p3 %>% 
        filter(FATALITIES > 0 | INJURIES > 0) %>% 
        group_by(EVTYPE) %>% 
        summarize(grand.total = sum(FATALITIES + INJURIES)) %>% 
        arrange(grand.total) %>% 
        pull(EVTYPE) %>% 
        as.character()

library(tidyr)
health2 <- p3 %>% 
        filter(FATALITIES > 0 | INJURIES > 0) %>% 
        group_by(EVTYPE) %>% 
        summarize(fatalities = sum(FATALITIES),
                  injuries = sum(INJURIES),
                  grand.total = sum(FATALITIES, INJURIES)) %>% 
        arrange(desc(grand.total)) %>% 
        gather("inj.fat", "count", fatalities, injuries) %>% 
        arrange(desc(grand.total)) %>% 
        head(20) %>% 
        mutate(EVTYPE = factor(EVTYPE, levels = health.levels), # Put factor levels in decreasing order
               inj.fat = factor(inj.fat, levels = c("injuries", "fatalities"))) %>% 
        select(-grand.total)
        

# Make a plot showing the ten storm event types that cause the most
# combined fatalities and injuries, broken down by fatalities and
# injuries for each storm event type.

library(ggplot2)
library(gridExtra)
library(grid)
library(RColorBrewer)
library(scales)
# theme stores all the information to make the plot look pretty
theme <- theme_classic() +
        theme(legend.position = c(.6,.25),
              legend.justification = c(0,0),
              legend.key.size = unit(2, "lines"),
              legend.text = element_text(size = 12),
              axis.text.y = element_text(color = "black"),
              axis.title.x = element_text(size = 16,
                                          margin = margin(20, 0, 0, 0)),
              plot.title = element_text(size = 18, 
                                        face = "bold",
                                        hjust = 0,
                                        margin = margin(0, 0, 30, 0)),
              axis.text = element_text(size = 12),
              panel.grid.major.x = element_line(color = "gray"),
              panel.grid.minor.x = element_line(color = "gray"),
              plot.caption = element_text(hjust = 0),
              plot.margin = margin(30,0,15,0))
captionA <- "FIGURE 1.  The ten storm events that have caused the highest number of deaths and injuries from
1996 to 2011. The length of the bars represents the total number of injuries plus the total number of deaths and
the color of the bars shows the individual totals of injuries and deaths. Tornadoes have caused by far the most
injuries (20,667 in total). Excessive heat has caused the most deaths (1,797 in total)."
title1 <- textGrob("Storm events causing the most \ndeaths and injuries from 1996-2011",
                   x = unit(.1, "npc"),
                   just = c("left"),
                   gp = gpar(fontsize = 18, 
                             fontface = "bold"))
plot1 <- ggplot(health2, aes(EVTYPE, count)) +
        geom_bar(stat = "identity", 
                 aes(fill = inj.fat)) +
        labs(x = "",
             y = "Total number of deaths and injuries") +
        scale_fill_brewer(name = "", 
                          labels = c("Injuries", "Deaths"),
                          palette = "Paired") +
        scale_y_continuous(labels = comma) +
        coord_flip() +
        theme
caption1 <- textGrob(str_wrap(captionA),
                     x = unit(.1, "npc"),
                     just = "left")

# Here's the actual plot
grid.arrange(plot1, top = title1, bottom = caption1)



# NOTE: the difference between HEAT and EXCESSIVE HEAT is the heat index threshold
# used to define them. A HEAT advisory occurs when the heat index is between 100 and
# 105 degrees. An EXCESSIVE HEAT warning occurs when the heat index reaches or exceeds
# 105 degrees. The heat index is a way of expressing the discomfort felt due the 
# combined effects of temperature and humidity.





### ECONOMIC CONSEQUENCES ###
# ===========================




# Change duplicate value (and typo!) to 0.
 p3[354415,7:9] <- 0

# Calculate damage statistics for all EVTYPEs
damage <- p3 %>% 
        filter(total.dmg > 0) %>% 
        group_by(EVTYPE) %>% 
        summarize(total.prop = sum(prop.dmg),
                  total.crop = sum(crop.dmg)) %>% 
        mutate(grand.total = total.prop + total.crop) %>% 
        arrange(desc(grand.total))

# The top ten storm types are all official storm types
all(damage$EVTYPE[1:10] %in% event.names)

damage.levels <- damage[1:10,] %>% # Create vector of EVTYPE factor levels
        arrange(grand.total) %>% 
        pull(EVTYPE) %>% 
        as.character

damage2 <- damage[1:10,] %>% 
        mutate(EVTYPE = factor(EVTYPE, levels = damage.levels)) %>% 
        gather("damage.type", "cost", total.crop, total.prop) %>% 
        mutate(damage.type = factor(damage.type, levels = c("total.prop", "total.crop"))) %>% 
        arrange(desc(grand.total))

# Make the plot
pal <- brewer.pal(6, "Paired")[5:6]
captionB <- "FIGURE 2.  The ten storm event types that have caused the most property and crop damage from 1996 to 2011.
The length of each bar shows the total property damage plus the total crop damage in billions of US dollars, and
the color of the bars represents the individual totals of property damage and crop damage.
Hurricanes have caused the most damage overall (about $87 billion) but droughts have caused the most crop
damage (about $13 billion)."
title2 <- textGrob("Storm events causing the most property \nand crop damage from 1996-2011",
                   x = unit(.1, "npc"),
                   just = c("left"),
                   gp = gpar(fontsize = 18, 
                             fontface = "bold"))
plot2 <- ggplot(damage2, aes(EVTYPE, cost/1e9)) +
        geom_bar(stat = "identity",
                 aes(fill = damage.type)) +
        labs(x = "",
             y = "Total damage in billions of US dollars") +
        scale_fill_manual(name = "", 
                          labels = c("Property damage", "Crop damage"),
                          values = pal) +
        coord_flip() +
        theme
caption2 <- textGrob(str_wrap(captionB),
                     x = unit(.1, "npc"),
                     just = c("left"))

# Here's the plot.
grid.arrange(plot2, top = title2, bottom = caption2)















