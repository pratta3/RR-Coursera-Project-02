
### Data Processing ###
#======================


# Download the .csv.bz2 file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, destfile = "stormData.csv.bz2")

# Unzip the bz2 file, store it in a temporary file, read in
# data using fread, and finally delete the temporary file.
library(R.utils)
library(data.table)
file <- bunzip2("stormData.csv.bz2", temporary = TRUE, remove = FALSE)
storms <- fread(file)
unlink(file, recursive = TRUE)





# select only 9 variables from storms
sub <- storms[,c(2,7,8,23,24,25,26,27,28)]

# define function that will calculate property and crop damage from given variables
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

# filter only events from 1996-2011 and calculate some values
library(lubridate)
library(dplyr)
p3 <- sub %>% 
        mutate(BGN_DATE = mdy_hms(BGN_DATE)) %>% # turn begin date into date format
        filter(year(BGN_DATE) %in% 1996:2011) %>% # filter only years 1996-2011
        mutate(EVTYPE = toupper(EVTYPE), # make storm types all uppercase
               total.fat.inj = FATALITIES + INJURIES, # sum fatalities and injuries
               prop.dmg = dmg.convert(PROPDMG, PROPDMGEXP), # calculate property damage
               crop.dmg = dmg.convert(CROPDMG, CROPDMGEXP), # calculate crop damage
               total.dmg = prop.dmg + crop.dmg) %>% #sum property and crop damage
        select(-c(PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)) %>% # de-select variables you don't need
        arrange(BGN_DATE) # arrange by date





# Extract official event names from online database. Note that there
# are actually 49 unique events, not 48.
library(stringr)
eventsurl <- url("https://www.ncdc.noaa.gov/stormevents/choosedates.jsp?statefips=-999%2CALL")
event.names <- readLines(eventsurl)[467:515]
event.names <- event.names %>% str_extract(">.+<") %>% str_extract("[A-Z].+[a-z]") %>% toupper
event.names[25] <- "HURRICANE/TYPHOON" # manually correct a typo
close(eventsurl) # close connection

# See if there are any EVTYPE categories that are misspelled and could affect
# conclusions if they were spelled correctly and were included in the analysis.
counts <- p3 %>% 
        filter(total.fat.inj > 0 | total.dmg > 0) %>% # filter out events that did 0 health or economic damage
        group_by(EVTYPE) %>% 
        summarize(is.correct = ifelse(first(EVTYPE) %in% event.names, "yes", "no"), # see if each storm type
                  count = n(),                                               # has official spelling or not
                  total.fat = sum(FATALITIES), # sum statistics
                  total.inj = sum(INJURIES),
                  max.fat.inj = max(total.fat.inj),
                  total.fat.inj = sum(total.fat.inj),
                  total.prop.dmg = sum(prop.dmg),
                  total.crop.dmg = sum(crop.dmg),
                  max.total.dmg = max(total.dmg),
                  total.dmg = sum(total.dmg)) %>% 
        arrange(desc(total.fat.inj)) # arrange by total fatalities and injuries and by
# arrange(desc(total.dmg))   # total damages separately and look for misspelled storm categories
# that are in or near the top ten in the summary data frame





sub2 <- storms[,c(2,6,7,8,23,24,25,26,27,28,36)] %>%  # subset containing REMARKS and COUNTY
        mutate(EVTYPE = toupper(EVTYPE),
               BGN_DATE = mdy_hms(BGN_DATE))

sub2 %>% filter(EVTYPE %in% event.names) %>% 
        mutate(prop.dmg = dmg.convert(PROPDMG, PROPDMGEXP),
               crop.dmg = dmg.convert(CROPDMG, CROPDMGEXP),
               tot.dmg = prop.dmg + crop.dmg) %>% 
        group_by(EVTYPE) %>% 
        summarize(max.dmg = max(tot.dmg),
                  date = first(BGN_DATE[tot.dmg == max.dmg]),
                  county = first(COUNTYNAME[tot.dmg == max.dmg])) %>% 
        arrange(desc(max.dmg)) %>% 
        head(1) # The worst flood occurred on January 1, 2006 in Napa County.

sub2 %>% filter(year(BGN_DATE) %in% 2005:2006) %>% 
        filter(str_detect(REMARKS, "Napa River")) %>% 
        select(BGN_DATE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) # look similar?

sub2 %>% filter(year(BGN_DATE) %in% 2005:2006) %>% 
        filter(str_detect(REMARKS, "Napa River")) %>% 
        select(REMARKS)







p3 <- p3 %>% mutate(EVTYPE = ifelse(EVTYPE == "RIP CURRENTS", "RIP CURRENT", EVTYPE),
                    EVTYPE = ifelse(EVTYPE == "EXTREME COLD", "EXTREME COLD/WIND CHILL", EVTYPE),
                    EVTYPE = ifelse(EVTYPE == "HURRICANE", "HURRICANE/TYPHOON", EVTYPE),
                    EVTYPE = ifelse(EVTYPE == "TYPHOON", "HURRICANE/TYPHOON", EVTYPE),
                    EVTYPE = ifelse(EVTYPE == "STORM SURGE", "STORM SURGE/TIDE", EVTYPE),
                    EVTYPE = ifelse(EVTYPE == "TSTM WIND", "THUNDERSTORM WIND", EVTYPE),
                    EVTYPE = ifelse(EVTYPE == "WILD/FOREST FIRE", "WILDFIRE", EVTYPE),
                    EVTYPE = as.factor(EVTYPE))
# Change duplicate value (and typo!) to 0.
p3[p3$total.dmg == 115032500000, 7:9] <- 0





### Results ###
#==============

# Question 1 #
#=============



# health.levels is a vector of factor levels to be used to make
# the plot pretty later
health.levels <- p3 %>% 
        filter(FATALITIES > 0 | INJURIES > 0) %>% 
        group_by(EVTYPE) %>% 
        summarize(grand.total = sum(FATALITIES + INJURIES)) %>% 
        arrange(grand.total) %>% 
        pull(EVTYPE) %>% 
        as.character()

# health2 is a data frame containing the ten storm types that 
# cause the most fatalities + injuries
library(tidyr)
health2 <- p3 %>% 
        filter(FATALITIES > 0 | INJURIES > 0) %>% 
        group_by(EVTYPE) %>% 
        summarize(fatalities = sum(FATALITIES),
                  injuries = sum(INJURIES),
                  grand.total = sum(FATALITIES, INJURIES)) %>% 
        arrange(desc(grand.total)) %>% 
        gather("inj.fat", "count", fatalities, injuries) %>% # gather FATALITIES and INJURIES into key/value pair
        arrange(desc(grand.total)) %>% 
        head(20) %>% # filter top ten storm types
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

# theme simply stores all the information to make the plot look pretty
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

# title1, plot1, and caption1 will be arguments in grid.arrange()
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





# Question 2#
#============


# Calculate damage statistics for all EVTYPEs
damage <- p3 %>% 
        filter(total.dmg > 0) %>% 
        group_by(EVTYPE) %>% 
        summarize(total.prop = sum(prop.dmg),
                  total.crop = sum(crop.dmg)) %>% 
        mutate(grand.total = total.prop + total.crop) %>% 
        arrange(desc(grand.total))

damage.levels <- damage[1:10,] %>% # Create vector of EVTYPE factor levels
        arrange(grand.total) %>% 
        pull(EVTYPE) %>% 
        as.character

damage <- damage[1:10,] %>% 
        mutate(EVTYPE = factor(EVTYPE, levels = damage.levels)) %>% 
        gather("damage.type", "cost", total.crop, total.prop) %>% 
        mutate(damage.type = factor(damage.type, levels = c("total.prop", "total.crop"))) %>% 
        arrange(desc(grand.total))

# Make the plot
pal <- brewer.pal(6, "Paired")[5:6]
captionB <- "FIGURE 2.  The ten storm event types that have caused the most property and crop damage from 1996 to 
2011. The length of each bar shows the total property damage plus the total crop damage in billions of US dollars, and
the color of the bars represents the individual totals of property damage and crop damage.
Hurricanes have caused the most damage overall (about $87 billion) but droughts have caused the most crop
damage (about $13 billion)."
title2 <- textGrob("Storm events causing the most property \nand crop damage from 1996-2011",
                   x = unit(.1, "npc"),
                   just = c("left"),
                   gp = gpar(fontsize = 18, 
                             fontface = "bold"))
plot2 <- ggplot(damage, aes(EVTYPE, cost/1e9)) +
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




