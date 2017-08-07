
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

length(unique(p2$EVTYPE)) # only one unique EVTYPE in p1
length(unique(p2$EVTYPE)) # Still way too many EVTYPEs in p2 and p3
length(unique(p3$EVTYPE))

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

# Match EVTYPES from p3 with official event types using amatch.
# Play around with the parameters.
install.packages("stringdist")
library(stringdist)
a <- amatch(events, event.names, maxDist = 1.4, weight = c(d = 0.2, i = 1, s = 1, t = 1))
event.df <- data.frame(events, a) %>% mutate(lookup = event.names[a]) %>% arrange(a)
sum(!is.na(event.df$lookup))

event.df$lookup[67:68] <- "STRONG WIND" # Manually correct just a couple bad ones

# Rename EVTYPE with the corrected spelling
colnames(event.df) <- c("EVTYPE", "A", "LOOKUP")
temp <- p3 %>% left_join(event.df, by = "EVTYPE") %>% mutate(EVTYPE = LOOKUP) %>% select(-c(LOOKUP, A))
sum(complete.cases(temp))



# Is it even worth trying to match the misspelled EVTYPEs?


temp2 <- p3 %>% filter(EVTYPE %in% event.names) # p3 subset where EVTYPE is spelled correctly
nrow(temp2)/nrow(p3) # Just matching directly retains almost 98% of the dataset.
# [1] 0.9783606
sum(complete.cases(temp))/nrow(p3) # Comnpare it to the amatch'd subset
# [1] 0.9829862

# I don't think it's even worth it to use amatch. (NOTE that p3 has 653,530 rows)



# > Sys.time()
# [1] "2017-08-07 16:19:56 EDT"
# 
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
#         [1] stringdist_0.9.4.6 lubridate_1.6.0    bindrcpp_0.2       dplyr_0.7.1        R.utils_2.5.0      R.oo_1.21.0        R.methodsS3_1.7.1 
# [8] stringr_1.2.0      sqldf_0.4-11       RSQLite_2.0        gsubfn_0.6-6       proto_1.0.0        data.table_1.10.4 
# 
# loaded via a namespace (and not attached):
#         [1] Rcpp_0.12.11     bindr_0.1        magrittr_1.5     bit_1.1-12       R6_2.2.2         rlang_0.1.1      blob_1.1.0      
# [8] tcltk_3.3.3      tools_3.3.3      parallel_3.3.3   DBI_0.7          bit64_0.9-7      digest_0.6.12    assertthat_0.2.0
# [15] tibble_1.3.3     glue_1.1.1       memoise_1.1.0    stringi_1.1.5    chron_2.3-50     pkgconfig_2.0.1 







