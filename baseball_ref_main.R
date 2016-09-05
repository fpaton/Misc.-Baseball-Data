# latest update: 2016-08-21
# Created by: Forrest Paton, SFU Statistics Senior Undergraduate.
#
# Inspired by Bill Petti's (The Hardball Times) "A Short(-ish) Introduction to 
# using R Packages for Baseball Research". I've recoded parts for readibility (spacing, inneficiencies)
# as well as added functions ( AddDates() ) and introduced general constant terms by removing hardcoded variables
# in functions. 
#
#
library(reshape2)
library(XML)
library(magrittr)
library(dplyr)
# Runtime (CPU, USER) for full season ~ 5 minutes (per League)

# kIndex: 8 = American League, 9 = National League, 7=NL West, 6= NL Cent, 5=NL East, 4=AL west, 
# 3= AL Cent, 2= AL East

kIndex.mlb=4  
kStart="2016/04/03"  # Start Date (Season Start)
kToday="2016/08/20"  # End Date


# create sequence of dates
dates <- as.data.frame(seq(as.Date(kStart), as.Date(kToday), by= "days"))
names(dates) <- "dates" 

# split the dates so that there are three separate inputs to feed the function
dates <- colsplit(dates$dates, "-", c("y", "m", "d"))

out.mlb <- dates %>% 
          group_by(y, m, d) %>% 
          do(DateScraper(.$y, .$m, .$d, kIndex.mlb)) %>% 
          AddDates()

mlb.df <- out.mlb  # Duplicate df for manipulation/ testing
                  # this avoids having to scrape data again if you make an error
