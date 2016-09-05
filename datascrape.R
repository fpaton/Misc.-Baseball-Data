library(reshape2)

# Index: index1= 8 (9) is American League (Nat League)
kIndex=9

DateScraper <- function(y, m, d, kIndex) {
  # Scrapes baseball-refrence standings page
  # Arg: 
  # y: year
  # m: month 
  # d: day
  # kIndex: 9 for NL, 8 for AL
  url <- paste0("http://www.baseball-reference.com/games/standings.cgi?year=",y,"&month=",m, "&day=",d,"&submit=Submit+Date")
  d <- readHTMLTable(url, stringsAsFactors = FALSE)
  d <- as.data.frame(d[kIndex])
  d
}

kToday="2016/08/14"  # End Date
kStart="2016/04/03"  # Start Date (Season Start)

# create a complete sequence of dates you want to scrape data for
dates <- as.data.frame(seq(as.Date(kStart), as.Date(kToday), by = "days"))
names(dates) <- "dates" 

# split the dates so that there are three separate inputs to feed the function
dates <- colsplit(dates$dates, "-", c("y", "m", "d"))

# use the do() function to iterate the scrape function over all the dates

out2.nl <- dates %>% group_by(y, m, d) %>% do(DateScraper(.$y, .$m, .$d, 9))
out2.al <- dates %>% group_by(y, m, d) %>% do(DateScraper(.$y, .$m, .$d, 8))

al.df <- out2.al %>% AddDates()
nl.df <- out2.nl %>% AddDates()

#format dates

AddDates <- function(x){
  # Adds a column of type Date
  # ARG: 
  #   x: A data frame pulled from above
  #
  # Return: Same Data frame with added column Date
  if (x$m <10) {
    x$m <- paste(0, x$m, sep="")
  }
  x$Date <- paste(x$y, x$m, x$d, sep="/")
  x$Date <- as.Date(x$Date, "%Y/%m/%d")
  return(x)
}
