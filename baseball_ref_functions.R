DateScraper <- function(y, m, d, kIndex.mlb) {
  # Scrapes baseball-reference standings page
  # Arg: 
  # y: year
  # m: month 
  # d: day
  # kIndex: defined at beggining of program
  #
  # Returns a data frame
  url <- paste0("http://www.baseball-reference.com/games/standings.cgi?year=",
                y, "&month=", m, "&day=", d, "&submit=Submit+Date")
  df <- readHTMLTable(url, stringsAsFactors= FALSE)
  df <- as.data.frame(df[kIndex.mlb])
  
  return(df)
}

AddDates <- function(x){
  # Adds a column of type Date
  # ARG: 
  #   x: A data frame pulled from above
  #
  # Return: Same Data frame with added column Date
  if (x$m <10) {
    x$m <- paste(0, x$m, sep= "")
  }
  x$Date <- paste(x$y, x$m, x$d, sep= "/")
  x$Date <- as.Date(x$Date, "%Y/%m/%d")
  return(x)
}


