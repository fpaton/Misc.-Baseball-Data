#Daily Updated Games

#Get Current Data:
year01=2016
month01=07
day01=04
al=8
nl=9
date_scrape <- function(y,m,d,x) {
  url <- paste0("http://www.baseball-reference.com/games/standings.cgi?year=",y,"&month=",m, "&day=",d,"&submit=Submit+Date")
  d <- readHTMLTable(url, stringsAsFactors = FALSE)
  d <- as.data.frame(d[x])
  d
}

al01=date_scrape(year01,month01,day01,al)
nl01=date_scrape(year01,month01,day01,nl)

#Finished Data
mlb01=rbind(al01,nl01)

1572/(1572+1506)
