#2015 Data

date_scrape <- function(y,m,d) {
  url <- paste0("http://www.baseball-reference.com/games/standings.cgi?year=",y,"&month=",m, "&day=",d,"&submit=Submit+Date")
  d <- readHTMLTable(url, stringsAsFactors = FALSE)
  d <- as.data.frame(d[8])
  d
}

Today="2015/10/04" #enter todays date

# create a complete sequence of dates you want to scrape data for
dates <- as.data.frame(seq(as.Date("2015/04/03"), as.Date(Today), by = "days"))
names(dates) <- "dates" 

# split the dates so that there are three separate inputs to feed the function
dates <- colsplit(dates$dates, "-", c("y", "m", "d"))

# use the do() function to iterate the scrape function over all the dates

out_2015 <- dates %>% group_by(y,m,d) %>% do(date_scrape(.$y, .$m, .$d))

ALDF_2015=out_2015

ALDF_2015$Date=paste(ALDF_2015$y,ALDF$m,ALDF$d, sep="/")
ALDF_2015$Date=as.Date(ALDF_2015$Date, "%Y/%m/%d")
head(ALDF_2015)


ALdelta_2015=ALDF_2015
ALdelta_2015=ALdelta[45:2775,]
ALdelta_2015$wmp=as.numeric(ALdelta_2015$W.L.)-as.numeric(ALdelta_2015$pythW.L.)
ALdelta_2015$W.L.=as.numeric(ALdelta_2015$W.L.)
ggplot(data=ALdelta_2015, aes(x=Date, y=wmp))+geom_point(size=.001)+geom_line(aes(color=Tm))

ind_2015=ALdelta_2015[which(ALdelta_2015$Tm=='TOR'),]
ggplot(data=ind_2015, aes(x=Date, y=wmp))+geom_line(aes(color=Tm))+geom_point()+geom_line(data=ind_2015, )

plot1=ggplot(data=ind_2015, aes(x=Date, y=W.L.), group=Tm)+geom_point()+geom_line(aes(color=Tm))
plot2=ggplot(data=ind_2015, aes(x=Date, y=wmp), group=Tm)+geom_point()+geom_line(aes(color=Tm))
plot1
print(plot2)
