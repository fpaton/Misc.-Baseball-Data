library(XML)
library(dplyr)
library(ggplot2)
library(scales)

dat <- readHTMLTable("http://www.baseball-reference.com/games/standings.cgi?year=2016&month=08&day=14&submit=Submit+Date")

### plots

AL<-dat[8]
NL<-dat[9]
NL <- as.data.frame(NL)
AL <- as.data.frame(AL)

df <- rbind(AL,NL)

df$W.L. <- as.numeric(levels(df$W.L.)) [df$W.L.]
df$pythW.L. <- as.numeric(levels(df$pythW.L.)) [df$pythW.L.]

ggplot(data=df, aes(x=W.L., y=pythW.L.), label=Tm)+geom_point()+
            geom_text(aes(label=Tm),hjust=0, vjust=0)+ geom_abline(slope=1, intercept=0)+
            scale_x_continuous(breaks=pretty_breaks(n=10))+ scale_y_continuous(breaks=pretty_breaks(n=10))+
            xlab("Win/Loss Percentage")+ylab("Pythagoreon Expectation")

ggplot(data=df, aes(x=W, y=L))+geom_abline(slope=1,intercept=0)+geom_point()
