#Plots

#American League
team="TOR"
df1=ALDF[which(ALDF$Tm==team),]
df1=df1[1:89,]
head(df1)

ggplot(data=df1, aes(x=Date, y=pythW.L., group=1))+geom_point()+geom_line(color="red")+
      geom_line(data=df1, aes(y=W.L., group=2))+geom_point(data=df1, aes(y=W.L., group=2))


#National League

NLDF$m=paste(0,NLDF$m, sep="")
NLDF$Date= NULL
NLDF$Date=paste(NLDF$y,NLDF$m,NLDF$d, sep="/")
NLDF$Date=as.Date(NLDF$Date)

head(NLDF)

team_nl="TOR"
nldf2=NLDF[which(NLDF$Tm==team_nl),]
nldf2=nldf2[1:89,]
head(nldf2)

ggplot(data=nldf2, aes(x=Date, y=pythW.L., group=1))+geom_point()+geom_line(color="blue")+
      geom_line(data=nldf2, aes(y=W.L., group=2))+geom_point(data=nldf2, aes(y=W.L., group=2))
