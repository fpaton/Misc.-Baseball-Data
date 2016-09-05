library(ggplot2)

plotdata <- al.w.df[al.w.df$Tm %in% c('TEX', 'OAK', 'SEA', 'HOU', 'LAA'), ]

al.w.df$GB[al.w.df$GB=="--"] <- 0
al.w.df$GB <- as.numeric(al.w.df$GB)



nlplot <- ggplot(al.w.df, aes(x=Date, y=GB, colour = factor(Tm), group = Tm))+
          geom_line(size = 2, alpha = .75)+ scale_y_reverse(breaks=seq(0,20,by=1), limits=c(20,0))+ scale_x_date()+
          scale_colour_manual(values = team_colors, name = "Team")+
          labs(title = "AL West Race 2016")+ geom_text(aes(label=ifelse(Date == "2016-08-14", as.character(GB),'')), 
                                            hjust=-1, show.legend = FALSE)+ 

  theme(legend.title = element_text(size = 12)) + theme(legend.text = element_text(size = 12)) + 
  
  theme(axis.text = element_text(size = 13, face = "bold"), 
        axis.title = element_text(size = 15, color = "grey10", face = "bold"), 
        plot.title = element_text(size = 25, face = "bold", vjust = 1))
       
        
  
nlplot2 <- nlplot +  theme(legend.background = element_rect(colour = "black")) + labs(y="Games Back")

team_colors = c("SEA" = "#005C5C", "HOU" = "#EB6E1F", "OAK" = "#EFB21E", "LAA" = "#BA0021", "TEX" = "#003278")



ff=al.df[al.df$Tm=="TEX", ]


nlplot

plotdata2$GB[321] <- 0

breaks = seq(0, 1, by=.1)
breaks=seq(1,10, lengthout=1)
help(ifelse)

aes(Date, GB, colour = factor(Tm), group = Tm))

head(plotdata2)


ggplot(data=df, aes(x=W.L., y=pythW.L.), label=Tm)+geom_point()+
  geom_text(aes(label=Tm),hjust=0, vjust=0)+ geom_abline(slope=1, intercept=0)+
  scale_x_continuous(breaks=pretty_breaks(n=10))+ scale_y_continuous(breaks=pretty_breaks(n=10))+
  xlab("Win/Loss Percentage")+ylab("Pythagoreon Expectation")
