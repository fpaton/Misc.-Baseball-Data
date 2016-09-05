


sea.p <- al.w.df[al.w.df$Tm=="SEA", ]
sea.p$W.L. <- as.numeric(sea.p$W.L.)
sea.p$pythW.L. <- as.numeric(sea.p$pythW.L.)



wlplot <- ggplot(sea.p, aes(Date))+  geom_line(aes(y = W.L., colour = "W.L.")) + 
  geom_line(aes(y = pythW.L., colour = "pythW.L."))+ theme(legend.title=element_blank())+
  geom_hline(yintercept = .5)
wlplot

help("geom_line")
str(sea.p)
std
var(sea.p$W.L.)
2*sqrt(var(sea.p$W.L.))

sd(sea.p$W.L.)

tex.p <- al.w.df[al.w.df$Tm=="SEA", ]
date1= as.Date("2016-04-15")
tex.p <- tex.p[tex.p$Date>=date1, ]

tex.p$W.L. <- as.numeric(tex.p$W.L.)
tex.p$pythW.L. <- as.numeric(tex.p$pythW.L.)

wlplot.t <- ggplot(tex.p, aes(Date))+  geom_line(aes(y = W.L., colour = "W.L.")) + 
  geom_line(aes(y = pythW.L., colour = "pythW.L."))+ theme(legend.title=element_blank())+
  geom_hline(yintercept = .5)+ scale_y_continuous(limits=c(.3,.7))
wlplot.t





