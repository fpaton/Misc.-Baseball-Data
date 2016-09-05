# Scrape Daily Games:
library(stringr)
library(gsubfn)
url1="http://projects.fivethirtyeight.com/2016-mlb-predictions/blue-jays/"

url2=read_html("http://projects.fivethirtyeight.com/2016-mlb-predictions/blue-jays/")

five38=html_node(url2, ".games")

data<-url2

list1= data %>%
  html_node("#played") %>% 
  html_text()

#word(list1, sep=fixed("Sun"))

vector1=str_split(string1, " ") #splits list into seperate characters

vector1=as.data.frame(vector1) #list into data frame
names(vector1)<-"column1"

#replace white sox/ red sox to white_sox and red_sox

str(vector1)

###### XXXXX

string1=vector1[1,1]
string1=lapply(list1, as.character)

sox_jays<-function(x,replace_team,sox_1){

    sox1=paste(sox_1," ", sep="")
    team.1=paste(team_replace,sox1,sep="")
    team.2=paste(team_replace,sox_1,"_", sep="")
    x=gsub(team.1,team.2,string1)
    return(x)
}

for(team_loop in c("Jays", "Red", "White")){
string1=sox_jays(string1, "Jays",team_loop)
}

#############################

score1=vector1[seq(2,nrow(vector1),5),1]
month1=vector1[seq(4,nrow(vector1),5),1]
day=  vector1[seq(3,nrow(vector1),5),1]

#####dealing with split team

team_p=vector1[seq(6,nrow(vector1),5),1]
team_p=lapply(team_p, as.character)
team_p=gsub("Jays","Jays ",team_p)
str(team_p)
team_p=gsub("@","@ ",team_p)
team_p=gsub("vs.","vs. ",team_p)
team_p=gsub("%", "% ", team_p)

team_p=str_split(team_p, " ")
team_p=lapply(team_p, as.character)

team_p_1=strapply(team_p, "^[[:lower:]]+|[[:upper:]][[:lower:]]*", c)[1:length(team_p)]
team_p_1=lapply(team_p_1, as.data.frame)
str(team_p_1)

