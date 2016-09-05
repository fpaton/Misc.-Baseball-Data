library(rvest)
library(gsubfn)
library(magrittr)

Rprof("profiling.out")

GetOdds=function(x){
# Pulls the most current predictions for MLB games from fivethirtyeight.com (538)
#  
#  Args:
#     x: Team name as a string.      
#  
#  Returns: 
#     538's prediction for the team entered
   
  # Set url
  data <- read_html(paste("http://projects.fivethirtyeight.com/2016-mlb-predictions/", x, sep= ""))

  # exctract win loss percent
  win.perc <- data %>%
    html_node(".teams") %>% 
    html_text()
  win.perc=gsub("@", "vs.", win.perc)  # win.perc pulls a string e.g. "42%vs.58%". 

  home.wp <- as.numeric(substr(win.perc, 1, 2))/ 100  # home.wp "home" as in the homepage team.
  away.wp <- as.numeric(substr(win.perc, 7, 8))/ 100  # away... seventh & eighth characters

  # extract date
  date.5 <- data %>% 
    html_node(".date") %>% 
    html_text()  # example date.5 <- "Tue Aug 16"
  
  # identify home or away marker
  away.or.home <- data %>%
    html_node(".middle") %>% 
    html_text()
  
  # dummy variable
  
  if(away.or.home== "vs."){
    home.away1 <- "Home"
    } else{ home.away1 <- 'Away'}
  
  if(home.away1== "Home"){
    home.away2 <- "Away"
    } else{ home.away2 <- "Home"}

  # extract team names
  team.names <- data %>%  # "BravesTwins"
    html_node(".team-names") %>% 
    html_text()

  # Reformat teams with two strings as names... White Sox, Red Sox, Blue Jays
  team.names <- team.names %>% gsub("Blue Jays", "Bluejays", .) %>% 
  gsub("White Sox", "Whitesox", .) %>%
  gsub("Red Sox", "Redsox", .)
  
  # Splits Team Names by CamelCase
  team.names <- strapply(team.names, "^[[:lower:]]+|[[:upper:]][[:lower:]]*", c)[1:length(team.names)]
  
  # Example of above code for team.names
  # team.names originally = "Blue JaysReds"
  # first pipe team.names= "BluejaysReds"
  # then splits into "Bluejays Reds" These are the two values used to populate the data frame
  
  # Temp DF to store values
  team.names <- as.data.frame(team.names[1], stringsAsFactors = FALSE)

  # populate the data frame that gets returned
  return.df <- data.frame(Team= "",Win= "",Date= "", Home.Away= "", 
                        EV.OverZero= "", EV.over.5p= "", EV.over.10= "", stringsAsFactors= FALSE)
  
  return.df[1, 1] <- team.names[1, 1]
  return.df[2, 1] <- team.names[2, 1]
  return.df[1, 2] <- home.wp
  return.df[2, 2] <- away.wp
  return.df[1:2, 3] <- date.5
  return.df[1, 4] <- home.away1
  return.df[2, 4] <- home.away2
  return.df[1, 5] <- round(as.numeric(1/home.wp), digits= 2)
  return.df[2, 5] <- round(as.numeric(1/away.wp), digits= 2)
  return.df[1, 6] <- round(as.numeric(1.05/home.wp), digits= 2)
  return.df[2, 6] <- round(as.numeric(1.05/away.wp), digits= 2)
  return.df[1, 7] <- round(as.numeric(1.1/home.wp), digits= 2)
  return.df[2, 7] <- round(as.numeric(1.1/away.wp), digits= 2)
  
  return(return.df)
}  # END of Function
# END


kNl.teams <- c("braves","marlins","mets","phillies","nationals","cubs","reds","astros",
                "brewers","pirates","cardinals","diamondbacks","rockies","dodgers","padres","giants")

kAl.teams <- c("orioles","red-sox","yankees","rays","blue-jays","white-sox","indians","tigers","royals",
                "twins","angels","athletics","mariners","rangers")

# lapply the list of teams through function GetOdds

# CPU/ connection speed test

ptm <- proc.time()
al.odds.test <- lapply(kAl.teams, GetOdds) %>%
           do.call("rbind", .) 
proc.time()-ptm

nl.odds <- lapply(kNl.teams, GetOdds) %>%
  do.call("rbind", .)

al.odds <- lapply(kAl.teams, GetOdds) %>%
  do.call("rbind", .)

final.df <- rbind(al.odds, nl.odds) 

final.df <- final.df[duplicated(final.df), ]

Rprof()
summaryRprof("profiling.out")
# Finished Data Frames

# Update data frame and save new days

todays.save <- final.df
yesterday.save <- readRDS(file= "gamelines.Rda")

new.df <- rbind(yesterday.save, todays.save)
saveRDS(final.df, file="gamelines.Rda")


