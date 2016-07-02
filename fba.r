waitFor <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

library(RSelenium)
loginURL <- "http://games.espn.go.com/fba/signin"
user <- 'chuckprotrade@gmail.com'
pass <- 'Z00mZ00m'
nteams = 10
nweeks = 11
year = 2016
#setwd("C:/Users/Charles.Willwerth/OneDrive for Business/GitHubChuckWillwerth/ESPNFFLScraper")
#setwd("C:/Users/ChuckAndCatherine/OneDrive for Business/GitHubChuckWillwerth/ESPNFFLScraper")
#nfltms = read.csv("NFLTeams.csv",header = TRUE)
#teams = data.frame(id = integer(), team = character(), owner = character())
#teams = rbind(teams,data.frame(id = 1,team = "Mike Ditka in a Box", owner = "Don Pease"))
#teams = rbind(teams,data.frame(id = 2,team = "Memein Freeman", owner = "John Bagley"))
#teams = rbind(teams,data.frame(id = 3,team = "Coffin Corner Gunners", owner = "Chuck Willwerth"))
#teams = rbind(teams,data.frame(id = 4,team = "Quad Box", owner = "Derek Prior"))
#teams = rbind(teams,data.frame(id = 5,team = "Wilfork Island", owner = "Damien Coffey"))
#teams = rbind(teams,data.frame(id = 6,team = "Boston Bobcatz", owner = "Chris Hehn"))
#teams = rbind(teams,data.frame(id = 7,team = "Huh, huh.... He said UpChuck", owner = "James Willwerth"))
#teams = rbind(teams,data.frame(id = 8,team = "Mr. Ambree Hinano!", owner = "Joel Fanjoy"))
#teams = rbind(teams,data.frame(id = 9,team = "Ya Down With PCP?", owner = "Mike Bergan"))
#teams = rbind(teams,data.frame(id = 10,team = "Marcy Project Freestylers", owner = "James Zozula"))
#teams = rbind(teams,data.frame(id = 11,team = "Team Gould", owner = "Zack Gould"))
#teams = rbind(teams,data.frame(id = 12,team = "$1 All Stars", owner = "Pat Quillen"))
#write.csv(teams, file="fteams2013.csv")
#teams = read.csv(file="fteams.csv")
RSelenium::checkForServer()
RSelenium::startServer()
remDr <- remoteDriver()
waitFor(3)
remDr$open()
remDr$navigate(loginURL)
remDr$switchToFrame("disneyid-iframe")
in1 = remDr$findElement("xpath","//input[@placeholder = 'Username or Email Address']")
in1$sendKeysToElement(list(user))
in2 = remDr$findElement("xpath","//input[@placeholder = 'Password (case sensitive)']")
in2$sendKeysToElement(list(pass))
btn = remDr$findElement("xpath","//button[@type = 'submit']")
btn$clickElement()

startFresh = FALSE
allLines = data.frame(week = integer(), team = integer(), Games = character(), Points = character());

#if (startFresh == TRUE){
  #startAt = 1;
#} else {
  #prevAllLines = read.csv("allLines.csv")
  #startAt = max(prevAllLines$week, na.rm = TRUE) + 1;
#}

for (i in 1:nteams){
  for(j in 1:nweeks)
  {
    dataURL <- paste0("http://games.espn.go.com/fba/boxscorefull?leagueId=179297&teamId=",i,"&scoringPeriodId=",(j-1)*7+6,"&seasonId=",year,"&view=matchup&version=full")
    remDr$navigate(dataURL)
    # YOU can get the page source for example 
    
    pageSrc <- remDr$getPageSource()[[1]]
    tables = readHTMLTable(pageSrc)
    line = data.frame(i, j, tables[[3]][nrow(tables[[3]]),][4], tables[[3]][nrow(tables[[3]]),][21])
    colnames(line) = c("Team", "Week", "Games","Points")
    allLines = rbind(allLines, line)
    colnames(allLines) = c("Team", "Week", "Games","Points")
  }
}
db = allLines


#allLines$Opponent = NULL
write.csv(allLines, file="FBAllLines.csv")
# now operate on pageSrc using for example library(XML) etc
# readHTMLTable(pageSrc) # for example
remDr$close()
remDr$closeServer()