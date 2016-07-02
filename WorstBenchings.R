setwd("C:/Users/Charles.Willwerth/OneDrive for Business/GitHubChuckWillwerth/ESPNFFLScraper")
allLines = read.csv("allLines.csv")
teams = read.csv("fteams.csv")
library(sqldf)
nTeams = max(allLines$team, na.rm = TRUE)
nWeeks = max(allLines$week, na.rm = TRUE)
benchings = subset(allLines, allLines$Pos == "BN" | allLines$Pos == "IR")
benchings$AltPlayer = ""
benchings$AltPos = ""
benchings$AltPts = 0
benchings$Diff = 0


for (k in 1:nrow(benchings))
{
  if (benchings[k,]$NFLPos == "RB" | benchings[k,]$NFLPos == "WR" | benchings[k,]$NFLPos == "TE")
  {
    altVal = min(subset(allLines, 
        allLines$week == benchings[k,]$week & 
        allLines$team == benchings[k,]$team & 
          (allLines$Pos == "FLEX" | as.character(allLines$Pos) == as.character(benchings[k,]$NFLPos))
        )$Pts)
    if (benchings[k,]$Pts > altVal)
    {
      benchings[k,]$AltPts = altVal
      benchings[k,]$Diff = benchings[k,]$Pts - altVal
      benchings[k,]$AltPlayer = 
        as.character(subset(allLines,
               allLines$week == benchings[k,]$week & 
               allLines$team == benchings[k,]$team &
               allLines$Pts == altVal & 
                 (allLines$Pos == "FLEX" | as.character(allLines$Pos) == as.character(benchings[k,]$NFLPos))
        )[1,]$Player)
      benchings[k,]$AltPos = 
        as.character(subset(allLines,
               allLines$week == benchings[k,]$week & 
                 allLines$team == benchings[k,]$team &
                 allLines$Pts == altVal & 
                 (allLines$Pos == "FLEX" | as.character(allLines$Pos) == as.character(benchings[k,]$NFLPos))
        )[1,]$Pos)
    }
  }
  else
  {
    altVal = min(subset(allLines, 
                        allLines$week == benchings[k,]$week & 
                          allLines$team == benchings[k,]$team & 
                          (as.character(allLines$Pos) == as.character(benchings[k,]$NFLPos))
    )$Pts)
    if (benchings[k,]$Pts > altVal)
    {
      benchings[k,]$AltPts = altVal
      benchings[k,]$Diff = benchings[k,]$Pts - altVal
      benchings[k,]$AltPlayer = 
        as.character(subset(allLines,
                            allLines$week == benchings[k,]$week & 
                              allLines$team == benchings[k,]$team &
                              allLines$Pts == altVal & 
                              (allLines$Pos == "FLEX" | as.character(allLines$Pos) == as.character(benchings[k,]$NFLPos))
        )[1,]$Player)
      benchings[k,]$AltPos = 
        as.character(subset(allLines,
                            allLines$week == benchings[k,]$week & 
                              allLines$team == benchings[k,]$team &
                              allLines$Pts == altVal & 
                              (allLines$Pos == "FLEX" | as.character(allLines$Pos) == as.character(benchings[k,]$NFLPos))
        )[1,]$Pos)
    }
  }
    
}

badBenchings = subset(benchings, benchings$Diff > 0)
badBenchings$teamName = teams[match(badBenchings$team, teams$id),]$team
write.csv(badBenchings[order(-badBenchings$Diff),],"badBenchings2014.csv")
