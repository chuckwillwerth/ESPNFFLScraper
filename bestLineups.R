setwd("c:/temp")
allLines = read.csv("allLines.csv")
teams = read.csv("fteams.csv")
nTeams = max(allLines$team, na.rm = TRUE)
nWeeks = max(allLines$week, na.rm = TRUE)
bestScores = data.frame(team = integer(), week = integer(), QB = integer(), RB1 = integer(), RB2 = integer(), WR1 = integer(), WR2 = integer(), TE = integer(), K = integer(), DEF = integer(), FLEX = integer(), TOTAL = integer())
weekScores = data.frame(team = integer(), week = integer(), opponent = integer(), Pts = integer(), OppPts = integer())
for (i in 1:nTeams)
{
  for (j in 1:nWeeks)
  {
    oppid = allLines[allLines$team == i & allLines$week == j,][1,]$OppId
    weekScores = rbind(weekScores, data.frame(team = i, week = j, opponent = allLines[allLines$team == i & allLines$week == j,][1,]$OppId,Pts = sum(allLines[allLines$team == i & allLines$week == j & allLines$Pos != "BN" & allLines$Pos != "IR",]$Pts, na.rm = TRUE),OppPts = sum(allLines[allLines$team == oppid & allLines$week == j & allLines$Pos != "BN" & allLines$Pos != "IR",]$Pts, na.rm = TRUE)))
  }
}
weekScores$BestScore = 0
weekScores$OppBestScore = 0
for (i in 1:nTeams)
{
  for (j in 1:nWeeks)
  {
    weekRoster = subset(allLines, allLines$team == i & allLines$week == j)
    QB = sort(subset(weekRoster, weekRoster$NFLPos == "QB")$Pts,decreasing = TRUE)[1]
    RB1 = sort(subset(weekRoster, weekRoster$NFLPos == "RB")$Pts,decreasing = TRUE)[1]
    RB2 = sort(subset(weekRoster, weekRoster$NFLPos == "RB")$Pts,decreasing = TRUE)[2]
    WR1 = sort(subset(weekRoster, weekRoster$NFLPos == "WR")$Pts,decreasing = TRUE)[1]
    WR2 = sort(subset(weekRoster, weekRoster$NFLPos == "WR")$Pts,decreasing = TRUE)[2]
    TE = sort(subset(weekRoster, weekRoster$NFLPos == "TE")$Pts,decreasing = TRUE)[1]
    K = sort(subset(weekRoster, weekRoster$NFLPos == "K")$Pts,decreasing = TRUE)[1]
    DEF = sort(subset(weekRoster, weekRoster$NFLPos == "D/ST")$Pts,decreasing = TRUE)[1]
    FLEX = max(sort(subset(weekRoster, weekRoster$NFLPos == "WR")$Pts,decreasing = TRUE)[3],
               sort(subset(weekRoster, weekRoster$NFLPos == "RB")$Pts,decreasing = TRUE)[3],
               sort(subset(weekRoster, weekRoster$NFLPos == "TE")$Pts,decreasing = TRUE)[2], na.rm=TRUE)
    TOTAL = sum(QB, RB1, RB2, WR1, WR2, TE, K, DEF, FLEX)
    bestScore = cbind(team = i, week = j,QB, RB1, RB2, WR1, WR2, TE, K, DEF, FLEX,TOTAL)
    bestScores = rbind(bestScores, bestScore)
    weekScores[weekScores$opponent == i & weekScores$week == j,][1,]$OppBestScore = TOTAL
    weekScores[weekScores$team == i & weekScores$week == j,][1,]$BestScore = TOTAL
    
  }
}
weekcalc = aggregate(x = weekScores, list(weekScores$team),sum)
weekcalc$team = weekcalc$team / 2
weekcalc$PotentialCaptured = weekcalc$Pts/weekcalc$BestScore
plot(weekcalc$PotentialCaptured ~ weekcalc$Pts)