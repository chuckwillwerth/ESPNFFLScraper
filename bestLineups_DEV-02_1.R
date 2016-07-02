setwd("C:/Users/Charles.Willwerth/OneDrive for Business/GitHubChuckWillwerth/ESPNFFLScraper")
allLines = read.csv("allLines.csv")
teams = read.csv("fteams.csv")
library(sqldf)
nTeams = max(allLines$team, na.rm = TRUE)
nWeeks = max(allLines$week, na.rm = TRUE)
bestScores = data.frame(team = integer(), week = integer(), QB = integer(), RB1 = integer(), RB2 = integer(), WR1 = integer(), WR2 = integer(), TE = integer(), K = integer(), DEF = integer(), FLEX = integer(), TOTAL = integer())
weekScores = data.frame(team = integer(), week = integer(), opponent = integer(), Pts = integer(), OppPts = integer(), ifW = integer(), ifL = integer(), ifT = integer())
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
weekScores$ifW = 0
weekScores$ifL = 0
weekScores$ifT = 0
weekScores$pwrW = 0
weekScores$pwrL = 0
weekScores$pwrT = 0
weekScores$Str = 0
for (i in 1:nTeams)
{
  for (j in 1:nWeeks)
  {
    otherScores = subset(weekScores, weekScores$team != i & weekScores$week == j)
    allScores = subset(weekScores, weekScores$week == j)
    mymed = median(allScores$Pts)
    mystddev = sd(allScores$Pts)
    
    myscore = subset(weekScores, team == i & week == j)$Pts
    myStr = (myscore - mymed) / mystddev
    ifW = nrow(subset(otherScores, otherScores$Pts < myscore))
    ifL = nrow(subset(otherScores, otherScores$Pts > myscore))
    ifT = nrow(subset(otherScores, otherScores$Pts == myscore))
    weekScores[weekScores$team == i & weekScores$week == j,][1,]$ifW = ifW
    weekScores[weekScores$team == i & weekScores$week == j,][1,]$ifL = ifL
    weekScores[weekScores$team == i & weekScores$week == j,][1,]$ifT = ifT
    weight = 1 + 3 * (j - 1) / (nWeeks - 1)
    pwrW = weight * ifW
    pwrL = weight * ifL
    pwrT = weight * ifT
    weekScores[weekScores$team == i & weekScores$week == j,][1,]$pwrW = pwrW
    weekScores[weekScores$team == i & weekScores$week == j,][1,]$pwrL = pwrL
    weekScores[weekScores$team == i & weekScores$week == j,][1,]$pwrT = pwrT
    weekScores[weekScores$team == i & weekScores$week == j,][1,]$Str = weight * myStr
  }
}
weekScores$W = 0
weekScores[weekScores$Pts > weekScores$OppPts,]$W = 1
weekScores$L = 0
weekScores[weekScores$Pts < weekScores$OppPts,]$L = 1
weekScores$T = 0
weekScores[weekScores$Pts == weekScores$OppPts,]$T = 1
weekScores$BSW = 0
weekScores[weekScores$BestScore > weekScores$OppBestScore,]$BSW = 1
weekScores$BSL = 0
weekScores[weekScores$BestScore < weekScores$OppBestScore,]$BSL = 1
weekScores$BST = 0
weekScores[weekScores$BestScore == weekScores$OppBestScore,]$BST = 1
weekcalc = aggregate(x = weekScores, list(weekScores$team),sum)
weekcalc$team = weekcalc$team / nWeeks
weekcalc$PotentialCaptured = weekcalc$Pts/weekcalc$BestScore

ranks = sqldf("SELECT team, sum(Pts) Pts, sum(W) Wins, sum(L) Losses, sum(T) Ties, sum(BSW) BSW, sum(BSL) BSL, sum(BST)BST, sum(BestScore) Best FROM weekScores GROUP BY team")
ranks$score = ranks$Pts * (ranks$Wins / (ranks$Wins + ranks$Losses)) + ranks$Pts
ranks$teamName = teams[match(ranks$team, teams$id),]$team
r = ranks[,c(11,10,3,4,5,6,7,8,9,2,1)]
r[order(-r$score),]
plot(weekcalc$Best ~ weekcalc$Pts)
text(weekcalc$Pts+2, weekcalc$Best, weekcalc$team)
lines(c(min(r$Pts),max(r$Pts)),c(min(r$Best),max(r$Best)))
ifsAndButs = sqldf("SELECT team, sum(ifW) ifW, sum(ifL) ifL, sum(ifT) ifT, sum(pwrW) pwrW, sum(pwrL) pwrL, sum(pwrT) pwrT, sum(Str) Str FROM weekScores GROUP BY team")
ifsAndButs$team = teams[match(ifsAndButs$team, teams$id),]$team
ifsAndButs$Pct = (ifsAndButs$ifW  + (ifsAndButs$ifT / 2))/  (ifsAndButs$ifW +  ifsAndButs$ifL +  ifsAndButs$ifT)
ifsAndButs$PwrPct = (ifsAndButs$pwrW + (ifsAndButs$pwrT / 2))/ (ifsAndButs$pwrW + ifsAndButs$pwrL + ifsAndButs$pwrT)

ifsAndButs[order(-ifsAndButs$Str),]
write.csv(ifsAndButs[order(-ifsAndButs$PwrPct),],"rankings.csv")


