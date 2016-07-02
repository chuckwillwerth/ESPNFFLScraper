waitFor <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

library(RSelenium)
loginURL <- "http://games.espn.go.com/ffl/signin"
user <- 'chuckprotrade@gmail.com'
pass <- 'Z00mZ00m'
nteams = 12
nweeks = 14
year = 2015
setwd("C:/Users/Charles.Willwerth/OneDrive for Business/GitHubChuckWillwerth/ESPNFFLScraper")
#setwd("C:/Users/ChuckAndCatherine/OneDrive for Business/GitHubChuckWillwerth/ESPNFFLScraper")
nfltms = read.csv("NFLTeams.csv",header = TRUE)
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
teams = read.csv(file="fteams.csv")
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
allLines = data.frame(week = integer(), team = integer(), Pos = character(), Player = character(), Pts = character(), Bye = integer());

if (startFresh == TRUE){
  startAt = 1;
} else {
  prevAllLines = read.csv("allLines.csv")
  startAt = max(prevAllLines$week, na.rm = TRUE) + 1;
}
for (i in 1:nteams){
  for(j in startAt:nweeks)
  {
    dataURL <- paste0("http://games.espn.go.com/ffl/boxscorefull?leagueId=545778&teamId=",i,"&scoringPeriodId=",j,"&seasonId=",year,"&view=scoringperiod&version=full")
    remDr$navigate(dataURL)
    # YOU can get the page source for example 
    pageSrc <- remDr$getPageSource()[[1]]
    tables = readHTMLTable(pageSrc)
    myscore = tables[[2]]
    kscore = tables[[3]]
    dscore = tables[[4]]
    bench = tables[[5]]
    ptsCol = 24
    if (year < 2015)
    {
	ptsCol = 22
    }
    kPtsCol = ptsCol - 12
    dPtsCol = ptsCol - 10
    QB = data.frame(myscore[4,1],myscore[4,2], myscore[4,ptsCol])
    RB1 = data.frame(myscore[5,1],myscore[5,2], myscore[5,ptsCol])
    RB2 = data.frame(myscore[6,1],myscore[6,2], myscore[6,ptsCol])
    WR1 = data.frame(myscore[7,1],myscore[7,2], myscore[7,ptsCol])
    WR2 = data.frame(myscore[8,1],myscore[8,2], myscore[8,ptsCol])
    TE  = data.frame(myscore[9,1],myscore[9,2], myscore[9,ptsCol])
    FLEX  = data.frame(myscore[10,1],myscore[10,2], myscore[10,ptsCol])
    K = data.frame(kscore[3,1],kscore[3,2],kscore[3,kPtsCol])
    DEF = data.frame(dscore[3,1], gsub("\\sD/ST\\sD/ST","",dscore[3,2]),dscore[3,dPtsCol])
    IR = subset(bench, bench$V1 == "IR")
    bench=subset(bench, bench$V1 == "Bench")
    BN = data.frame(Pos = character(), Player = character(), Pts = character(), Bye = integer(), stringsAsFactors = FALSE)
    if (year > 2014)
    {
      for (k in 1:nrow(bench))
      {
          BN = rbind(BN,data.frame(Pos = "BN",Player = as.character(bench[k,2]),Pts = as.character(bench[k,ptsCol]), stringsAsFactors = FALSE))
      }
      if (tables[[6]][1,1] == "BENCH: KICKERS")
      {
        KBench = tables[[6]]
        KBench = subset(KBench, KBench$V1 == "Bench")
        if (nrow(KBench) > 0)
        {
          for (k in 1:nrow(KBench))
          {
            BN = rbind(BN, data.frame(Pos = "BN", Player = KBench[k,2], Pts = ifelse(is.na(KBench[k,kPtsCol]),0,as.character(KBench[k,kPtsCol])), stringsAsFactors = FALSE))
          }
        }
        DefBench = tables[[7]]
        DefBench = subset(DefBench, DefBench$V1 == "Bench")
        if (nrow(DefBench) > 0)
        {
          for (k in 1:nrow(DefBench))
          {
            BN = rbind(BN,data.frame(Pos = "BN",Player = gsub("\\sD/ST\\sD/ST","",DefBench[k,2]), Pts = ifelse(is.na(DefBench[k,dPtsCol]),0,as.character(DefBench[k,dPtsCol]))))
          }
        }
      }else {
        KBench = NULL
        DefBench = tables[[6]]
        DefBench = subset(DefBench, DefBench$V1 == "Bench")
        if (nrow(DefBench) > 0)
        {
          for (k in 1:nrow(DefBench))
          {
            BN = rbind(BN,data.frame(Pos = "BN",Player = gsub("\\sD/ST\\sD/ST","",DefBench[k,2]), Pts = ifelse(is.na(DefBench[k,14]),0,DefBench[k,14])))
          }
        }
        
      }
    }
        
    if (!is.na(IR[1,2]))
    {
      IR = data.frame(Pos = "IR",Player = IR[1,2],Pts = IR[1,ptsCol - 1])
    } else {
      IR = NULL;
    }
    colnames(QB)= c("Pos","Player","Pts")
    colnames(RB1)= c("Pos","Player","Pts")
    colnames(RB2)= c("Pos","Player","Pts")
    colnames(WR1)= c("Pos","Player","Pts")
    colnames(WR2)= c("Pos","Player","Pts")
    colnames(TE)= c("Pos","Player","Pts")
    colnames(FLEX)= c("Pos","Player","Pts")
    colnames(K)=c("Pos","Player","Pts")
    colnames(DEF)= c("Pos","Player","Pts")
    #colnames(IR)= c("Pos","Player","Pts")
    
    lines = rbind(QB,RB1,RB2,WR1,WR2,TE,FLEX,K,DEF,BN,IR)
    lines$Player = gsub("([^a-zA-Z0-9\\s\\.,'/])"," ",lines$Player)
    lines$Pts = as.numeric(as.character(lines$Pts))
    lines$Bye = 0
    if (nrow(lines[is.na(lines$Pts),]) > 0)
    {
      lines[is.na(lines$Pts),]$Bye = 1
      lines[is.na(lines$Pts),]$Pts = 0
    }
    lines$week = j
    lines$team = i
    if (year == 2015)
    {
     if ((tables[[6]][1,1] == "BENCH: TEAM D/ST") | (tables[[6]][1,1] == "BENCH: KICKERS"))
     {
       lines$Opponent = gsub(" Box Score","",tables[[7]][1,1])
     }else {
      lines$Opponent = gsub(" Box Score","",tables[[6]][1,1])
     }
    }
    else
    {
	     if ((tables[[5]][1,1] == "BENCH: TEAM D/ST") | (tables[[5]][1,1] == "BENCH: KICKERS"))
     {
       lines$Opponent = gsub(" Box Score","",tables[[6]][1,1])
     }else {
      lines$Opponent = gsub(" Box Score","",tables[[5]][1,1])
     }
    }
    
    allLines = rbind(allLines, lines)
    
  }
}
db = allLines
allLines$Player = gsub("\\s*$","",allLines$Player)
allLines$Player = gsub("Ã,Â","",allLines$Player)
allLines$Player = gsub("Ã,Â","",allLines$Player)
allLines$OppId = match(allLines$Opponent,teams$team)
allLines$Player = gsub("\\sD/ST","",allLines$Player)
allLines$NFLTeam = gsub("(.*), *(.*)","\\2",allLines$Player)
allLines$Player = gsub("(.*), *(.*)","\\1",allLines$Player)
allLines$Player = gsub("\\*$","",allLines$Player)
allLines$Player = gsub("\\s*$","",allLines$Player)
allLines$NFLTeam = gsub("\\s*(([QPDO])|(SSPD))$","",allLines$NFLTeam)
allLines$NFLPos = gsub("\\s+IR$","",allLines$NFLTeam)
allLines$NFLPos = gsub("^(.*)\\s+(.*)$","\\2",allLines$NFLPos)
allLines$NFLTeam = gsub("^(.*)\\s+(.*)$","\\1",allLines$NFLTeam)
allLines$NFLTeam = gsub("\\s*$","",allLines$NFLTeam)
allLines[!is.na(match(allLines$NFLTeam, nfltms$TeamName)),]$NFLPos = "D/ST"
allLines[!is.na(match(allLines$NFLTeam, nfltms$TeamName)),]$NFLTeam = as.character(nfltms[match(allLines[!is.na(match(allLines$NFLTeam,nfltms$TeamName)),]$NFLTeam,nfltms$TeamName),]$ID)
if (startFresh == FALSE)
{
  prevAllLines$X = NULL
  allLines = rbind(prevAllLines, allLines)
}

#allLines$Opponent = NULL
write.csv(allLines, file="allLines.csv")
# now operate on pageSrc using for example library(XML) etc
# readHTMLTable(pageSrc) # for example
remDr$close()
remDr$closeServer()