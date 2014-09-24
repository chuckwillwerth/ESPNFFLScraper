waitFor <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

library(RSelenium)
loginURL <- "http://games.espn.go.com/ffl/signin"
user <- 'doubledown'
pass <- 'starranger'
nteams = 12
nweeks = 3
setwd("C:/chuck/github/espnffl")
nfltms = read.csv("NFLTeams.csv",header = TRUE)
teams = data.frame(id = integer(), team = character())
teams = rbind(teams,data.frame(id = 1,team = "The Peasemakers"))
teams = rbind(teams,data.frame(id = 2,team = "Stuck in Gears"))
teams = rbind(teams,data.frame(id = 3,team = "Coffin Corner Gunners"))
teams = rbind(teams,data.frame(id = 4,team = "Quad Box"))
teams = rbind(teams,data.frame(id = 5,team = "Wilfork Island"))
teams = rbind(teams,data.frame(id = 6,team = "Boston Bobcatz"))
teams = rbind(teams,data.frame(id = 7,team = "Now I'm Selling Cars N I know Y"))
teams = rbind(teams,data.frame(id = 8,team = "Mr. Ambree Hinano!"))
teams = rbind(teams,data.frame(id = 9,team = "Love in an Elevator"))
teams = rbind(teams,data.frame(id = 10,team = "Marcy Project Freestylers"))
teams = rbind(teams,data.frame(id = 11,team = "Team To Be Named Later"))
teams = rbind(teams,data.frame(id = 12,team = "Tom Terrific"))
write.csv(teams, file="fteams.csv")
RSelenium::checkForServer()
RSelenium::startServer()
remDr <- remoteDriver()
waitFor(10)
remDr$open()
remDr$navigate(loginURL)
webElem <- remDr$findElement('name', 'username')
webElem$sendKeysToElement(list(user))
webElem <- remDr$findElement('name', 'password')
webElem$sendKeysToElement(list(pass))
remDr$findElement('name', 'submit')$clickElement()
allLines = data.frame(week = integer(), team = integer(), Pos = character(), Player = character(), Pts = character())
for (i in 1:nteams){
  for(j in 1:nweeks)
  {
    dataURL <- paste0("http://games.espn.go.com/ffl/boxscorefull?leagueId=545778&teamId=",i,"&scoringPeriodId=",j,"&seasonId=2014&view=scoringperiod&version=full")
    remDr$navigate(dataURL)
    # YOU can get the page source for example 
    pageSrc <- remDr$getPageSource()[[1]]
    tables = readHTMLTable(pageSrc)
    myscore = tables[[2]]
    kscore = tables[[3]]
    dscore = tables[[4]]
    bench = tables[[5]]
    QB = data.frame(myscore[4,1],myscore[4,2], myscore[4,23])
    RB1 = data.frame(myscore[5,1],myscore[5,2], myscore[5,23])
    RB2 = data.frame(myscore[6,1],myscore[6,2], myscore[6,23])
    WR1 = data.frame(myscore[7,1],myscore[7,2], myscore[7,23])
    WR2 = data.frame(myscore[8,1],myscore[8,2], myscore[8,23])
    TE  = data.frame(myscore[9,1],myscore[9,2], myscore[9,23])
    FLEX  = data.frame(myscore[10,1],myscore[10,2], myscore[10,23])
    K = data.frame(kscore[3,1],kscore[3,2],kscore[3,12])
    DEF = data.frame(dscore[3,1], gsub("\\sD/ST\\sD/ST","",dscore[3,2]),dscore[3,14])
    IR = subset(bench, bench$V1 == "IR")
    bench=subset(bench, bench$V1 == "Bench")
    
    BN = data.frame(Pos = character(), Player = character(), Pts = character())
    for (k in 1:nrow(bench))
    {
      BN = rbind(BN,data.frame(Pos = "BN",Player = bench[k,2],Pts = bench[k,23]))
    }
    DefBench = tables[[6]]
    DefBench = subset(DefBench, DefBench$V1 == "Bench")
    if (nrow(DefBench) > 0)
    {
      for (k in 1:nrow(DefBench))
      {
        BN = rbind(BN,data.frame(Pos = "BN",Player = gsub("\\sD/ST\\sD/ST","",DefBench[k,2]), Pts = DefBench[k,14]))
      }
    }
    
    if (!is.na(IR[1,2]))
    {
      IR = data.frame(Pos = "IR",Player = IR[1,2],Pts = IR[1,23])
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
    lines$Player = gsub("Ã,Â","",lines$Player)
    lines$Pts = as.numeric(as.character(lines$Pts))
    lines$week = j
    lines$team = i
    if (tables[[6]][1,1] == "BENCH: TEAM D/ST")
    {
      lines$Opponent = gsub(" Box Score","",tables[[7]][1,1])
    }else {
     lines$Opponent = gsub(" Box Score","",tables[[6]][1,1])
    }
    
    allLines = rbind(allLines, lines)
    
  }
}
allLines$Player = gsub("Ã,Â","",allLines$Player)
allLines$Player = gsub("Ã,Â","",allLines$Player)
allLines$OppId = match(allLines$Opponent,teams$team)
allLines$Player = gsub("\\sD/ST","",allLines$Player)
allLines$NFLTeam = gsub("(.*), (.*)","\\2",allLines$Player)
allLines$Player = gsub("(.*), (.*)","\\1",allLines$Player)
allLines$Player = gsub("\\*$","",allLines$Player)
allLines$NFLTeam = gsub("\\s*(([QPDO])|(SSPD))$","",allLines$NFLTeam)
allLines$NFLPos = gsub("^(.*)\\s(.*)$","\\2",allLines$NFLTeam)
allLines$NFLTeam = gsub("^(.*)\\s(.*)$","\\1",allLines$NFLTeam)
allLines[!is.na(match(allLines$NFLTeam, nfltms$TeamName)),]$NFLPos = "D/ST"
allLines[!is.na(match(allLines$NFLTeam, nfltms$TeamName)),]$NFLTeam = as.character(nfltms[match(allLines[!is.na(match(allLines$NFLTeam,nfltms$TeamName)),]$NFLTeam,nfltms$TeamName),]$ID)
allLines$Opponent = NULL
write.csv(allLines, file="allLines.csv")
# now operate on pageSrc using for example library(XML) etc
# readHTMLTable(pageSrc) # for example
remDr$close()
remDr$closeServer()