QB = sqldf("SELECT team, Player, count(*) Num FROM allLines WHERE Pos = 'QB' GROUP BY team, Player ORDER BY team, Num DESC")
QBComp = sqldf("SELECT QB.* FROM QB JOIN (SELECT team, max(Num) Num FROM QB GROUP BY team) maxes ON ((QB.team == maxes.team) & (QB.Num == maxes.Num))")
QBSummary = sqldf("Select team, Num, group_concat(Player) FROM QBComp GROUP BY team")
QBTotals = sqldf("SELECT team, Sum(Pts) Pts FROM allLines WHERE Pos == 'QB' GROUP BY team")
QBSummary = sqldf("SELECT QBSummary.*, Pts FROM QBSummary JOIN QBTotals ON QBSummary.team == QBTotals.team ORDER BY Pts DESC")

RB = sqldf("SELECT team, Player, count(*) Num FROM allLines WHERE Pos = 'RB' GROUP BY team, Player ORDER BY team, Num DESC")
RBComp = sqldf("SELECT RB.* FROM RB JOIN (SELECT team, max(Num) Num FROM RB GROUP BY team) maxes ON ((RB.team == maxes.team) & (RB.Num == maxes.Num))")
RBSummary = sqldf("Select team, Num, group_concat(Player) FROM RBComp GROUP BY team")
RBTotals = sqldf("SELECT team, Sum(Pts) Pts FROM allLines WHERE Pos == 'RB' GROUP BY team")
RBSummary = sqldf("SELECT RBSummary.*, Pts FROM RBSummary JOIN RBTotals ON RBSummary.team == RBTotals.team ORDER BY Pts DESC")

WR = sqldf("SELECT team, Player, count(*) Num FROM allLines WHERE Pos = 'WR' GROUP BY team, Player ORDER BY team, Num DESC")
WRComp = sqldf("SELECT WR.* FROM WR JOIN (SELECT team, max(Num) Num FROM WR GROUP BY team) maxes ON ((WR.team == maxes.team) & (WR.Num == maxes.Num))")
WRSummary = sqldf("Select team, Num, group_concat(Player) FROM WRComp GROUP BY team")
WRTotals = sqldf("SELECT team, Sum(Pts) Pts FROM allLines WHERE Pos == 'WR' GROUP BY team")
WRSummary = sqldf("SELECT WRSummary.*, Pts FROM WRSummary JOIN WRTotals ON WRSummary.team == WRTotals.team ORDER BY Pts DESC")

FLEX = sqldf("SELECT team, Player, count(*) Num FROM allLines WHERE Pos = 'FLEX' GROUP BY team, Player ORDER BY team, Num DESC")
FLEXComp = sqldf("SELECT FLEX.* FROM FLEX JOIN (SELECT team, max(Num) Num FROM FLEX GROUP BY team) maxes ON ((FLEX.team == maxes.team) & (FLEX.Num == maxes.Num))")
FLEXSummary = sqldf("Select team, Num, group_concat(Player) FROM FLEXComp GROUP BY team")
FLEXTotals = sqldf("SELECT team, Sum(Pts) Pts FROM allLines WHERE Pos == 'FLEX' GROUP BY team")
FLEXSummary = sqldf("SELECT FLEXSummary.*, Pts FROM FLEXSummary JOIN FLEXTotals ON FLEXSummary.team == FLEXTotals.team ORDER BY Pts DESC")

TE = sqldf("SELECT team, Player, count(*) Num FROM allLines WHERE Pos = 'TE' GROUP BY team, Player ORDER BY team, Num DESC")
TEComp = sqldf("SELECT TE.* FROM TE JOIN (SELECT team, max(Num) Num FROM TE GROUP BY team) maxes ON ((TE.team == maxes.team) & (TE.Num == maxes.Num))")
TESummary = sqldf("Select team, Num, group_concat(Player) FROM TEComp GROUP BY team")
TETotals = sqldf("SELECT team, Sum(Pts) Pts FROM allLines WHERE Pos == 'TE' GROUP BY team")
TESummary = sqldf("SELECT TESummary.*, Pts FROM TESummary JOIN TETotals ON TESummary.team == TETotals.team ORDER BY Pts DESC")

K = sqldf("SELECT team, Player, count(*) Num FROM allLines WHERE Pos = 'K' GROUP BY team, Player ORDER BY team, Num DESC")
KComp = sqldf("SELECT K.* FROM K JOIN (SELECT team, max(Num) Num FROM K GROUP BY team) maxes ON ((K.team == maxes.team) & (K.Num == maxes.Num))")
KSummary = sqldf("Select team, Num, group_concat(Player) FROM KComp GROUP BY team")
KTotals = sqldf("SELECT team, Sum(Pts) Pts FROM allLines WHERE Pos == 'K' GROUP BY team")
KSummary = sqldf("SELECT KSummary.*, Pts FROM KSummary JOIN KTotals ON KSummary.team == KTotals.team ORDER BY Pts DESC")

DST = sqldf("SELECT team, Player, count(*) Num FROM allLines WHERE Pos = 'D/ST' GROUP BY team, Player ORDER BY team, Num DESC")
DSTComp = sqldf("SELECT DST.* FROM DST JOIN (SELECT team, max(Num) Num FROM DST GROUP BY team) maxes ON ((DST.team == maxes.team) & (DST.Num == maxes.Num))")
DSTSummary = sqldf("Select team, Num, group_concat(Player) FROM DSTComp GROUP BY team")
DSTTotals = sqldf("SELECT team, Sum(Pts) Pts FROM allLines WHERE Pos == 'D/ST' GROUP BY team")
DSTSummary = sqldf("SELECT DSTSummary.*, Pts FROM DSTSummary JOIN DSTTotals ON DSTSummary.team == DSTTotals.team ORDER BY Pts DESC")
library(stringr)
ggplot(data=KSummary, aes(x=reorder(team, -Pts), y=Pts, xlab="NULL",fill=Pts)) + 
  geom_bar(stat="identity")+ 
  scale_fill_gradient(low="yellow", high="red") + 
  theme(legend.position="none")+ 
  theme(axis.text.x = element_text(angle = 270, hjust = 0, size=16, vjust=.5)) + 
  theme(axis.text.y = element_text(size=16)) +
  xlab(NULL) + 
  ggtitle("Total points at Kicker") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

