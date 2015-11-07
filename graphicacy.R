library(Lahman)
library(ggplot2)
library(ggthemes)
# Player ID's for Lahmans database
offensivePlayers <- c("ruthba01","cobbty01","bondsba01","mayswi01","aaronha01",
                      "willite01", "musiast01", "speaktr01","wagneho01","hornsro01")
offenseNames <- c("Babe Ruth", "Ty Cobb", "Barry Bonds", "Willie Mays", "Hank Aaron","Ted Williams","Stan Musial",
                  "Tris Speaker", "Honus Wagner", "Rogers Hornsby")
offensiveWar <- c(155.1,150.9,142.6,136.4,131.8,126.3,124.7,123.6,123,121.6)

pitchers <- c("youngcy01", "johnswa01", "clemero02","alexape01","nichoki01","grovele01","seaveto01","maddugr01","johnsra05","niekrph01")

pitcherNames <- c("Cy Young","Walter Johnson", "Roger Clemens","Pete Alexander","Kid Nichols","Lefty Grove","Tom Seaver", "Greg Maddux",
                  "Randy Johnson","Phil Niekro")

#data frame containing stats for only offesnive players
offenseDF <- Batting[Batting$playerID %in% offensivePlayers,]
offenseDF$Avg <- offenseDF$H/offenseDF$AB # create Batting Average column
offenseDF$Avg <- round(offenseDF$Avg,3) # Round to 3 digits

# data frame for pitchers
pitcherDF <- Pitching[Pitching$playerID %in% pitchers,]
pitcherDF$WHIP <- (pitcherDF$W+pitcherDF$H)/(pitcherDF$IPouts/3)
pitcherDF$WHIP <- round(pitcherDF$WHIP,3)

# Really annoying clean up for players who got traded  to different team in middle of season
# If left un changed there will be straight line up or down on that year. Needs to be fixed
pitcherDF[pitcherDF$playerID=="johnsra05"&pitcherDF$yearID==1998,]$BAOpp <- c(.224,.224)
pitcherDF[pitcherDF$playerID=="clemero02"&pitcherDF$yearID==1998,]$BAOpp <- .198
pitcherDF[pitcherDF$playerID=="maddugr01"&pitcherDF$yearID==1998,]$BAOpp <- .220
pitcherDF[pitcherDF$playerID=="maddugr01"&pitcherDF$yearID==1999,]$BAOpp <- .294
pitcherDF[pitcherDF$playerID=="clemero02"&pitcherDF$yearID==1999,]$BAOpp <- .261
pitcherDF[pitcherDF$playerID=="johnsra05"&pitcherDF$yearID==1999,]$BAOpp <- .224
pitcherDF <- pitcherDF[-c(151,152),] # remove values from Phil Niekrpho1
pitcherDF[pitcherDF$playerID=="niekrph01"&pitcherDF$yearID==1987,]$ERA <- (5.89+8.25+15)/3
pitcherDF[pitcherDF$playerID=="niekrph01"&pitcherDF$yearID==1987,]$WHIP <- (1.577+1.833+4)/3
pitcherDF[pitcherDF$playerID=="niekrph01"&pitcherDF$yearID==1987,]$BAOpp <- (.28+.3+.42)/3
pitcherDF <- pitcherDF[-32,]
pitcherDF[pitcherDF$playerID=="nichoki01"&pitcherDF$yearID==1905,]$ERA <- (2.27+5.4)/2
pitcherDF[pitcherDF$playerID=="nichoki01"&pitcherDF$yearID==1905,]$BAOpp <- (.25+.29)/2
pitcherDF[pitcherDF$playerID=="nichoki01"&pitcherDF$yearID==1905,]$WHIP <- (1.002+1.258)/2
pitcherDF <- pitcherDF[-214,]
pitcherDF[pitcherDF$playerID=="maddugr01"&pitcherDF$yearID==2008,]$ERA <- (5.09+3.99)/2
pitcherDF[pitcherDF$playerID=="maddugr01"&pitcherDF$yearID==2008,]$BAOpp <- (.276+.271)/2
pitcherDF[pitcherDF$playerID=="maddugr01"&pitcherDF$yearID==2008,]$WHIP <- (1.089+1.107)/2
pitcherDF <- pitcherDF[-145,]
pitcherDF[pitcherDF$playerID=="seaveto01"&pitcherDF$yearID==1986,]$ERA <- (4.38+3.8)/2
pitcherDF[pitcherDF$playerID=="seaveto01"&pitcherDF$yearID==1986,]$BAOpp <- (.27+.24)/2
pitcherDF[pitcherDF$playerID=="seaveto01"&pitcherDF$yearID==1986,]$WHIP <- (1.141+.944)/2
pitcherDF <- pitcherDF[-153,]
pitcherDF[pitcherDF$playerID=="johnsra05"&pitcherDF$yearID==1989,]$ERA <- (4.4+6.67)/2
pitcherDF[pitcherDF$playerID=="johnsra05"&pitcherDF$yearID==1989,]$BAOpp <- (.24+.26)/2
pitcherDF[pitcherDF$playerID=="johnsra05"&pitcherDF$yearID==1989,]$WHIP <- (.954+.978)/2
pitcherDF <- pitcherDF[-180,]
pitcherDF[pitcherDF$playerID=="johnsra05"&pitcherDF$yearID==1998,]$ERA <- (1.28+4.33)/2
pitcherDF[pitcherDF$playerID=="johnsra05"&pitcherDF$yearID==1998,]$BAOpp <- .224
pitcherDF[pitcherDF$playerID=="johnsra05"&pitcherDF$yearID==1998,]$WHIP <- (.794+.969)/2
pitcherDF <- pitcherDF[-205,]
pitcherDF[pitcherDF$playerID=="maddugr01"&pitcherDF$yearID==2006,]$ERA <- (4.69+3.3)/2
pitcherDF[pitcherDF$playerID=="maddugr01"&pitcherDF$yearID==2006,]$BAOpp <- (.284+.244)/2
pitcherDF[pitcherDF$playerID=="maddugr01"&pitcherDF$yearID==2006,]$WHIP <- (1.188+.977)/2
pitcherDF <- pitcherDF[-74,]
pitcherDF[pitcherDF$playerID=="alexape01"&pitcherDF$yearID==1926,]$ERA <- (2.91+3.46)/2
pitcherDF[pitcherDF$playerID=="alexape01"&pitcherDF$yearID==1926,]$BAOpp <- (.24+.27)/2
pitcherDF[pitcherDF$playerID=="alexape01"&pitcherDF$yearID==1926,]$WHIP <- (.978+1.115)/2
pitcherDF <- pitcherDF[-121,]
pitcherDF[pitcherDF$playerID=="seaveto01"&pitcherDF$yearID==1977,]$ERA <- (2.34+3)/2
pitcherDF[pitcherDF$playerID=="seaveto01"&pitcherDF$yearID==1977,]$BAOpp <- (.2+.22)/2
pitcherDF[pitcherDF$playerID=="seaveto01"&pitcherDF$yearID==1977,]$WHIP <- (.81+.896)/2
pitcherDF <- pitcherDF[-44,]
pitcherDF[pitcherDF$playerID=="youngcy01"&pitcherDF$yearID==1911,]$ERA <- (3.88+3.71)/2
pitcherDF[pitcherDF$playerID=="youngcy01"&pitcherDF$yearID==1911,]$BAOpp <- (.26+.29)/2
pitcherDF[pitcherDF$playerID=="youngcy01"&pitcherDF$yearID==1911,]$WHIP <- (1.087+1.230)/2
offenseDF <- offenseDF[-106,]
offenseDF[offenseDF$playerID=="hornsro01"&offenseDF$yearID==1933,]$Avg <- (.325+.333)/2
offenseDF[offenseDF$playerID=="hornsro01"&offenseDF$yearID==1933,]$HR <- 3
offenseDF[offenseDF$playerID=="hornsro01"&offenseDF$yearID==1933,]$X2B <- 7
offenseDF[offenseDF$playerID=="hornsro01"&offenseDF$yearID==1933,]$X3B <- 0



# color choices for hitters
cols <- c("HR"="#f04546","Doubles"="#3591d1","Triples"="#62c76b","Average"="#000000")
#create graphs for hitters
count = 1
for(i in offensivePlayers){
  dat <- offenseDF[offenseDF$playerID==i,]
 
 x <- ggplot(dat, aes(x=yearID))+
    geom_line(size=.75,aes(y=HR,colour="HR"))+
    geom_line(size=.75,aes(y=X2B,colour="Doubles"))+  
    geom_line(size=.75,aes(y=X3B,colour="Triples"))+
    geom_line(size=.75,aes(y=Avg*100,colour="Average"))+ geom_point(aes(y=Avg*100))+
      ggtitle(paste(count,". ",offenseNames[count],sep=""))+
      scale_colour_manual(name="",values=cols)+
      theme_fivethirtyeight()
 ggsave(x, file=paste(offenseNames[count],".png"),width=7,height=5.72)
 count = count+1
}

count =1
# create graphs for pitchers 
colsPitcher <- c("WHIP"="#000000","Opponent Batting Average"="#3591d1","Earned Run Average"="#62c76b")
for(i in pitchers){
  dat <- pitcherDF[pitcherDF$playerID==i,]
  
  y <- ggplot(dat, aes(x=yearID))+
    geom_line(size=.75,aes(y=WHIP,colour="WHIP"))+geom_point(aes(y=WHIP))+
    geom_line(size=.75,aes(y=BAOpp*10,colour="Opponent Batting Average"))+  
    geom_line(size=.75,aes(y=ERA,colour="Earned Run Average"))+
    ggtitle(paste(count,". ",pitcherNames[count],sep=""))+
    scale_colour_manual(name="",values=colsPitcher)+
    theme_fivethirtyeight()
  ggsave(y, file=paste(pitcherNames[count],".png"),width=8,height=6.72)
  count = count+1
}
