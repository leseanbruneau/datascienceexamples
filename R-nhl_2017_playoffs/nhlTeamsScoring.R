## Data Science Examples Blog
## http://datascienceexamples.com
## 
## Example R script
## Using NHL 2017 Playoff Game Data
##    http://sports.yahoo.com/nhl/scoreboard/  
##
## 1. Create R working directory 
## 2. Get test files from Github (https://github.com/mndatascienceexamples/datascienceexamples)
##    nhl_2017_playoffs.txt
##    nhlTeamsScoring.R
## 3. Move test files into R working directory
## 4. Load NHL Playoff Data into R Data Frame
##       games <- read.table("nhl_2017_playoffs.txt", sep=",", header=TRUE, stringsAsFactors = TRUE)
## 5. Install (if necessary) and load dlpyr library package
##       install.packages("dplyr")  ## install dplyr library
##       library(dplyr)             ## load dplyr library
## 6. Add R functions in script
##       source("nhlTeamsScoring.R")
## 7. Run function playoffMatchup to compare teams
##       allTeamsScoring(games)   ## List all team's scoring total 


teamTotalGames <- function(nhlgames, nhlteam) {
  teamTotalGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam | nhlgames$roadTeam == nhlteam))
  return(nrow(teamTotalGames))
}

teamFirstPeriodGoals <- function(nhlgames, nhlteam) {
  totalRoadGoals <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  totalHomeGoals <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  team1PGoals <- as.numeric(sum(totalRoadGoals$rtGoals1P) + sum(totalHomeGoals$htGoals1P))
  
  return(team1PGoals)
}

teamSecondPeriodGoals <- function(nhlgames, nhlteam) {
  totalRoadGoals <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  totalHomeGoals <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  team2PGoals <- as.numeric(sum(totalRoadGoals$rtGoals2P) + sum(totalHomeGoals$htGoals2P))
  
  return(team2PGoals)
}

teamThirdPeriodGoals <- function(nhlgames, nhlteam) {
  totalRoadGoals <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  totalHomeGoals <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  team3PGoals <- as.numeric(sum(totalRoadGoals$rtGoals3P) + sum(totalHomeGoals$htGoals3P))
  
  return(team3PGoals)
}

teamOTPeriodGoals <- function(nhlgames, nhlteam) {
  totalRoadGoals <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  totalHomeGoals <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamOTGoals <- as.numeric(sum(totalRoadGoals$rtGoals1OT) + sum(totalRoadGoals$rtGoals2OT) + sum(totalRoadGoals$rtGoals3OT) +
	+ sum(totalHomeGoals$htGoals1OT)  + sum(totalHomeGoals$htGoals2OT) + sum(totalHomeGoals$htGoals3OT))
  
  return(teamOTGoals)
}

teamTotalGoals <- function(nhlgames, nhlteam) {
  totalRoadGoals <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  totalHomeGoals <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamTotalGoals <- as.numeric(sum(totalRoadGoals$rtTotalGoals) + sum(totalHomeGoals$htTotalGoals))
  
  return(teamTotalGoals)
}

teamScoring <- function(nhlgames, nhlteam) {
  teamScore <- data.frame(matrix(ncol = 8))
  names(teamScore) <- c("team", "games", "1Period", "2Period", "3Period", "OT", "TotalGoals", "GoalsPerGame")
  teamScore[1,1] <- nhlteam
  teamScore[1,2] <- teamTotalGames(nhlgames, nhlteam)
  teamScore[1,3] <- teamFirstPeriodGoals(nhlgames, nhlteam)
  teamScore[1,4] <- teamSecondPeriodGoals(nhlgames, nhlteam)
  teamScore[1,5] <- teamThirdPeriodGoals(nhlgames, nhlteam)
  teamScore[1,6] <- teamOTPeriodGoals(nhlgames, nhlteam)
  teamScore[1,7] <- teamTotalGoals(nhlgames, nhlteam)
  teamScore[1,8] <- round((teamScore[7] / teamScore[2]), digits = 3)
  return(teamScore)
}
  
allTeamsScoring <- function(nhlgames) {
  teamScore <- data.frame(matrix(ncol = 8, nrow = 0))
  names(teamScore) <- c("team", "games", "1Period", "2Period", "3Period", "OT", "TotalGoals", "GoalsPerGame")
  roadteams <- cbind(distinct(nhlgames,roadTeam))
  
  for (i in roadteams$roadTeam) {
    teamScore <- rbind(teamScore, teamScoring(nhlgames,i))
  }
  
  arrange(teamScore, desc(GoalsPerGame))
  
} 



