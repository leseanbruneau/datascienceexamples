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
##    nhlTeamsShotsOnGoal.R
## 3. Move test files into R working directory
## 4. Load NHL Playoff Data into R Data Frame
##       games <- read.table("nhl_2017_playoffs.txt", sep=",", header=TRUE, stringsAsFactors = TRUE)
## 5. Install (if necessary) and load dlpyr library package
##       install.packages("dplyr")  ## install dplyr library
##       library(dplyr)             ## load dplyr library
## 6. Add R functions in script
##       source("nhlTeamsShotsOnGoal.R")
## 7. Run function playoffMatchup to compare teams
##       allTeamsSOG(games)   ## List all team's shots on goal total 


teamTotalGames <- function(nhlgames, nhlteam) {
  teamTotalGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam | nhlgames$roadTeam == nhlteam))
  return(nrow(teamTotalGames))
}

teamFirstPeriodShotsOnGoal <- function(nhlgames, nhlteam) {
  totalRoadGoals <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  totalHomeGoals <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  team1PShotsOnGoal <- as.numeric(sum(totalRoadGoals$rtSOG1P) + sum(totalHomeGoals$htSOG1P))
  
  return(team1PShotsOnGoal)
}

teamSecondPeriodShotsOnGoal <- function(nhlgames, nhlteam) {
  totalRoadGoals <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  totalHomeGoals <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  team2PShotsOnGoal <- as.numeric(sum(totalRoadGoals$rtSOG2P) + sum(totalHomeGoals$htSOG2P))
  
  return(team2PShotsOnGoal)
}

teamThirdPeriodShotsOnGoal <- function(nhlgames, nhlteam) {
  totalRoadGoals <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  totalHomeGoals <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  team3PShotOnGoal <- as.numeric(sum(totalRoadGoals$rtSOG3P) + sum(totalHomeGoals$htSOG3P))
  
  return(team3PShotOnGoal)
}

teamOTPeriodShotsOnGoal <- function(nhlgames, nhlteam) {
  totalRoadGoals <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  totalHomeGoals <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamOTShotsOnGoal <- as.numeric(sum(totalRoadGoals$rtSOG1OT) + sum(totalRoadGoals$rtSOG2OT) + sum(totalRoadGoals$rtSOG3OT) +
	+ sum(totalHomeGoals$htSOG1OT)  + sum(totalHomeGoals$htSOG2OT) + sum(totalHomeGoals$htSOG3OT))
  
  return(teamOTShotsOnGoal)
}

teamTotalShotsOnGoal <- function(nhlgames, nhlteam) {
  totalRoadGoals <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  totalHomeGoals <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamTotalShotsOnGoal <- as.numeric(sum(totalRoadGoals$rtTotalSOG) + sum(totalHomeGoals$htTotalSOG))
  
  return(teamTotalShotsOnGoal)
}

teamShotsOnGoal <- function(nhlgames, nhlteam) {
  teamShotsOnGoal <- data.frame(matrix(ncol = 8))
  names(teamShotsOnGoal) <- c("team", "games", "1Period", "2Period", "3Period", "OT", "TotalSOG", "SOGPerGame")
  teamShotsOnGoal[1,1] <- nhlteam
  teamShotsOnGoal[1,2] <- teamTotalGames(nhlgames, nhlteam)
  teamShotsOnGoal[1,3] <- teamFirstPeriodShotsOnGoal(nhlgames, nhlteam)
  teamShotsOnGoal[1,4] <- teamSecondPeriodShotsOnGoal(nhlgames, nhlteam)
  teamShotsOnGoal[1,5] <- teamThirdPeriodShotsOnGoal(nhlgames, nhlteam)
  teamShotsOnGoal[1,6] <- teamOTPeriodShotsOnGoal(nhlgames, nhlteam)
  teamShotsOnGoal[1,7] <- teamTotalShotsOnGoal(nhlgames, nhlteam)
  teamShotsOnGoal[1,8] <- round((teamShotsOnGoal[7] / teamShotsOnGoal[2]), digits = 3)
  return(teamShotsOnGoal)
}
  
allTeamsSOG <- function(nhlgames) {
  teamSOG <- data.frame(matrix(ncol = 8, nrow = 0))
  names(teamSOG) <- c("team", "games", "1Period", "2Period", "3Period", "OT", "TotalSOG", "SOGPerGame")
  roadteams <- cbind(distinct(nhlgames,roadTeam))
  
  for (i in roadteams$roadTeam) {
    teamSOG <- rbind(teamSOG, teamShotsOnGoal(nhlgames,i))
  }
  
  arrange(teamSOG, desc(SOGPerGame, games))
  
} 



