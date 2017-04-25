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
##    nhlTeamsPenaltyKill.R
## 3. Move test files into R working directory
## 4. Load NHL Playoff Data into R Data Frame
##       games <- read.table("nhl_2017_playoffs.txt", sep=",", header=TRUE, stringsAsFactors = TRUE)
## 5. Install (if necessary) and load dlpyr library package
##       install.packages("dplyr")  ## install dplyr library
##       library(dplyr)             ## load dplyr library
## 6. Add R functions in script
##       source("nhlTeamsPenaltyKill.R")
## 7. Run function playoffMatchup to compare teams
##       allTeamsPK(games)   ## List all team's penalty kill total 

teamTotalGames <- function(nhlgames, nhlteam) {
  teamTotalGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam | nhlgames$roadTeam == nhlteam))
  return(nrow(teamTotalGames))
}

teamPKGoalsAllowed <- function(nhlgames, nhlteam) {
  roadGames <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  homeGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamPKGoalsAllowed <- as.numeric(sum(roadGames$htPPConverted) + sum(homeGames$rtPPConverted))
  
  return(teamPKGoalsAllowed)
}

teamPKSuccessful <- function(nhlgames, nhlteam) {
  roadGames <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  homeGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamPKSuccessful <- as.numeric((sum(roadGames$htPowerPlays) - sum(roadGames$htPPConverted))
	+ (sum(homeGames$rtPowerPlays) - sum(homeGames$rtPPConverted)))
  
  return(teamPKSuccessful)
}

teamPKAttempts <- function(nhlgames, nhlteam) {
  roadGames <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  homeGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamPKAttempts <- as.numeric(sum(roadGames$htPowerPlays) + sum(homeGames$rtPowerPlays))
  
  return(teamPKAttempts)
}

teamPK <- function(nhlgames, nhlteam) {
  teamPK <- data.frame(matrix(ncol = 5))
  names(teamPK) <- c("team", "games", "PKGoalsAllowed", "PKAttempts", "PKConv")
  teamPK[1,1] <- nhlteam
  teamPK[1,2] <- teamTotalGames(nhlgames, nhlteam)
  teamPK[1,3] <- teamPKSuccessful(nhlgames, nhlteam)
  teamPK[1,4] <- teamPKAttempts(nhlgames, nhlteam)
  teamPK[1,5] <- round((teamPK[3] / teamPK[4]) * 100, digits = 3)
  return(teamPK)
}
  
allTeamsPK <- function(nhlgames) {
  teamPK <- data.frame(matrix(ncol = 5, nrow = 0))
  names(teamPK) <- c("team", "games", "PKGoalsAllowed", "PKAttempts", "PKConv")
  roadteams <- cbind(distinct(nhlgames,roadTeam))
  
  for (i in roadteams$roadTeam) {
    teamPK <- rbind(teamPK, teamPK(nhlgames,i))
  }
  
  arrange(teamPK, desc(PKConv))
  
} 
