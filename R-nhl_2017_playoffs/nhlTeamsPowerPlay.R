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
##    nhlTeamsPowerPlay.R
## 3. Move test files into R working directory
## 4. Load NHL Playoff Data into R Data Frame
##       games <- read.table("nhl_2017_playoffs.txt", sep=",", header=TRUE, stringsAsFactors = TRUE)
## 5. Install (if necessary) and load dlpyr library package
##       install.packages("dplyr")  ## install dplyr library
##       library(dplyr)             ## load dplyr library
## 6. Add R functions in script
##       source("nhlTeamsPowerPlay.R")
## 7. Run function playoffMatchup to compare teams
##       allTeamsPP(games)   ## List all team's power play total 



teamTotalGames <- function(nhlgames, nhlteam) {
  teamTotalGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam | nhlgames$roadTeam == nhlteam))
  return(nrow(teamTotalGames))
}

teamPPGoals <- function(nhlgames, nhlteam) {
  roadGames <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  homeGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamPPGoals <- as.numeric(sum(roadGames$rtPPConverted) + sum(homeGames$htPPConverted))
  
  return(teamPPGoals)
}

teamPPAttempts <- function(nhlgames, nhlteam) {
  roadGames <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  homeGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamPPAttempts <- as.numeric(sum(roadGames$rtPowerPlays) + sum(homeGames$htPowerPlays))
  
  return(teamPPAttempts)
}

teamPP <- function(nhlgames, nhlteam) {
  teamPP <- data.frame(matrix(ncol = 5))
  names(teamPP) <- c("team", "games", "PPGoals", "PPAttempts", "PPConv")
  teamPP[1,1] <- nhlteam
  teamPP[1,2] <- teamTotalGames(nhlgames, nhlteam)
  teamPP[1,3] <- teamPPGoals(nhlgames, nhlteam)
  teamPP[1,4] <- teamPPAttempts(nhlgames, nhlteam)
  teamPP[1,5] <- round((teamPP[3] / teamPP[4]) * 100, digits = 3)
  return(teamPP)
}
  
allTeamsPP <- function(nhlgames) {
  teamPP <- data.frame(matrix(ncol = 5, nrow = 0))
  names(teamPP) <- c("team", "games", "PPGoals", "PPAttempts", "PPConv")
  roadteams <- cbind(distinct(nhlgames,roadTeam))
  
  for (i in roadteams$roadTeam) {
    teamPP <- rbind(teamPP, teamPP(nhlgames,i))
  }
  
  arrange(teamPP, desc(PPConv))
  
} 
