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
##    nhlTeamsBlockedShots.R
## 3. Move test files into R working directory
## 4. Load NHL Playoff Data into R Data Frame
##       games <- read.table("nhl_2017_playoffs.txt", sep=",", header=TRUE, stringsAsFactors = TRUE)
## 5. Install (if necessary) and load dlpyr library package
##       install.packages("dplyr")  ## install dplyr library
##       library(dplyr)             ## load dplyr library
## 6. Add R functions in script
##       source("nhlTeamsBlockedShots.R")
## 7. Run function playoffMatchup to compare teams
##       allTeamsBlockedShots(games)   ## List all team's blocked shots 


teamTotalGames <- function(nhlgames, nhlteam) {
  teamTotalGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam | nhlgames$roadTeam == nhlteam))
  return(nrow(teamTotalGames))
}

teamBShots <- function(nhlgames, nhlteam) {
  roadGames <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  homeGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamBShots <- as.numeric(sum(roadGames$rtBlockedShots) + sum(homeGames$htBlockedShots))
  
  return(teamBShots)
}

teamBlockedShots <- function(nhlgames, nhlteam) {
  teamBlockedShots <- data.frame(matrix(ncol = 4))
  names(teamBlockedShots) <- c("team", "games", "BlockedShots", "BlockedShotsPerGame")
  teamBlockedShots[1,1] <- nhlteam
  teamBlockedShots[1,2] <- teamTotalGames(nhlgames, nhlteam)
  teamBlockedShots[1,3] <- teamBShots(nhlgames, nhlteam)
  teamBlockedShots[1,4] <- round((teamBlockedShots[3] / teamBlockedShots[2]), digits = 3)
  return(teamBlockedShots)
}
  
allTeamsBlockedShots <- function(nhlgames) {
  teamBlockedShots <- data.frame(matrix(ncol = 4, nrow = 0))
  names(teamBlockedShots) <- c("team", "games", "BlockedShots", "BlockedShotsPerGame")
  roadteams <- cbind(distinct(nhlgames,roadTeam))
  
  for (i in roadteams$roadTeam) {
    teamBlockedShots <- rbind(teamBlockedShots, teamBlockedShots(nhlgames,i))
  }
  
  arrange(teamBlockedShots, desc(BlockedShotsPerGame))
  
} 
