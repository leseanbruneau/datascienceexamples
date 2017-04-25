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
##    nhlTeamsFaceoffs.R
## 3. Move test files into R working directory
## 4. Load NHL Playoff Data into R Data Frame
##       games <- read.table("nhl_2017_playoffs.txt", sep=",", header=TRUE, stringsAsFactors = TRUE)
## 5. Install (if necessary) and load dlpyr library package
##       install.packages("dplyr")  ## install dplyr library
##       library(dplyr)             ## load dplyr library
## 6. Add R functions in script
##       source("nhlTeamsFaceoffs.R")
## 7. Run function playoffMatchup to compare teams
##       allTeamsFO(games)   ## List all team's faceoff win-loss total 


teamTotalGames <- function(nhlgames, nhlteam) {
  teamTotalGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam | nhlgames$roadTeam == nhlteam))
  return(nrow(teamTotalGames))
}

teamFOWon <- function(nhlgames, nhlteam) {
  roadGames <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  homeGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamFOWon <- as.numeric(sum(roadGames$rtFaceOffsWon) + sum(homeGames$htFaceOffsWon))
  
  return(teamFOWon)
}

teamFOLoss <- function(nhlgames, nhlteam) {
  roadGames <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  homeGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamFOLoss <- as.numeric(sum(roadGames$htFaceOffsWon) + sum(homeGames$rtFaceOffsWon))
  
  return(teamFOLoss)
}

teamFO <- function(nhlgames, nhlteam) {
  teamFO <- data.frame(matrix(ncol = 5))
  names(teamFO) <- c("team", "games", "FOWon", "FOLoss", "FOWinPct")
  teamFO[1,1] <- nhlteam
  teamFO[1,2] <- teamTotalGames(nhlgames, nhlteam)
  teamFO[1,3] <- teamFOWon(nhlgames, nhlteam)
  teamFO[1,4] <- teamFOLoss(nhlgames, nhlteam)
  teamFO[1,5] <- round((teamFO[3] / (teamFO[3] + teamFO[4])) * 100, digits = 3)
  return(teamFO)
}
  
allTeamsFO <- function(nhlgames) {
  teamFO <- data.frame(matrix(ncol = 5, nrow = 0))
  names(teamFO) <- c("team", "games", "FOWon", "FOLoss", "FOWinPct")
  roadteams <- cbind(distinct(nhlgames,roadTeam))
  
  for (i in roadteams$roadTeam) {
    teamFO <- rbind(teamFO, teamFO(nhlgames,i))
  }
  
  arrange(teamFO, desc(FOWinPct))
  
} 
