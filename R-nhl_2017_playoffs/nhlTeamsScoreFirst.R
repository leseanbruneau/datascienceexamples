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
##    nhlTeamsScoreFirst.R
## 3. Move test files into R working directory
## 4. Load NHL Playoff Data into R Data Frame
##       games <- read.table("nhl_2017_playoffs.txt", sep=",", header=TRUE, stringsAsFactors = TRUE)
## 5. Install (if necessary) and load dlpyr library package
##       install.packages("dplyr")  ## install dplyr library
##       library(dplyr)             ## load dplyr library
## 6. Add R functions in script
##       source("nhlTeamsScoreFirst.R")
## 7. Run function playoffMatchup to compare teams
##       allTeamsScoreFirstRecords(games)   ## List all team's records when team scores first 


teamScoredFirstTotalGames <- function(nhlgames, nhlteam) {
  totalGamesTeamScoredFirst <- subset(nhlgames, (
      (nhlgames$homeTeam == nhlteam & nhlgames$scoredFirst == 'H')
	      | (nhlgames$roadTeam == nhlteam & nhlgames$scoredFirst == 'R'))
	)
	return(nrow(totalGamesTeamScoredFirst))
}

teamScoredFirstTotalWins <- function(nhlgames, nhlteam) {
  totalTeamScoreFirstWins <- subset(nhlgames, (
      (nhlgames$homeTeam == nhlteam & nhlgames$scoredFirst == 'H' & (nhlgames$htTotalGoals > nhlgames$rtTotalGoals))
	      | (nhlgames$roadTeam == nhlteam & nhlgames$scoredFirst == 'R' & (nhlgames$rtTotalGoals > nhlgames$htTotalGoals))
    ))
  return(nrow(totalTeamScoreFirstWins))
}

teamScoredFirstTotalLosses <- function(nhlgames, nhlteam) {
  totalTeamScoreFirstLosses <- subset(nhlgames, (
      (nhlgames$homeTeam == nhlteam & nhlgames$scoredFirst == 'H' & (nhlgames$htTotalGoals < nhlgames$rtTotalGoals))
	  | (nhlgames$roadTeam == nhlteam & nhlgames$scoredFirst == 'R' & (nhlgames$rtTotalGoals < nhlgames$htTotalGoals))
    ))
  return(nrow(totalTeamScoreFirstLosses))
}


teamScoreFirstRecord <- function(nhlgames, nhlteam) {
  teamscorefirstrecord <- data.frame(matrix(ncol = 5))
  names(teamscorefirstrecord) <- c("team", "games", "win", "loss", "winPct")
  teamscorefirstrecord[1,1] <- nhlteam
  teamscorefirstrecord[1,2] <- teamScoredFirstTotalGames(nhlgames, nhlteam)
  teamscorefirstrecord[1,3] <- teamScoredFirstTotalWins(nhlgames, nhlteam)
  teamscorefirstrecord[1,4] <- teamScoredFirstTotalLosses(nhlgames, nhlteam)
  teamscorefirstrecord[1,5] <- round((teamscorefirstrecord[3] / teamscorefirstrecord[2]), digits = 3)
  return(teamscorefirstrecord)
}
  
allTeamsScoreFirstRecords <- function(nhlgames) {
  teamScoreFirstRec <- data.frame(matrix(ncol = 5, nrow = 0))
  names(teamScoreFirstRec) <- c("team", "games", "win", "loss", "winPct")
  roadteams <- cbind(distinct(nhlgames,roadTeam))
  
  for (i in roadteams$roadTeam) {
    teamScoreFirstRec <- rbind(teamScoreFirstRec, teamScoreFirstRecord(nhlgames,i))
  }
  
  arrange(teamScoreFirstRec, desc(winPct, games))
  
} 

