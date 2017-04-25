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
##    nhlOppScoreFirst.R
## 3. Move test files into R working directory
## 4. Load NHL Playoff Data into R Data Frame
##       games <- read.table("nhl_2017_playoffs.txt", sep=",", header=TRUE, stringsAsFactors = TRUE)
## 5. Install (if necessary) and load dlpyr library package
##       install.packages("dplyr")  ## install dplyr library
##       library(dplyr)             ## load dplyr library
## 6. Add R functions in script
##       source("nhlOppScoreFirst.R")
## 7. Run function playoffMatchup to compare teams
##       allTeamsOppScoreFirstRecords(games)   ## List all team's records when opponent scores first 


oppScoredFirstTotalGames <- function(nhlgames, nhlteam) {
  totalTeamGamesNotScoredFirst <- subset(nhlgames, (
      (nhlgames$homeTeam == nhlteam & nhlgames$scoredFirst == 'R')
	      | (nhlgames$roadTeam == nhlteam & nhlgames$scoredFirst == 'H'))
	)
	return(nrow(totalTeamGamesNotScoredFirst))
}

oppScoredFirstTotalWins <- function(nhlgames, nhlteam) {
  totalTeamWins <- subset(nhlgames, (
      (nhlgames$homeTeam == nhlteam & nhlgames$scoredFirst == 'R' & (nhlgames$htTotalGoals > nhlgames$rtTotalGoals))
	      | (nhlgames$roadTeam == nhlteam & nhlgames$scoredFirst == 'H' & (nhlgames$rtTotalGoals > nhlgames$htTotalGoals))
    ))
  return(nrow(totalTeamWins))
}

oppScoredFirstTotalLosses <- function(nhlgames, nhlteam) {
  totalTeamLosses <- subset(nhlgames, (
      (nhlgames$homeTeam == nhlteam & nhlgames$scoredFirst == 'R' & 
	  (nhlgames$htTotalGoals < nhlgames$rtTotalGoals))
	  | (nhlgames$roadTeam == nhlteam & nhlgames$scoredFirst == 'H' & (nhlgames$rtTotalGoals < nhlgames$htTotalGoals))
    ))
  return(nrow(totalTeamLosses))
}


teamOppScoreFirstRecord <- function(nhlgames, nhlteam) {
  teamoppscorefirstrecord <- data.frame(matrix(ncol = 5))
  names(teamoppscorefirstrecord) <- c("team", "games", "win", "loss", "winPct")
  teamoppscorefirstrecord[1,1] <- nhlteam
  teamoppscorefirstrecord[1,2] <- oppScoredFirstTotalGames(nhlgames, nhlteam)
  teamoppscorefirstrecord[1,3] <- oppScoredFirstTotalWins(nhlgames, nhlteam)
  teamoppscorefirstrecord[1,4] <- oppScoredFirstTotalLosses(nhlgames, nhlteam)
  teamoppscorefirstrecord[1,5] <- round((teamoppscorefirstrecord[3] / teamoppscorefirstrecord[2]), digits = 3)
  return(teamoppscorefirstrecord)
}
  
allTeamsOppScoreFirstRecords <- function(nhlgames) {
  teamOppScoreFirstRec <- data.frame(matrix(ncol = 5, nrow = 0))
  names(teamOppScoreFirstRec) <- c("team", "games", "win", "loss", "winPct")
  roadteams <- cbind(distinct(nhlgames,roadTeam))
  
  for (i in roadteams$roadTeam) {
    teamOppScoreFirstRec <- rbind(teamOppScoreFirstRec, teamOppScoreFirstRecord(nhlgames,i))
  }
  
  arrange(teamOppScoreFirstRec, desc(winPct))
}  

			  
			  