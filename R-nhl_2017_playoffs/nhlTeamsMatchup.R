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
##    nhlTeamsMatchup.R
## 3. Move test files into R working directory
## 4. Load NHL Playoff Data into R Data Frame
##       games <- read.table("nhl_2017_playoffs.txt", sep=",", header=TRUE, stringsAsFactors = TRUE)
## 5. Install (if necessary) and load dlpyr library package
##       install.packages("dplyr")  ## install dplyr library
##       library(dplyr)             ## load dplyr library
## 6. Add R functions in script
##       source("nhlTeamsMatchup.R")
## 7. Run function playoffMatchup to compare teams
##       playoffMatchup(games, "MIN", "STL")   ## MIN-STL series example 

 

teamsTotalGames <- function(nhlgames, nhlteam1, nhlteam2) {
  teamsTotalGames <- subset(nhlgames, (
      (nhlgames$roadTeam == nhlteam1 & nhlgames$homeTeam == nhlteam2)
	      | (nhlgames$roadTeam == nhlteam2 & nhlgames$homeTeam == nhlteam1))
	)
	return(teamsTotalGames)
}

teamWins <- function(games, team, opponent) {
	
	teamWins <- subset(games, (
      (games$homeTeam == team & games$roadTeam == opponent & (games$htTotalGoals > games$rtTotalGoals))
	      | (games$roadTeam == team & games$homeTeam == opponent & (games$rtTotalGoals > games$htTotalGoals))
    ))
	
	return(nrow(teamWins))
}

matchupWins <- function(games, team1, team2) {

	matchupWins <- data.frame(matrix(ncol = 3, nrow = 0))
	names(matchupWins) <- c("Statistic", team1, team2)
	
	matchupWins[1,1] <- "Wins"
	matchupWins[1,2] <- teamWins(games,team1,team2)
	matchupWins[1,3] <- teamWins(games,team2,team1)
	
	return(matchupWins)
}

teamOTWins <- function(games, team, opponent) {
	
	teamOTWins <- subset(games, (
      (games$homeTeam == team & games$roadTeam == opponent & (games$htTotalGoals > games$rtTotalGoals)
			& ((games$htGoals1OT + games$htGoals2OT + games$htGoals3OT) > 0))
	  | (games$roadTeam == team & games$homeTeam == opponent & (games$rtTotalGoals > games$htTotalGoals)
			& ((games$rtGoals1OT + games$rtGoals2OT + games$rtGoals3OT) > 0))
	))
	
	return(nrow(teamOTWins))
}

matchupOTWins <- function(games, team1, team2) {

	matchupOTWins <- data.frame(matrix(ncol = 3, nrow = 0))
	names(matchupOTWins) <- c("Statistic", team1, team2)
	
	matchupOTWins[1,1] <- "OT Wins"
	matchupOTWins[1,2] <- teamOTWins(games,team1,team2)
	matchupOTWins[1,3] <- teamOTWins(games,team2,team1)
	
	return(matchupOTWins)
}


teamTotalGoals <- function(games, team, opponent) {
	
	totalRoadGoals <- subset(games, (games$roadTeam == team & games$homeTeam == opponent))
	totalHomeGoals <- subset(games, (games$roadTeam == opponent & games$homeTeam == team))
	teamTotalGoals <- as.numeric(sum(totalRoadGoals$rtTotalGoals) + sum(totalHomeGoals$htTotalGoals))

	return(teamTotalGoals)
}

matchupTotalGoals <- function(games, team1, team2) {

	matchupTotalGoals <- data.frame(matrix(ncol = 3, nrow = 0))
	names(matchupTotalGoals) <- c("Statistic", team1, team2)
	
	matchupTotalGoals[1,1] <- "Goals"
	matchupTotalGoals[1,2] <- teamTotalGoals(games,team1,team2)
	matchupTotalGoals[1,3] <- teamTotalGoals(games,team2,team1)
	
	return(matchupTotalGoals)
}

teamSOG <- function(games, team, opponent) {
	
	totalRoadSOG <- subset(games, (games$roadTeam == team & games$homeTeam == opponent))
	totalHomeSOG <- subset(games, (games$roadTeam == opponent & games$homeTeam == team))
	teamTotalSOG <- as.numeric(sum(totalRoadSOG$rtTotalSOG) + sum(totalHomeSOG$htTotalSOG))

	return(teamTotalSOG)
}

matchupTotalSOG <- function(games, team1, team2) {

	matchupTotalSOG <- data.frame(matrix(ncol = 3, nrow = 0))
	names(matchupTotalSOG) <- c("Statistic", team1, team2)
	
	matchupTotalSOG[1,1] <- "Shots on Goal"
	matchupTotalSOG[1,2] <- teamSOG(games,team1,team2)
	matchupTotalSOG[1,3] <- teamSOG(games,team2,team1)
	
	return(matchupTotalSOG)
}

teamScoreFirst <- function(games, team, opponent) {
	
	totalRoadScoreFirst <- subset(games, (games$roadTeam == team & games$homeTeam == opponent) 
		& games$scoredFirst == 'R')
	totalHomeScoreFirst <- subset(games, (games$roadTeam == opponent & games$homeTeam == team) 
		& games$scoredFirst == 'H')
	teamScoreFirst <- as.numeric(sum(nrow(totalRoadScoreFirst)) + sum(nrow(totalHomeScoreFirst)))

	return(teamScoreFirst)
}

matchupTeamScoreFirst <- function(games, team1, team2) {

	matchupTeamScoreFirst <- data.frame(matrix(ncol = 3, nrow = 0))
	names(matchupTeamScoreFirst) <- c("Statistic", team1, team2)
	
	matchupTeamScoreFirst[1,1] <- "Team Score First"
	matchupTeamScoreFirst[1,2] <- teamScoreFirst(games,team1,team2)
	matchupTeamScoreFirst[1,3] <- teamScoreFirst(games,team2,team1)
	
	return(matchupTeamScoreFirst)
}

teamPPGoals <- function(games, team, opponent) {
	
	totalRoadPPGoals <- subset(games, (games$roadTeam == team & games$homeTeam == opponent))
	totalHomePPGoals <- subset(games, (games$roadTeam == opponent & games$homeTeam == team))
	teamPPGoals <- as.numeric(sum(totalRoadPPGoals$rtPPConverted) + sum(totalHomePPGoals$htPPConverted))

	return(teamPPGoals)
}

matchupTotalPPGoals <- function(games, team1, team2) {

	matchupTotalPPGoals <- data.frame(matrix(ncol = 3, nrow = 0))
	names(matchupTotalPPGoals) <- c("Statistic", team1, team2)
	
	matchupTotalPPGoals[1,1] <- "PP Goals"
	matchupTotalPPGoals[1,2] <- teamPPGoals(games,team1,team2)
	matchupTotalPPGoals[1,3] <- teamPPGoals(games,team2,team1)
	
	return(matchupTotalPPGoals)
}

teamPPAttempts <- function(games, team, opponent) {
	
	totalRoadPPAttempts <- subset(games, (games$roadTeam == team & games$homeTeam == opponent))
	totalHomePPAttempts <- subset(games, (games$roadTeam == opponent & games$homeTeam == team))
	teamPPAttempts <- as.numeric(sum(totalRoadPPAttempts$rtPowerPlays) + sum(totalHomePPAttempts$htPowerPlays))

	return(teamPPAttempts)
}

matchupTotalPPAttempts <- function(games, team1, team2) {

	matchupTotalPPAttempts <- data.frame(matrix(ncol = 3, nrow = 0))
	names(matchupTotalPPAttempts) <- c("Statistic", team1, team2)
	
	matchupTotalPPAttempts[1,1] <- "PP Attempts"
	matchupTotalPPAttempts[1,2] <- teamPPAttempts(games,team1,team2)
	matchupTotalPPAttempts[1,3] <- teamPPAttempts(games,team2,team1)
	
	return(matchupTotalPPAttempts)
}

teamFOWon <- function(games, team, opponent) {
	
	totalRoadFOWon <- subset(games, (games$roadTeam == team & games$homeTeam == opponent))
	totalHomeFOWon <- subset(games, (games$roadTeam == opponent & games$homeTeam == team))
	teamFOWon <- as.numeric(sum(totalRoadFOWon$rtFaceOffsWon) + sum(totalHomeFOWon$htFaceOffsWon))

	return(teamFOWon)
}

matchupTotalFOWon <- function(games, team1, team2) {

	matchupTotalFOWon <- data.frame(matrix(ncol = 3, nrow = 0))
	names(matchupTotalFOWon) <- c("Statistic", team1, team2)
	
	matchupTotalFOWon[1,1] <- "Faceoffs Won"
	matchupTotalFOWon[1,2] <- teamFOWon(games,team1,team2)
	matchupTotalFOWon[1,3] <- teamFOWon(games,team2,team1)
	
	return(matchupTotalFOWon)
}

teamHits <- function(games, team, opponent) {
	
	totalRoadHits <- subset(games, (games$roadTeam == team & games$homeTeam == opponent))
	totalHomeHits <- subset(games, (games$roadTeam == opponent & games$homeTeam == team))
	teamHits <- as.numeric(sum(totalRoadHits$rtHits) + sum(totalHomeHits$htHits))

	return(teamHits)
}

matchupTotalHits <- function(games, team1, team2) {

	matchupTotalHits <- data.frame(matrix(ncol = 3, nrow = 0))
	names(matchupTotalHits) <- c("Statistic", team1, team2)
	
	matchupTotalHits[1,1] <- "Hits"
	matchupTotalHits[1,2] <- teamHits(games,team1,team2)
	matchupTotalHits[1,3] <- teamHits(games,team2,team1)
	
	return(matchupTotalHits)
}

teamBlockedShots <- function(games, team, opponent) {
	
	totalRoadBlockedShots <- subset(games, (games$roadTeam == team & games$homeTeam == opponent))
	totalHomeBlockedShots <- subset(games, (games$roadTeam == opponent & games$homeTeam == team))
	teamBlockedShots <- as.numeric(sum(totalRoadBlockedShots$rtBlockedShots) + sum(totalHomeBlockedShots$htBlockedShots))

	return(teamBlockedShots)
}

matchupTotalBlockedShots <- function(games, team1, team2) {

	matchupTotalBlockedShots <- data.frame(matrix(ncol = 3, nrow = 0))
	names(matchupTotalBlockedShots) <- c("Statistic", team1, team2)
	
	matchupTotalBlockedShots[1,1] <- "Blocked Shots"
	matchupTotalBlockedShots[1,2] <- teamBlockedShots(games,team1,team2)
	matchupTotalBlockedShots[1,3] <- teamBlockedShots(games,team2,team1)
	
	return(matchupTotalBlockedShots)
}

playoffMatchup <- function(allnhlgames, team1, team2)  {

	matchupGames <- teamsTotalGames(allnhlgames, team1, team2);

	matchup <- data.frame(matrix(ncol = 3, nrow = 0))
	names(matchup) <- c("Statistic", team1, team2)
	
	matchup <- rbind(matchup, matchupWins(matchupGames, team1, team2))
	matchup <- rbind(matchup, matchupOTWins(matchupGames, team1, team2))
	matchup <- rbind(matchup, matchupTotalGoals(matchupGames, team1, team2))
	matchup <- rbind(matchup, matchupTotalSOG(matchupGames, team1, team2))
	matchup <- rbind(matchup, matchupTeamScoreFirst(matchupGames, team1, team2))
	matchup <- rbind(matchup, matchupTotalPPGoals(matchupGames, team1, team2))
	matchup <- rbind(matchup, matchupTotalPPAttempts(matchupGames, team1, team2))
	matchup <- rbind(matchup, matchupTotalFOWon(matchupGames, team1, team2))
	matchup <- rbind(matchup, matchupTotalHits(matchupGames, team1, team2))
	matchup <- rbind(matchup, matchupTotalBlockedShots(matchupGames, team1, team2))
	
	
	return(matchup)
}


