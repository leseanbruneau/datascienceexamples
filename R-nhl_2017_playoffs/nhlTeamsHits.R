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
##    nhlTeamsHits.R
## 3. Move test files into R working directory
## 4. Load NHL Playoff Data into R Data Frame
##       games <- read.table("nhl_2017_playoffs.txt", sep=",", header=TRUE, stringsAsFactors = TRUE)
## 5. Install (if necessary) and load dlpyr library package
##       install.packages("dplyr")  ## install dplyr library
##       library(dplyr)             ## load dplyr library
## 6. Add R functions in script
##       source("nhlTeamsHits.R")
## 7. Run function playoffMatchup to compare teams
##       allTeamsHits(games)   ## List all team's hits 


teamTotalGames <- function(nhlgames, nhlteam) {
  teamTotalGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam | nhlgames$roadTeam == nhlteam))
  return(nrow(teamTotalGames))
}

teamHitsGiven <- function(nhlgames, nhlteam) {
  roadGames <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  homeGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamHitsGiven <- as.numeric(sum(roadGames$rtHits) + sum(homeGames$htHits))
  
  return(teamHitsGiven)
}

teamHitsTaken <- function(nhlgames, nhlteam) {
  roadGames <- subset(nhlgames, (nhlgames$roadTeam == nhlteam))
  homeGames <- subset(nhlgames, (nhlgames$homeTeam == nhlteam))
  
  teamHitsTaken <- as.numeric(sum(roadGames$htHits) + sum(homeGames$rtHits))
  
  return(teamHitsTaken)
}

teamHits <- function(nhlgames, nhlteam) {
  teamHits <- data.frame(matrix(ncol = 5))
  names(teamHits) <- c("team", "games", "HitsGiven", "HitsTaken", "HitDifference")
  teamHits[1,1] <- nhlteam
  teamHits[1,2] <- teamTotalGames(nhlgames, nhlteam)
  teamHits[1,3] <- teamHitsGiven(nhlgames, nhlteam)
  teamHits[1,4] <- teamHitsTaken(nhlgames, nhlteam)
  teamHits[1,5] <- teamHits[3] - teamHits[4]
  return(teamHits)
}
  
allTeamsHits <- function(nhlgames) {
  teamHits <- data.frame(matrix(ncol = 5, nrow = 0))
  names(teamHits) <- c("team", "games", "HitsGiven", "HitsTaken", "HitDifference")
  roadteams <- cbind(distinct(nhlgames,roadTeam))
  
  for (i in roadteams$roadTeam) {
    teamHits <- rbind(teamHits, teamHits(nhlgames,i))
  }
  
  arrange(teamHits, desc(HitDifference))
  
} 
