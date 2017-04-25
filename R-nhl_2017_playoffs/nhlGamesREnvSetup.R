## Data Science Examples Blog
## http://datascienceexamples.com
## 
## Example R script - Optional wrapper script to load all source files
##
## Using NHL 2017 Playoff Game Data
##    http://sports.yahoo.com/nhl/scoreboard/  
##
## 1. Create R working directory 
## 2. Get test files from Github (https://github.com/mndatascienceexamples/datascienceexamples)
##
##  List of R source along with command for main function call
##    nhl_2017_playoffs.txt    -> data input file
##    nhlOppScoreFirst.R       -> allTeamsOppScoreFirstRecords(games)
##    nhlTeamsBlockedShots.R   -> allTeamsBlockedShots(games)
##    nhlTeamsFaceoffs.R       -> allTeamsFO(games)
##    nhlTeamsHits.R           -> allTeamsHits(games)
##    nhlTeamsMatchup.R        -> playoffMatchup(games, "MIN", "STL")
##    nhlTeamsPenaltyKill.R    -> allTeamsPK(games)
##    nhlTeamsPowerPlay.R      -> allTeamsPP(games)
##    nhlTeamsScoreFirst.R     -> allTeamsScoreFirstRecords(games)
##    nhlTeamsScoring.R        -> allTeamsScoring(games)
##    nhlTeamsShotsOnGoal.R    -> allTeamsSOG(games)
##    
## 3. Move test files into R working directory
## 4. Install (if necessary) and load dlpyr library package
##       install.packages("dplyr")  ## install dplyr library
## 5. Load nhlGamesREnvSetup.R
##       source("nhlGamesREnvSetup.R")

# R Studio environment settings

options(prompt="R> ")

getwd()

# Local:
setwd("C:\\R\\")

rm(games)
#games <- read.table("nhl_playoffs.txt", sep=",", header=TRUE)
games <- read.table("nhl_playoffs.txt", sep=",", header=TRUE, stringsAsFactors = TRUE)


# Load Libraries
### If dplyr is not already installed
###   install.packages("dplyr")
library(dplyr)

# Source Scripts
source("nhlOppScoreFirst.R")
source("nhlTeamsBlockedShots.R")
source("nhlTeamsFaceoffs.R")
source("nhlTeamsHits.R")
source("nhlTeamsMatchup.R")
source("nhlTeamsPenaltyKill.R")
source("nhlTeamsPowerPlay.R")
source("nhlTeamsScoreFirst.R")
source("nhlTeamsScoring.R")
source("nhlTeamsShotsOnGoal.R")

