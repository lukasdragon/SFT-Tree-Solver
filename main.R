require(magrittr)
require(tibble)
require(dplyr)
require(ggplot2)
require(ggthemes)
require(gghighlight)

wd <- "C:\\Users\\Lukas Olson\\DataspellProjects\\UFOR401TreeAssessment"
targetObjectID <- 1
targetDBHSizeRatio <- 2

setwd(wd)
data <- read.csv("data.csv")
selectedEntry <- data[targetObjectID,]

targetDBHSize <- selectedEntry$DBH * targetDBHSizeRatio

dbhDataFrame <- filter(data, DBH >= targetDBHSize * 0.75 & DBH <= targetDBHSize * 1.25)
#divide dbh by target, subtract 1, take absolute value, invert, multiply by 10
dbhDataFrame <- dbhDataFrame %>% mutate(Score = (1 - abs((DBH / targetDBHSize) - 1)) * 10)

dbhDataFrame <- dbhDataFrame %>% mutate(Score = Score + ifelse(Genus == selectedEntry$Genus & Species == selectedEntry$Species, 20, ifelse(Genus == selectedEntry$Genus, 10, 0)))

dbhDataFrame <- dbhDataFrame %>% mutate(Score = Score + (abs(CROWN_LIGHTEXP - selectedEntry$CROWN_LIGHTEXP) * 2))

dbhDataFrame <- dbhDataFrame %>% mutate(Score = Score + ifelse(((1-(abs((NEIGHBORS / selectedEntry$NEIGHBORS) - 1)))*10) >= 0, ((1-(abs((NEIGHBORS / selectedEntry$NEIGHBORS) - 1)))*10),0 ))

finalRanking <- dbhDataFrame[-targetObjectID,]

top25 <- top_n(finalRanking, 0.25 * nrow(finalRanking), Score)

top25Mean <- mean(top25$Score)

# Plot results
ggplot(title = "Score vs Tree Height (m)",data = finalRanking, aes(x = Score, y = THEIGHT, col = DBH)) + geom_point() + gghighlight(Score >= top25Mean)

