require(magrittr)
require(tibble)
require(dplyr)
require(ggplot2)
require(ggthemes)
require(gghighlight)

wd <- "C:\\Users\\Lukas Olson\\DataspellProjects\\UFOR401TreeAssessment"
ObjectID <- 1

setwd(wd)
data <- read.csv("data.csv")

get_match <- function (data, targetObjectID, targetDBHSizeRatio){

selectedEntry <- data[targetObjectID,]

targetDBHSize <- selectedEntry$DBH * targetDBHSizeRatio

dbhDataFrame <- filter(data, DBH >= targetDBHSize * 0.75 & DBH <= targetDBHSize * 1.25)
#divide dbh by target, subtract 1, take absolute value, invert, multiply by 10
dbhDataFrame <- dbhDataFrame %>% mutate(Score = (1 - abs((DBH / targetDBHSize) - 1)) * 10)

dbhDataFrame <- dbhDataFrame %>% mutate(Score = Score + ifelse(Genus == selectedEntry$Genus & Species == selectedEntry$Species, 20, ifelse(Genus == selectedEntry$Genus, 10, 0)))

dbhDataFrame <- dbhDataFrame %>% mutate(Score = Score + (abs(CROWN_LIGHTEXP - selectedEntry$CROWN_LIGHTEXP) * 2))

dbhDataFrame <- dbhDataFrame %>% mutate(Score = Score + ifelse(((1-(abs((NEIGHBORS / selectedEntry$NEIGHBORS) - 1)))*10) >= 0, ((1-(abs((NEIGHBORS / selectedEntry$NEIGHBORS) - 1)))*10),0 ))

finalRanking <- dbhDataFrame[-targetObjectID,]

return (finalRanking)
}

make_s_graph <- function (data, title){
  dataTop10Percent <- top_frac(data, 0.1, Score)
  dataMin <- min(dataTop10Percent$Score)
  dataMean <- mean(dataTop10Percent$Score)

  ggplot(data = data, aes(x = Score, y = THEIGHT, col = DBH)) + ggtitle(paste(title, " Tree Height (m) VS Score")) + geom_point() + gghighlight(Score >= dataMin) + geom_vline(xintercept = dataMean, linetype = "dashed", color = "red")
}

r2 <- get_match(data, ObjectID, 2)
r3 <- get_match(data, ObjectID, 3)
r4 <- get_match(data, ObjectID, 4)

make_s_graph(r2, "R2")
make_s_graph(r3, "R3")
make_s_graph(r4, "R4")

r2Top10 <- top_frac(r2, 0.1, Score)
r3Top10 <- top_frac(r3, 0.1, Score)
r4Top10 <- top_frac(r4, 0.1, Score)