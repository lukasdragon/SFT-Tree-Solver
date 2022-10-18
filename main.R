require(magrittr)
require(tibble)
require(dplyr)
require(ggplot2)
require(ggthemes)
require(gghighlight)

wd <- "C:\\Users\\Lukas Olson\\DataspellProjects\\UFOR401TreeAssessment"
ObjectID <- 5

dbhMultipliers <- c(1, 2, 3, 4,5)


setwd(wd)
data <- read.csv("data.csv")

get_match <- function (data, targetObjectID, targetDBHSizeRatio){

selectedEntry <- data[targetObjectID,]

targetDBHSize <- selectedEntry$DBH * targetDBHSizeRatio

dbhDataFrame <- filter(data, DBH >= targetDBHSize * 0.7 & DBH <= targetDBHSize * 1.3)
#divide dbh by target, subtract 1, take absolute value, invert, multiply by regression where 1 is best match and gives 10 points. 0.7 is worst match and gives 1 point
dbhDataFrame <- dbhDataFrame %>% mutate(Score = (30 * (1 - abs((DBH / targetDBHSize) - 1))) - 20)

dbhDataFrame <- dbhDataFrame %>% mutate(Score = Score + ifelse(Genus == selectedEntry$Genus & Species == selectedEntry$Species, 20, ifelse(Genus == selectedEntry$Genus, 10, 0)))


dbhDataFrame <- dbhDataFrame %>% mutate(Score = Score + (1 - abs((CROWN_LIGHTEXP / selectedEntry$CROWN_LIGHTEXP) - 1)) * 10)

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

calculate_series <- function (data, targetObjectID, dbhMultiplierSeries){

  height <- NULL

  for (i in dbhMultiplierSeries){
    print(paste("DBH Size Ratio:", i))
    series <- get_match(data, targetObjectID, i)

    print(paste("Top score:", max(series$Score)))

    top10 <- top_frac(series, 0.1, Score)
    height <- c(height, mean(top10$THEIGHT))

    print(make_s_graph(get_match(data, targetObjectID, i), paste("DBH Size Ratio:", i)))
  }

  df <- data.frame(DBHSizeRatio = dbhMultiplierSeries, Height = height)
  return(df)
}

dataframe <- calculate_series(data, ObjectID, dbhMultipliers)


ggplot(data = dataframe, aes(x = DBHSizeRatio, y = Height)) + geom_point() + geom_line() + ggtitle("DBH Size Ratio VS Average Tree Height (m)")

#s2 <- get_match(data, ObjectID, 2)
#s3 <- get_match(data, ObjectID, 3)
#s4 <- get_match(data, ObjectID, 4)

#make_s_graph(s2, "R2")
#make_s_graph(s3, "R3")
#make_s_graph(s4, "R4")

#r2Top10 <- top_frac(r2, 0.1, Score)
#r3Top10 <- top_frac(r3, 0.1, Score)
#r4Top10 <- top_frac(r4, 0.1, Score)

#tHeight <- c(mean(r2Top10$THEIGHT), mean(r3Top10$THEIGHT), mean(r4Top10$THEIGHT))
#df <- data.frame(dbhMultipliers, tHeight)

#ggplot(data = df, aes(x = dbhMultiplier, y = tHeight)) + geom_point() + geom_line() + ggtitle("Tree Height (m) VS DBH Multiplier")