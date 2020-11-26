library(tidyverse)
library(rpart)
library(rpart.plot)
library(lattice)
library(caret)
library(e1071)
library(anytime)

# Read data 'games.features.csv'
gameData <- read.csv("games-features.csv")


# Getting rid of duplicated data
gameData <- unique(gameData)

# Getting rid of NAs
gameData <- na.omit(gameData)

#Find the number of data that is more than mean of SteamSpyPlayersEstimate
gameData %>% filter(SteamSpyPlayersEstimate > floor(mean(gameData$SteamSpyPlayersEstimate))) %>%
  nrow() -> nFamous
nFamous

#Find the probability
nFamous/nrow(gameData)

#Mutate the "famous" column that have 2 values.
#If that row has SteamSpyPlayersEstimate more than mean, a state is "yes"
#Otherwise, a state is "no"
gameData %>% mutate(famous = ifelse(SteamSpyPlayersEstimate > mean(SteamSpyPlayersEstimate),
                                     "yes", "no")) -> gameData1

#Separate "ReleaseDate" column into month, day, and year
gameData1 %>% separate(ReleaseDate, c("month", "day","year")) -> gameData2

gameData2$year <- as.numeric(gameData2$year)
gameData2$day <- as.numeric(gameData2$day)

#Filter data that year is between 1997 and 2016
gameData2 %>% filter(year >=1997 & year <= 2016) -> gameData3

gameData3 %>% group_by(year) %>% count()

#Filter data that has year in form starting 3 letters of month
gameData3 %>% filter(month == "Jan" | month == "Feb" | month == "Mar" |
                     month == "Apr" | month == "May" | month == "Jun" |
                     month == "Jul" | month == "Aug" | month == "Sep" |
                     month == "Oct" | month == "Nov" | month == "Dec") -> gameData4

#Filter data that is day between 0 and 32
gameData4 %>% filter(day > 0 & day < 32) -> gameData5

#Select variables that we don't want
gameData5 %>% select(RequiredAge, DemoCount, DLCCount, Metacritic,
                     PackageCount, IsFree, PlatformWindows, PlatformLinux,
                     PlatformMac, CategorySinglePlayer, CategoryMultiplayer, CategoryCoop,
                     CategoryMMO, CategoryIncludeLevelEditor, CategoryVRSupport, CategoryInAppPurchase,
                     GenreIsIndie, GenreIsAction, GenreIsAdventure, GenreIsCasual, GenreIsStrategy,
                     GenreIsRPG, GenreIsSimulation, GenreIsEarlyAccess, GenreIsFreeToPlay,
                     GenreIsSports, GenreIsRacing, GenreIsMassivelyMultiplayer, famous) ->
                     gameDataFinal

set.seed(555)

test_ind <- sample(nrow(gameDataFinal), 0.3*nrow(gameDataFinal))
gameData_training <- gameDataFinal[-test_ind,]
gameData_testing <- gameDataFinal[test_ind,]

tree <- rpart(famous~RequiredAge + DemoCount + DLCCount + Metacritic +
                PackageCount + IsFree + PlatformWindows +
                PlatformLinux + PlatformMac + CategorySinglePlayer + CategoryMultiplayer +
                CategoryCoop + CategoryMMO + CategoryIncludeLevelEditor + CategoryVRSupport +
                CategoryInAppPurchase + GenreIsIndie + GenreIsAction + GenreIsAdventure + GenreIsCasual +
                GenreIsStrategy + GenreIsRPG + GenreIsSimulation + GenreIsEarlyAccess +
                GenreIsFreeToPlay + GenreIsSports + GenreIsRacing + GenreIsMassivelyMultiplayer,
              data = gameData_training)

rpart.plot(tree)

tree$variable.importance

res <- predict(tree, gameData_testing, type = "class")

confusionMatrix(res, factor(gameData_testing$famous), positive = "no", mode = "prec_recall")

#Separate type of game in each year
gameData5 %>% select(year, GenreIsIndie, GenreIsAction, GenreIsAdventure, GenreIsCasual, 
                           GenreIsStrategy, GenreIsRPG, GenreIsSimulation, GenreIsEarlyAccess,
                           GenreIsFreeToPlay, GenreIsSports, GenreIsRacing,
                           GenreIsMassivelyMultiplayer) -> gameDataGenre

gameDataGenre %>% gather("Genre", "Status", 2:13) %>% group_by(year, Genre, Status) %>% 
  summarise(n = n()) %>% filter(Status == "True") %>% ggplot() +
  geom_col(aes(x = Genre, y = n, fill = factor(year))) + theme_bw() +
  facet_wrap(~year,scales = "free_x") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.x = element_text(size = 5))

gameDataGenre %>% gather("Genre", "Status", 2:13) %>% group_by(year, Genre, Status) %>% 
  summarise(n = n()) %>% filter(Status == "True") %>% ggplot() +
  geom_col(aes(x = year, y = n, fill= Genre)) + theme_bw() + facet_wrap(~Genre,scales = "free_x")


#Top 10 games that has the most RecommendationCount
gameData %>% top_n(20, RecommendationCount) %>%
             arrange(desc(RecommendationCount)) -> TopRecommend
TopRecommend %>% select(QueryName, RecommendationCount)

#Top 10 games that has the most number of players
gameData %>% top_n(10, SteamSpyPlayersEstimate) %>%
    arrange(desc(SteamSpyPlayersEstimate)) -> TopSteamPlayer
TopSteamPlayer %>% select(QueryName, SteamSpyPlayersEstimate)

#2 Platform
tree <- rpart(famous~PlatformWindows + PlatformLinux + PlatformMac,
              data = gameData_training)

#3 Category
tree <- rpart(famous~CategorySinglePlayer + CategoryMultiplayer + CategoryInAppPurchase +
                CategoryCoop + CategoryMMO + CategoryIncludeLevelEditor + CategoryVRSupport,
              data = gameData_training)

#4 General
tree <- rpart(famous~
                GenreIsIndie + GenreIsAction + GenreIsAdventure + GenreIsCasual +
                GenreIsStrategy + GenreIsRPG + GenreIsSimulation + GenreIsEarlyAccess +
                GenreIsFreeToPlay + GenreIsSports + GenreIsRacing + GenreIsMassivelyMultiplayer,
              data = gameData_training)

#5 No category, general, and platform
tree <- rpart(famous~RequiredAge + DemoCount + DLCCount + Metacritic +
                PackageCount + IsFree,
              data = gameData_training)

#6
tree <- rpart(famous~RequiredAge + DemoCount + DLCCount + Metacritic +
                PackageCount + IsFree +
                GenreIsIndie + GenreIsAction + GenreIsAdventure + GenreIsCasual +
                GenreIsStrategy + GenreIsRPG + GenreIsSimulation + GenreIsEarlyAccess +
                GenreIsFreeToPlay + GenreIsSports + GenreIsRacing + GenreIsMassivelyMultiplayer,
              data = gameData_training)

