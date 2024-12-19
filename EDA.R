# Import package and customize function
{
    library(dplyr)
    library(corrplot)
    
    source("utils_eda.R")
}

# Load data
data_raw     <- read.csv("high_diamond_ranked_10min.csv", encoding = "UTF-8")
# column_table <- read.csv("column_table.csv", encoding = "UTF-8")

# checkpoint: missing data
stopifnot(sum(is.na(data_raw) == TRUE) == 0)
stopifnot(sum(is.nan(as.matrix(data_raw)) == TRUE) == 0)

# checkpoint: duplicated game
stopifnot(length(unique(data_raw$gameId)) == nrow(data_raw))

# remove the column `gameId`
data_noID <- data_raw[, -1]

# split red team data and blue team data
# remove the column `CSpermin` and `Goldpermin`
data_blue_team <- data_noID[, grep("blue", names(data_noID))] %>% .[, -c(19, 20)]
data_red_team  <- data_noID[, grep("red", names(data_noID))] %>% .[, -c(18, 19)]

# EDA: blueWins
blueWins_Figure(column = data_noID$blueWins, save_fig = TRUE)

# EDA: WardsPlaced
blueWardsPlaced_Figure(data = data_blue_team, save_fig = TRUE)
summary(data_blue_team$blueWardsPlaced)

redWardsPlaced_Figure(data = data_red_team, save_fig = TRUE)
summary(data_red_team$redWardsPlaced)

WardsPlaced_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# EDA: WardsDestroyed
blueWardsDestroyed_Figure(data = data_blue_team, save_fig = TRUE)
summary(data_blue_team$blueWardsDestroyed)

redWardsDestroyed_Figure(data = data_red_team, save_fig = TRUE)
summary(data_red_team$redWardsDestroyed)

WardsDestroyed_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# checkpoint: unmatched data in column `FirstBlood`
stopifnot(
    (table(data_blue_team$blueFirstBlood)[0] == table(data_red_team$redFirstBlood)[1]) & 
    (table(data_blue_team$blueFirstBlood)[1] == table(data_red_team$redFirstBlood)[0])
)

# EDA: FirstBlood
FirstBlood_Figure(column = data_noID$blueFirstBlood, save_fig = TRUE)

# EDA: KDA
KDA_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# EDA: Elite Monsters / Dragons / Heralds
EliteMonsters_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)
Dragons_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)
Heralds_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# EDA: TowersDestroyed
TowersDestroyed_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# EDA: TotalGold
TotalGold_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)
TotalGold_BoxFigure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# EDA: AvgLevel
AvgLevel_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)
AvgLevel_BoxFigure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# EDA: TotalExperience
TotalExperience_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)
TotalExperience_BoxFigure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# EDA: TotalMinionsKilled
TotalMinionsKilled_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)
TotalMinionsKilled_BoxFigure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# EDA: TotalJungleMinionsKilled
TotalJungleMinionsKilled_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)
TotalJungleMinionsKilled_BoxFigure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# EDA: GoldDiff
GoldDiff_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)
GoldDiff_BoxFigure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# EDA: ExperienceDiff
ExperienceDiff_Figure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)
ExperienceDiff_BoxFigure(data1 = data_blue_team, data2 = data_red_team, save_fig = TRUE)

# EDA: heatmap of correlated coefficient
corrplot(cor(data_noID), method = "color", tl.col = "black", tl.srt = 45, tl.cex = 0.7)

png(filename = "figure/WardsPlaced_density.png", width = 1200, height = 800)
density_KS_Figure(data1 = data_blue_team$blueWardsPlaced, data2 = data_red_team$redWardsPlaced, 
                  name = "WardsPlaced", label_x_poi = 150, label_y_poi = 0.5)
dev.off()

png(filename = "figure/WardsDestroyed_density.png", width = 1200, height = 800)
density_KS_Figure(data1 = data_blue_team$blueWardsDestroyed, 
                  data2 = data_red_team$redWardsDestroyed, 
                  name = "WardsDestroyed", label_x_poi = 20, label_y_poi = 0.5)
dev.off()

png(filename = "figure/TotalGold_density.png", width = 1200, height = 800)
density_KS_Figure(data1 = data_blue_team$blueTotalGold, data2 = data_red_team$redTotalGold, 
                  name = "TotalGold", label_x_poi = 22000, label_y_poi = 0.5)
dev.off()

png(filename = "figure/AvgLevel_density.png", width = 1200, height = 800)
density_KS_Figure(data1 = data_blue_team$blueAvgLevel, data2 = data_red_team$redAvgLevel, 
                  name = "AvgLevel", label_x_poi = 5, label_y_poi = 0.5)
dev.off()

png(filename = "figure/TotalExperience_density.png", width = 1200, height = 800)
density_KS_Figure(data1 = data_blue_team$blueTotalExperience, 
                  data2 = data_red_team$redTotalExperience, 
                  name = "TotalExperience", label_x_poi = 11000, label_y_poi = 0.5)
dev.off()

png(filename = "figure/TotalMinionsKilled_density.png", width = 1200, height = 800)
density_KS_Figure(data1 = data_blue_team$blueTotalMinionsKilled, 
                  data2 = data_red_team$redTotalMinionsKilled, 
                  name = "TotalMinionsKilled", label_x_poi = 125, label_y_poi = 0.5)
dev.off()

png(filename = "figure/TotalJungleMinionsKilled_density.png", width = 1200, height = 800)
density_KS_Figure(data1 = data_blue_team$blueTotalJungleMinionsKilled, 
                  data2 = data_red_team$redTotalJungleMinionsKilled, 
                  name = "TotalJungleMinionsKilled", label_x_poi = 10, label_y_poi = 0.5)
dev.off()

png(filename = "figure/GoldDiff_density.png", width = 1200, height = 800)
density_KS_Figure(data1 = data_blue_team$blueGoldDiff, 
                  data2 = data_red_team$redGoldDiff, 
                  name = "GoldDiff", label_x_poi = 7000, label_y_poi = 0.5)
dev.off()

png(filename = "figure/ExperienceDiff_density.png", width = 1200, height = 800)
density_KS_Figure(data1 = data_blue_team$blueExperienceDiff, 
                  data2 = data_red_team$redExperienceDiff, 
                  name = "ExperienceDiff", label_x_poi = 5000, label_y_poi = 0.5)
dev.off()