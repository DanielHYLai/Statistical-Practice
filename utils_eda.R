source("requirements.R")
{
    library(ggplot2)
    library(hrbrthemes)
}

blueWins_Figure <- function(column = data_noID$blueWins, save_fig = FALSE) {
    data_pie <- data.frame(
        team  = c("Blue", "Red"), 
        count = c(length(which(column == 1)), length(which(column == 0)))
    )
    data_pie$fraction <- data_pie$count / sum(data_pie$count)
    data_pie$ymax <- cumsum(data_pie$fraction)
    data_pie$ymin <- c(0, head(data_pie$ymax, n = -1))
    data_pie$labelPosition <- (data_pie$ymax + data_pie$ymin) / 2 + 0.05
    data_pie$label <- paste0(data_pie$team, "\n", round(data_pie$count / 9879, 4))
    
    ggplot2::ggplot(data_pie, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = team)) + 
        geom_rect() + 
        geom_text(x = 1, aes(y = labelPosition, label = label, color = team), size = 6) + 
        scale_fill_manual(values = c("#84C1FF", "#FF9797")) + 
        scale_color_manual(values = c("#84C1FF", "#FF9797")) + 
        coord_polar(theta = "y") + 
        xlim(c(-1, 4)) + 
        ggtitle("blueWins") + 
        theme_void() + 
        theme(legend.position = "none", 
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/blueWins_pie.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


blueWardsPlaced_Figure <- function(data = data_blue_team, save_fig = FALSE) {
    
    ggplot2::ggplot(data, aes(x = blueWardsPlaced)) + 
        geom_histogram(bins = max(data$blueWardsPlaced), fill = "#84C1FF", color = "black") + 
        labs(x = "value", y = "counts", title = "WardsPlaced") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/blueWardsPlaced_hist.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


redWardsPlaced_Figure <- function(data = data_red_team, save_fig = FALSE) {
    ggplot2::ggplot(data, aes(x = redWardsPlaced)) + 
        geom_histogram(bins = max(data$redWardsPlaced), fill = "#FF9797", color = "black") + 
        labs(x = "value", y = "counts", title = "WardsPlaced") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/redWardsPlaced_hist.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


WardsPlaced_Figure <- function(data1 = data_blue_team, data2 = data_red_team, save_fig = FALSE) {
    data.frame(
        value = c(data1$blueWardsPlaced, 
                  data2$redWardsPlaced), 
        type  = c(rep("blue", nrow(data1)), 
                  rep("red", nrow(data2)))
    ) %>% 
        ggplot2::ggplot(., aes(x = type, y = value, fill = "type")) + 
        geom_boxplot(fill = c("#84C1FF", "#FF9797"), color = "black") + 
        ggtitle("WardsPlaced") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/WardsPlaced_box.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


blueWardsDestroyed_Figure <- function(data = data_blue_team, save_fig = FALSE) {
    
    ggplot2::ggplot(data, aes(x = blueWardsDestroyed)) + 
        geom_histogram(bins = max(data$blueWardsDestroyed), fill = "#84C1FF", color = "black") + 
        labs(x = "value", y = "counts", title = "WardsDestroyed") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/blueWardsDestroyed_hist.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


redWardsDestroyed_Figure <- function(data = data_red_team, save_fig = FALSE) {
    ggplot2::ggplot(data, aes(x = redWardsDestroyed)) + 
        geom_histogram(bins = max(data$redWardsDestroyed), fill = "#FF9797", color = "black") + 
        labs(x = "value", y = "counts", title = "WardsDestroyed") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/redWardsDestroyed_hist.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


WardsDestroyed_Figure <- function(data1 = data_blue_team, 
                                  data2 = data_red_team, 
                                  save_fig = FALSE) {
    data.frame(
        value = c(data1$blueWardsDestroyed, 
                  data2$redWardsDestroyed), 
        type  = c(rep("blue", nrow(data1)), 
                  rep("red", nrow(data2)))
    ) %>% 
        ggplot2::ggplot(., aes(x = type, y = value, fill = type)) + 
        geom_boxplot(fill = c("#84C1FF", "#FF9797"), color = "black") + 
        ggtitle("WardsDestroyed") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/WardsDestroyed_box.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


FirstBlood_Figure <- function(column = data_noID$blueFirstBlood, save_fig = FALSE) {
    data_pie <- data.frame(
        team  = c("Blue", "Red"), 
        count = c(length(which(column == 1)), length(which(column == 0)))
    )
    data_pie$fraction <- data_pie$count / sum(data_pie$count)
    data_pie$ymax <- cumsum(data_pie$fraction)
    data_pie$ymin <- c(0, head(data_pie$ymax, n = -1))
    data_pie$labelPosition <- (data_pie$ymax + data_pie$ymin) / 2 + 0.05
    data_pie$label <- paste0(data_pie$team, "\n", round(data_pie$count / 9879, 4))
    
    ggplot2::ggplot(data_pie, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = team)) + 
        geom_rect() + 
        geom_text(x = 1, aes(y = labelPosition, label = label, color = team), size = 6) + 
        scale_fill_manual(values = c("#84C1FF", "#FF9797")) + 
        scale_color_manual(values = c("#84C1FF", "#FF9797")) + 
        coord_polar(theta = "y") + 
        xlim(c(-1, 4)) + 
        ggtitle("FirstBlood") + 
        theme_void() + 
        theme(legend.position = "none", 
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/FirstBlood_pie.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


KDA_Figure <- function(data1 = data_blue_team, data2 = data_red_team, save_fig = FALSE) {
    data.frame(
        KDA = c(rep("Kills", nrow(data1) * 2), 
                rep("Deaths", nrow(data1) * 2), 
                rep("Assists", nrow(data1) * 2)), 
        value = c(data1$blueKills, data2$redKills, 
                  data1$blueDeaths, data2$redDeaths, 
                  data1$blueAssists, data2$redAssists), 
        type = c(rep("blue", nrow(data1)), rep("red", nrow(data2)), 
                 rep("blue", nrow(data1)), rep("red", nrow(data2)), 
                 rep("blue", nrow(data1)), rep("red", nrow(data2)))
    ) %>% 
        ggplot2::ggplot(., aes(x = KDA, y = value, fill = type)) + 
        geom_boxplot() + 
        scale_fill_manual(values=c("#84C1FF", "#FF9797")) +
        ggtitle("KDA") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/KDA_box.png", plot = fig, width = 12, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


EliteMonsters_Figure <- function(data1 = data_blue_team, data2 = data_red_team, save_fig = FALSE) {
    data.frame(
        kill_amount = c(table(data1$blueEliteMonsters)[1], table(data2$redEliteMonsters)[1], 
                        table(data1$blueEliteMonsters)[2], table(data2$redEliteMonsters)[2], 
                        table(data1$blueEliteMonsters)[3], table(data2$redEliteMonsters)[3]), 
        type = rep(c("blue", "red"), 3), 
        kill_num = c(names(table(data1$blueEliteMonsters)[1]), 
                     names(table(data2$redEliteMonsters)[1]), 
                     names(table(data1$blueEliteMonsters)[2]), 
                     names(table(data2$redEliteMonsters)[2]), 
                     names(table(data1$blueEliteMonsters)[3]), 
                     names(table(data2$redEliteMonsters)[3]))
    ) %>% 
        ggplot2::ggplot(., aes(x = kill_num, y = kill_amount, fill = type)) + 
        geom_bar(position = "dodge", stat = "identity") + 
        scale_fill_manual(values=c("#84C1FF", "#FF9797")) +
        ggtitle("EliteMonsters") + 
        labs(x = "Num", y = "Counts") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/EliteMonsters_bar.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


Dragons_Figure <- function(data1 = data_blue_team, data2 = data_red_team, save_fig = FALSE) {
    data.frame(
        kill_amount = c(table(data1$blueDragons)[1], table(data2$redDragons)[1], 
                        table(data1$blueDragons)[2], table(data2$redDragons)[2]), 
        type = rep(c("blue", "red"), 2), 
        kill_num = c(names(table(data1$blueDragons)[1]), 
                     names(table(data2$redDragons)[1]), 
                     names(table(data1$blueDragons)[2]), 
                     names(table(data2$redDragons)[2]))
    ) %>% 
        ggplot2::ggplot(., aes(x = kill_num, y = kill_amount, fill = type)) + 
        geom_bar(position = "dodge", stat = "identity") + 
        scale_fill_manual(values=c("#84C1FF", "#FF9797")) +
        ggtitle("Dragons") + 
        labs(x = "Num", y = "Counts") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/Dragons_bar.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


Heralds_Figure <- function(data1 = data_blue_team, data2 = data_red_team, save_fig = FALSE) {
    data.frame(
        kill_amount = c(table(data1$blueHeralds)[1], table(data2$redHeralds)[1], 
                        table(data1$blueHeralds)[2], table(data2$redHeralds)[2]), 
        type = rep(c("blue", "red"), 2), 
        kill_num = c(names(table(data1$blueHeralds)[1]), 
                     names(table(data2$redHeralds)[1]), 
                     names(table(data1$blueHeralds)[2]), 
                     names(table(data2$redHeralds)[2]))
    ) %>% 
        ggplot2::ggplot(., aes(x = kill_num, y = kill_amount, fill = type)) + 
        geom_bar(position = "dodge", stat = "identity") + 
        scale_fill_manual(values=c("#84C1FF", "#FF9797")) +
        ggtitle("Heralds") + 
        labs(x = "Num", y = "Counts") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/Heralds_bar.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


TowersDestroyed_Figure <- function(data1 = data_blue_team, 
                                   data2 = data_red_team, 
                                   save_fig = FALSE) {
    data.frame(
        kill_amount = c(table(data1$blueTowersDestroyed)[1], table(data2$redTowersDestroyed)[1], 
                        table(data1$blueTowersDestroyed)[2], table(data2$redTowersDestroyed)[2], 
                        table(data1$blueTowersDestroyed)[3], table(data1$blueTowersDestroyed)[4], 
                        table(data1$blueTowersDestroyed)[5]), 
        type = c(rep(c("blue", "red"), 2), "blue", "blue", "blue"), 
        kill_num = c(names(table(data1$blueTowersDestroyed)[1]), 
                     names(table(data2$redTowersDestroyed)[1]), 
                     names(table(data1$blueTowersDestroyed)[2]), 
                     names(table(data2$redTowersDestroyed)[2]), 
                     names(table(data1$blueTowersDestroyed)[3]), 
                     names(table(data1$blueTowersDestroyed)[4]), 
                     names(table(data1$blueTowersDestroyed)[5]))
    ) %>% 
        ggplot2::ggplot(., aes(x = kill_num, y = kill_amount, fill = type)) + 
        geom_bar(position = "dodge", stat = "identity") + 
        scale_fill_manual(values=c("#84C1FF", "#FF9797")) +
        ggtitle("TowersDestroyed") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/TowersDestroyed_bar.png", plot = fig, width = 12, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


TotalGold_Figure <- function(data1 = data_blue_team, data2 = data_red_team, save_fig = FALSE) {
    data.frame(
        type  = c(rep("blue", nrow(data1)), rep("red", nrow(data2))), 
        value = c(data1$blueTotalGold, data2$redTotalGold)
    ) %>% 
        ggplot2::ggplot(., aes(x = value, fill = type)) + 
        geom_histogram(binwidth = 30, alpha = 0.7, position = "identity") + 
        scale_fill_manual(values = c("#84C1FF", "#FF9797")) + 
        ggtitle("TotalGold") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/TotalGold_hist.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


TotalGold_BoxFigure <- function(data1 = data_blue_team, data2 = data_red_team, save_fig = FALSE) {
    data.frame(
        value = c(data1$blueTotalGold, 
                  data2$redTotalGold), 
        type  = c(rep("blue", nrow(data1)), 
                  rep("red", nrow(data2)))
    ) %>% 
        ggplot2::ggplot(., aes(x = type, y = value, fill = type)) + 
        geom_boxplot(fill = c("#84C1FF", "#FF9797"), color = "black") + 
        ggtitle("TotalGold") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/TotalGold_box.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


AvgLevel_Figure <- function(data1 = data_blue_team, data2 = data_red_team, save_fig = FALSE) {
    data.frame(
        type  = c(rep("blue", nrow(data1)), rep("red", nrow(data2))), 
        value = c(data1$blueAvgLevel, data2$redAvgLevel)
    ) %>% 
        ggplot2::ggplot(., aes(x = value, fill = type)) + 
        geom_bar(alpha = 0.7, position = "identity") + 
        scale_fill_manual(values = c("#84C1FF", "#FF9797")) + 
        ggtitle("AvgLevel") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/AvgLevel_bar.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


AvgLevel_BoxFigure <- function(data1 = data_blue_team, data2 = data_red_team, save_fig = FALSE) {
    data.frame(
        value = c(data1$blueAvgLevel, 
                  data2$redAvgLevel), 
        type  = c(rep("blue", nrow(data1)), 
                  rep("red", nrow(data2)))
    ) %>% 
        ggplot2::ggplot(., aes(x = type, y = value, fill = type)) + 
        geom_boxplot(fill = c("#84C1FF", "#FF9797"), color = "black") + 
        ggtitle("AvgLevel") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/AvgLevel_box.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


TotalExperience_Figure <- function(data1 = data_blue_team, 
                                   data2 = data_red_team, 
                                   save_fig = FALSE) {
    data.frame(
        type  = c(rep("blue", nrow(data1)), rep("red", nrow(data2))), 
        value = c(data1$blueTotalExperience, data2$redTotalExperience)
    ) %>% 
        ggplot2::ggplot(., aes(x = value, fill = type)) + 
        geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") + 
        scale_fill_manual(values = c("#84C1FF", "#FF9797")) + 
        ggtitle("TotalExperience") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/TotalExperience_hist.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


TotalExperience_BoxFigure <- function(data1 = data_blue_team, 
                                      data2 = data_red_team, 
                                      save_fig = FALSE) {
    data.frame(
        value = c(data1$blueTotalExperience, 
                  data2$redTotalExperience), 
        type  = c(rep("blue", nrow(data1)), 
                  rep("red", nrow(data2)))
    ) %>% 
        ggplot2::ggplot(., aes(x = type, y = value, fill = type)) + 
        geom_boxplot(fill = c("#84C1FF", "#FF9797"), color = "black") + 
        ggtitle("TotalExperience") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/TotalExperience_box.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


TotalMinionsKilled_Figure <- function(data1 = data_blue_team, 
                                      data2 = data_red_team, 
                                      save_fig = FALSE) {
    data.frame(
        type  = c(rep("blue", nrow(data1)), rep("red", nrow(data2))), 
        value = c(data1$blueTotalMinionsKilled, data2$redTotalMinionsKilled)
    ) %>% 
        ggplot2::ggplot(., aes(x = value, fill = type)) + 
        geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") + 
        scale_fill_manual(values = c("#84C1FF", "#FF9797")) + 
        ggtitle("TotalMinionsKilled") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/TotalMinionsKilled_hist.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


TotalMinionsKilled_BoxFigure <- function(data1 = data_blue_team, 
                                         data2 = data_red_team, 
                                         save_fig = FALSE) {
    data.frame(
        value = c(data1$blueTotalMinionsKilled, 
                  data2$redTotalMinionsKilled), 
        type  = c(rep("blue", nrow(data1)), 
                  rep("red", nrow(data2)))
    ) %>% 
        ggplot2::ggplot(., aes(x = type, y = value, fill = type)) + 
        geom_boxplot(fill = c("#84C1FF", "#FF9797"), color = "black") + 
        ggtitle("TotalMinionsKilled") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/TotalMinionsKilled_box.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


TotalJungleMinionsKilled_Figure <- function(data1 = data_blue_team, 
                                            data2 = data_red_team, 
                                            save_fig = FALSE) {
    data.frame(
        type  = c(rep("blue", nrow(data1)), rep("red", nrow(data2))), 
        value = c(data1$blueTotalJungleMinionsKilled, data2$redTotalJungleMinionsKilled)
    ) %>% 
        ggplot2::ggplot(., aes(x = value, fill = type)) + 
        geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") + 
        scale_fill_manual(values = c("#84C1FF", "#FF9797")) + 
        ggtitle("TotalJungleMinionsKilled") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/TotalJungleMinionsKilled_hist.png", 
                        plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


TotalJungleMinionsKilled_BoxFigure <- function(data1 = data_blue_team, 
                                               data2 = data_red_team, 
                                               save_fig = FALSE) {
    data.frame(
        value = c(data1$blueTotalJungleMinionsKilled, 
                  data2$redTotalJungleMinionsKilled), 
        type  = c(rep("blue", nrow(data1)), 
                  rep("red", nrow(data2)))
    ) %>% 
        ggplot2::ggplot(., aes(x = type, y = value, fill = type)) + 
        geom_boxplot(fill = c("#84C1FF", "#FF9797"), color = "black") + 
        ggtitle("TotalJungleMinionsKilled") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/TotalJungleMinionsKilled_box.png", 
                        plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


GoldDiff_Figure <- function(data1 = data_blue_team, 
                            data2 = data_red_team, 
                            save_fig = FALSE) {
    data.frame(
        type  = c(rep("blue", nrow(data1)), rep("red", nrow(data2))), 
        value = c(data1$blueGoldDiff, data2$redGoldDiff)
    ) %>% 
        ggplot2::ggplot(., aes(x = value, fill = type)) + 
        geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") + 
        scale_fill_manual(values = c("#84C1FF", "#FF9797")) + 
        ggtitle("GoldDiff") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/GoldDiff_bar.png", 
                        plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


GoldDiff_BoxFigure <- function(data1 = data_blue_team, 
                               data2 = data_red_team, 
                               save_fig = FALSE) {
    data.frame(
        value = c(data1$blueGoldDiff, 
                  data2$redGoldDiff), 
        type  = c(rep("blue", nrow(data1)), 
                  rep("red", nrow(data2)))
    ) %>% 
        ggplot2::ggplot(., aes(x = type, y = value, fill = type)) + 
        geom_boxplot(fill = c("#84C1FF", "#FF9797"), color = "black") + 
        ggtitle("GoldDiff") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/GoldDiff_box.png", 
                        plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


ExperienceDiff_Figure <- function(data1 = data_blue_team, 
                                  data2 = data_red_team, 
                                  save_fig = FALSE) {
    data.frame(
        type  = c(rep("blue", nrow(data1)), rep("red", nrow(data2))), 
        value = c(data1$blueExperienceDiff, data2$redExperienceDiff)
    ) %>% 
        ggplot2::ggplot(., aes(x = value, fill = type)) + 
        geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") + 
        scale_fill_manual(values = c("#84C1FF", "#FF9797")) + 
        ggtitle("ExperienceDiff") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/ExperienceDiff_bar.png", 
                        plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


ExperienceDiff_BoxFigure <- function(data1 = data_blue_team, 
                                     data2 = data_red_team, 
                                     save_fig = FALSE) {
    data.frame(
        value = c(data1$blueExperienceDiff, 
                  data2$redExperienceDiff), 
        type  = c(rep("blue", nrow(data1)), 
                  rep("red", nrow(data2)))
    ) %>% 
        ggplot2::ggplot(., aes(x = type, y = value, fill = type)) + 
        geom_boxplot(fill = c("#84C1FF", "#FF9797"), color = "black") + 
        ggtitle("ExperienceDiff") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/ExperienceDiff_box.png", 
                        plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}


density_KS_Figure <- function(data1, data2, name, label_x_poi = 10, label_y_poi= 0.05) {
    
    plot(stats::ecdf(data1), 
         col = rgb(0.68, 0.85, 0.9, alpha = 0.8), 
         cex = 2, main = name, xlab = "values", ylab = "density", 
         cex.main = 2.5, cex.lab = 1.5, cex.axis = 1.5)
    lines(stats::ecdf(data2), 
          col = rgb(1, 0.75, 0.8, alpha = 0.5), cex = 2)
    
    ks_result <- stats::ks.test(data1, data2)
    ks_text <- paste0("KS test:\nD = ", round(ks_result$statistic, 4), 
                      "\np-value = ", round(ks_result$p.value, 4))
    
    text(x = label_x_poi, y = label_y_poi, labels = ks_text, adj = 0, cex = 2.5)
}
