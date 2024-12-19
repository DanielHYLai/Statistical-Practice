library(tree)
library(latex2exp)
library(ggplot2)

result <- as.integer(test_pred_XGB == test_data_significant$blueWins)
test_data_XGB <- test_data_significant
test_data_XGB$blueWins <- result
names(test_data_XGB)[ncol(test_data_XGB)] <- "XGB_vs_True"

analysis_XGB <- tree::tree(as.factor(XGB_vs_True) ~ ., data = test_data_XGB)
plot(analysis_XGB, uniform = TRUE, margin = 0.1)
text(analysis_XGB, use.n = TRUE, all = TRUE, cex = 0.8)

# corrplot(cor(test_data_XGB), method = "color", tl.col = "black", tl.srt = 45, tl.cex = 0.7)

analysis_Figure <- function(data1 = test_data_XGB[test_data_XGB$XGB_vs_True == 1, ], 
                            data2 = test_data_XGB[test_data_XGB$XGB_vs_True == 0, ], 
                            feature, binwidth, 
                            save_fig = FALSE) {
    data.frame(
        type  = c(rep("Correct", nrow(data1)), rep("False", nrow(data2))), 
        value = c(data1[, feature], data2[, feature])
    ) %>% 
        ggplot2::ggplot(., aes(x = value, fill = type)) + 
        geom_histogram(binwidth = binwidth, alpha = 0.7, position = "identity") + 
        scale_fill_manual(values = c("#84C1FF", "#FF9797")) + 
        ggtitle(feature) + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) -> fig
    
    if (save_fig == TRUE) {
        ggplot2::ggsave("./figure/TotalMinionsKilled_hist.png", plot = fig, width = 8, height = 6)
        cat("Save figure successfully.\n")
    }
    
    return(fig)
}
binwidth <- c(1, 1, 10, 100, 100, 1, 10)
idx <- 1
for (feature in names(test_data_XGB)[-8]) {
    print(analysis_Figure(data1 = test_data_XGB[test_data_XGB$XGB_vs_True == 1, ], 
                    data2 = test_data_XGB[test_data_XGB$XGB_vs_True == 0, ], 
                    feature = feature, binwidth = binwidth[idx]))
    idx <- idx + 1
}

cut_idx <- c(500, 450)
idx <- 1
for (feature in names(test_data_XGB)[c(5, 4)]) {
    cat(feature, "\n")
    interval_count <- seq(summary(test_data_XGB[, feature])[1], 
                          summary(test_data_XGB[, feature])[6], 
                          by = cut_idx[idx])
    end_idx <- 2
    idx <- idx + 1
    result <- c()
    for (start_idx in 1:(length(interval_count) - 1)) {
        nrow(test_data_XGB[((interval_count[start_idx] <= test_data_XGB[, feature]) & 
                                (test_data_XGB[, feature] <= interval_count[end_idx])) & 
                               test_data_XGB$XGB_vs_True == 1, ]) -> correct_data
        nrow(test_data_XGB[((interval_count[start_idx] <= test_data_XGB[, feature]) & 
                                (test_data_XGB[, feature] <= interval_count[end_idx])) & 
                               test_data_XGB$XGB_vs_True == 0, ]) -> false_data
        result <- c(result, false_data / (correct_data + false_data))
        end_idx <- end_idx + 1
    }
    result <- data.frame(result = result, 
                         interval = interval_count[c(1:(length(interval_count) - 1))]) %>% 
        na.omit(.)
    
    result[result$result >= mean(result$result) + 1.28 * sd(result$result), "interval"] %>% 
        print(.)
    
    result$error_upper <- mean(result$result) + 1.28 * sd(result$result)
    
    result$subinterval <- factor(result$interval, levels = result$interval)
    
    # 繪圖
    fig <- ggplot(result, aes(x = subinterval, y = result)) + 
        geom_point(color = "#84C1FF", linewidth = 2) + 
        geom_line(aes(group = 1), color = "#84C1FF", size = 1) + 
        geom_hline(aes(yintercept = error_upper), color = "red", linetype = "dashed", size = 1) + 
        annotate("text", x = 1, y = mean(result$result) + 1.28 * sd(result$result) + 0.025,
                 label = TeX("$\\bar{X} + z_{0.1}\\times\\frac{\\hat{\\sigma}}{\\sqrt{n}}$"),
                 parse = TRUE, color = "red", hjust = -0.1) + 
        labs(x = "subinterval", y = "error rate", title = paste0("Error Rate of ", feature)) +  
        theme_minimal() + 
        theme(
            legend.position = "none", 
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            axis.title = element_text(size = 16), 
            axis.text = element_text(size = 14)
        ) +
        scale_x_discrete(breaks = result$subinterval[seq(1, length(result$subinterval), by = 2)])
    print(fig)
    
}