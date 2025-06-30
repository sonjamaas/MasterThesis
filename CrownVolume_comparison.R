############ Script for Comparing Crown Volume from TLS & BP Data ##############

#### 1. descriptive statistics
#### 2. Paired comparison test


################################################################################

library(ggplot2)
library(tidyr)

setwd("E:/Sonja/Msc_Thesis/data/Metrics/")

bp <- read.csv2("bp/Tree_segmentation_LiDAR360/bp_feb5.csv")
tls <- read.csv("tls/tree_segmentation_LiDAR360/tls_winter.csv")

bp[,1:8] <- NULL
bp$CrownVolume <- as.numeric(bp$CrownVolume)

tls[,3:8] <- NULL
tls[,1] <- NULL

data <- merge(bp, tls, by.x = "NewID", by.y = "TreeID")
colnames(data) <- c("TreeID", "CrownVolume_BP", "CrownVolume_TLS")

data_long <- pivot_longer(data, cols = c("CrownVolume_BP", "CrownVolume_TLS"), names_to = "Source", values_to = "CrownVolume")


#### 1. Descriptive statistics


## Mean
bp_mean <- mean(na.omit(data$CrownVolume_BP))
# [1] 325.1399
tls_mean <- mean(data$CrownVolume_TLS)
# [1] 439.4049


## Median
bp_median <- median(na.omit(data$CrownVolume_BP))
# [1] 285.825
tls_median <- median(data$CrownVolume_TLS)
# [1] 375.799

## Standard Deviation
bp_sd <- sd(na.omit(data$CrownVolume_BP))
# [1] 281.5164
tls_sd <- sd(data$CrownVolume_TLS)
# [1] 289.551

## Range
bp_range <- range(na.omit(data$CrownVolume_BP))
# [1] 1.160 1885.531
tls_range <- range(data$CrownVolume_TLS)
# [1] 0.591 1323.592

## Viz
ggplot(data_long, aes(x = Source, y = CrownVolume))+
  geom_jitter(color = "grey",
              alpha = 0.7,
              size = 3)+
  geom_boxplot(color = "darkolivegreen",
               fill = "darkolivegreen4",
               alpha = 0.3,
               notch = TRUE,
               notchwidth = 0.8,
               outlier.colour="red",
               outlier.fill="red",
               outlier.size=4)+
  labs(title = "Crown Volume measurement Comparison")+
  xlab("") +
  ylab("Crown Volume [m³]") +
  theme_minimal()+
  theme(plot.title = element_text(size = 15))
# export in 6.28 6.4, cubes quadratic



#### 2. Paired Comparison Tests

differences <- data$CrownVolume_BP - data$CrownVolume_TLS



# Histogram
hist(differences, main = "Histogram of Differences in Crown Volume Measurements (Backpack & TLS Data)", xlab = "Difference")


# QQ plot
qqnorm(differences)
qqline(differences)

t.test(data$CrownVolume_BP, data$CrownVolume_TLS, paired = TRUE)


#### 3. Correlation & Regression

lm <- lm(data$CrownVolume_BP ~ data$CrownVolume_TLS)

summary.lm(lm)

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = CrownVolume_TLS, y = CrownVolume_BP), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Crown Volume Backpack") +
  xlab("Crown Volume TLS")+
  xlim(0, 2000)+
  ylim(0, 2000)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  annotate("text", x = 2000, y = 250, label = "p-value = 0.000\nmean difference = 114.270", hjust = "right")

lay2 <- matrix(1:1, nrow = 1, ncol = 1, byrow = TRUE)
# library(grid)
grid.arrange(a,
             layout_matrix = lay2, widths = c(1), heights = c(1)
             ,top = textGrob("Comparison of Crown Volume measurements [m³]", gp=gpar(fontsize =15))
)
a>s# export in 6.28 6.4, cubes quadratic