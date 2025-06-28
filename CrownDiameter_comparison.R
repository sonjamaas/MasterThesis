######### Script for Comparing Crown Area from TLS & BP & UAV Data ###########

#### 1. descriptive statistics
#### 2. Paired comparison test


################################################################################

library(ggplot2)
library(tidyr)

setwd("E:/Sonja/Msc_Thesis/data/Metrics/")

bp <- read.csv2("bp/Tree_segmentation_LiDAR360/bp_feb5.csv")
tls <- read.csv("tls/tree_segmentation_LiDAR360/tls_winter.csv")
uav <- read.csv2("E:/Sonja/Msc_Thesis/data/8_preprocessedData/uav/UAV_feb2_shifted_clipped_CHM_CHM Segmentation.csv")

bp[,1:6] <- NULL
bp[,2:3] <- NULL
bp$CrownDiameter <- as.numeric(bp$CrownDiameter)

tls[,3:6] <- NULL
tls[,1] <- NULL
tls[,3:4] <- NULL

uav[,1:4] <- NULL
uav[,2] <- NULL
uav$CrownDiameter <- as.numeric(uav$CrownDiameter)

data <- merge(bp, tls, by.x = "NewID", by.y = "TreeID")
data <- merge(data, uav, by.x = "NewID", by.y = "NewID")
colnames(data) <- c("TreeID", "CrownDiameter_BP", "CrownDiameter_TLS", "CrownDiameter_UAV")

data_long <- pivot_longer(data, cols = c("CrownDiameter_BP", "CrownDiameter_TLS", "CrownDiameter_UAV"), names_to = "Source", values_to = "CrownDiameter")


#### 1. Descriptive statistics


## Mean
bp_mean <- mean(na.omit(data$CrownDiameter_BP))
# [1] 8.389
tls_mean <- mean(data$CrownDiameter_TLS)
# [1] 9.368108
uav_mean <- mean(na.omit(data$CrownDiameter_UAV))
# 7.855554


## Median
bp_median <- median(na.omit(data$CrownDiameter_BP))
# [1] 8.525
tls_median <- median(data$CrownDiameter_TLS)
# [1] 9
uav_median <- median(na.omit(data$CrownDiameter_UAV))
# [1] 7.375

## Standard Deviation
bp_sd <- sd(na.omit(data$CrownDiameter_BP))
# [1] 2.6047
tls_sd <- sd(data$CrownDiameter_TLS)
# [1] 2.300927
uav_sd <- sd(na.omit(data$CrownDiameter_UAV))
# [1] 2.881721

## Range
bp_range <- range(na.omit(data$CrownDiameter_BP))
# [1] 1.192 16.185
tls_range <- range(data$CrownDiameter_TLS)
# [1] 3.712 14.233
uav_range <- range(na.omit(data$CrownDiameter_UAV))
# [1] 1.354 16.205

## Viz
ggplot(data_long, aes(x = Source, y = CrownDiameter))+
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
  labs(title = "Crown Diameter measurement Comparison")



#### 2. Paired Comparison Tests

differences <- data$CrownDiameter_BP - data$CrownDiameter_TLS
differences2 <- data$CrownDiameter_BP - data$CrownDiameter_UAV
differences5 <- data$CrownDiameter_UAV - data$CrownDiameter_TLS



# Histogram
hist(differences, main = "Histogram of Differences in Height Measurements (Backpack & TLS Data)", xlab = "Difference")
hist(differences2, main = "Histogram of Differences in Height Measurements (Backpack & UAV Data)", xlab = "Difference")
hist(differences5, main = "Histogram of Differences in Height Measurements (UAV & TLS Data)", xlab = "Difference")


# QQ plot
qqnorm(differences)
qqline(differences)

qqnorm(differences2)
qqline(differences2)

qqnorm(differences5)
qqline(differences5)

t.test(data$CrownDiameter_BP, data$CrownDiameter_TLS, paired = TRUE)
t.test(data$CrownDiameter_BP, data$CrownDiameter_UAV, paired = TRUE)
t.test(data$CrownDiameter_UAV, data$CrownDiameter_TLS, paired = TRUE)



#### 3. Correlation & Regression

lm <- lm(data$CrownDiameter_BP ~ data$CrownDiameter_TLS)
lm <- lm(data$CrownDiameter_BP ~ data$CrownDiameter_UAV)
lm <- lm(data$CrownDiameter_UAV ~ data$CrownDiameter_TLS)

summary.lm(lm)


#### 4. pairwise scatterplots

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = CrownDiameter_TLS, y = CrownDiameter_BP), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Crown Diameter Backpack") +
  xlab("")+
  xlim(0, 17.5)+
  ylim(0, 17.5)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = CrownDiameter_TLS, y = CrownDiameter_UAV), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Crown Diameter UAV") +
  xlab("Crown Diameter TLS")+
  xlim(0, 17.5)+
  ylim(0, 17.5)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"))

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = CrownDiameter_BP, y = CrownDiameter_UAV), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("Crown Diameter Backpack")+
  xlim(0, 17.5)+
  ylim(0, 17.5)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"))

lay2 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
# library(grid)
grid.arrange(a, NULL, b, c,
             layout_matrix = lay2, widths = c(1,0.9), heights = c(1,1)
             ,top = textGrob("Pairwise Comparison of Crown Diameter measurements [m]", gp=gpar(fontsize =15))
)
# export in 6.28 6.4, cubes quadratic