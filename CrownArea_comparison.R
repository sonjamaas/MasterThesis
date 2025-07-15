######### Script for Comparing Crown Area from TLS & BP & UAV Data ###########

#### 1. descriptive statistics
#### 2. Paired comparison test


################################################################################

library(ggplot2)
library(tidyr)
library(grid)
library(gridExtra)

setwd("E:/Sonja/Msc_Thesis/data/Metrics/")

bp <- read.csv2("bp/Tree_segmentation_LiDAR360/bp_feb5.csv")
tls <- read.csv("tls/tree_segmentation_LiDAR360/tls_winter.csv")
uav <- read.csv2("E:/Sonja/Msc_Thesis/data/8_preprocessedData/uav/UAV_feb2_shifted_clipped_CHM_CHM Segmentation.csv")

bp[,1:7] <- NULL
bp[,2] <- NULL
bp$CrownArea <- as.numeric(bp$CrownArea)

tls[,3:7] <- NULL
tls[,1] <- NULL
tls[,3] <- NULL

uav[,1:5] <- NULL
uav$CrownArea <- as.numeric(uav$CrownArea)

data <- merge(bp, tls, by.x = "NewID", by.y = "TreeID")
data <- merge(data, uav, by.x = "NewID", by.y = "NewID")
colnames(data) <- c("TreeID", "CrownArea_BP", "CrownArea_TLS", "CrownArea_UAV")
data$CrownArea_UAV <- as.numeric(data$CrownArea_UAV)

data_long <- pivot_longer(data, cols = c("CrownArea_BP", "CrownArea_TLS", "CrownArea_UAV"), names_to = "Source", values_to = "CrownArea")


#### 1. Descriptive statistics


## Mean
bp_CA_mean <- mean(na.omit(data$CrownArea_BP))
# [1] 60.51905
tls_CA_mean <- mean(data$CrownArea_TLS)
# [1] 73.02245
uav_CA_mean <- mean(na.omit(data$CrownArea_UAV))
# 54.88923


## Median
bp_median <- median(na.omit(data$CrownArea_BP))
# [1] 57.083
tls_median <- median(data$CrownArea_TLS)
# [1] 63.619
uav_median <- median(na.omit(data$CrownArea_UAV))
# [1] 42.72

## Standard Deviation
bp_sd <- sd(na.omit(data$CrownArea_BP))
# [1] 35.21666
tls_sd <- sd(data$CrownArea_TLS)
# [1] 
uav_sd <- sd(na.omit(data$CrownArea_UAV))
# [1] 40.49777

## Range
bp_range <- range(na.omit(data$CrownArea_BP))
# [1] 1.116 205.747
tls_range <- range(data$CrownArea_TLS)
# [1] 10.824 159.108
uav_range <- range(na.omit(data$CrownArea_UAV))
# [1] 

## Viz
ggplot(data_long, aes(x = Source, y = CrownArea))+
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
  #labs(title = "Crown Area measurement Comparison")+
  xlab("") +
  ylab("Crown Area [m²]") +
  theme_minimal()+
  theme(plot.title = element_text(size = 15))
# export in 6.28 6.4, cubes quadratic



#### 2. Paired Comparison Tests

differences <- data$CrownArea_BP - data$CrownArea_TLS
differences2 <- data$CrownArea_BP - data$CrownArea_UAV
differences5 <- data$CrownArea_UAV - data$CrownArea_TLS



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

t.test(data$CrownArea_BP, data$CrownArea_TLS, paired = TRUE)
t.test(data$CrownArea_BP, data$CrownArea_UAV, paired = TRUE)
t.test(data$CrownArea_UAV, data$CrownArea_TLS, paired = TRUE)



#### 3. Correlation & Regression

lm <- lm(data$CrownArea_BP ~ data$CrownArea_TLS)
lm <- lm(data$CrownArea_BP ~ data$CrownArea_UAV)
lm <- lm(data$CrownArea_UAV ~ data$CrownArea_TLS)

summary.lm(lm)


#### 4. paired scatterplots

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = CrownArea_TLS, y = CrownArea_BP), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Crown Area Backpack") +
  xlab("")+
  xlim(0, 210)+
  ylim(0, 210)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))+
  annotate("text", x = 200, y = 25, label = "p-value = 0.000\nmean difference = 12.503", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = CrownArea_TLS, y = CrownArea_UAV), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Crown Area UAV") +
  xlab("Crown Area TLS")+
  xlim(0, 210)+
  ylim(0, 210)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 200, y = 25, label = "p-value = 0.000\nmean difference = 18.133", hjust = "right")

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = CrownArea_BP, y = CrownArea_UAV), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("Crown Area Backpack")+
  xlim(0, 210)+
  ylim(0, 210)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"))+
  annotate("text", x = 200, y = 25, label = "p-value = 0.327\nmean difference = 5.630", hjust = "right")

lay2 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)

grid.arrange(a, NULL, b, c,
             layout_matrix = lay2, widths = c(1,0.9), heights = c(1,1)
             # ,top = textGrob("Pairwise Comparison of Crown Area measurements [m²]", gp=gpar(fontsize =15))
)
# export in 6.28 6.4, cubes quadratic

layout <- "
A#
BC
"
combined <- (
  a + b + c +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right")
)
