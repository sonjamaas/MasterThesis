####### Script for Comparing Heights from TLS & BP & Field & UAV Data ##########

#### 1. descriptive statistics
#### 2. Paired comparison test


################################################################################

library(ggplot2)
library(tidyr)
library(grid)

setwd("E:/Sonja/Msc_Thesis/data/Metrics/")

bp <- read.csv2("bp/Tree_segmentation_LiDAR360/bp_feb5.csv")
tls <- read.csv("tls/tree_segmentation_LiDAR360/tls_winter.csv")
field <- read.csv2("allData.csv")
uav <- read.csv2("E:/Sonja/Msc_Thesis/data/8_preprocessedData/uav/UAV_feb2_shifted_clipped_CHM_CHM Segmentation.csv")

field[,4:35] <- NULL
field[,1] <- NULL
# field$fieldData_DBH <- field$fieldData_DBH/pi/100

# for(i in 1:nrow(field)){
#   if(is.na(field$fieldData_Height[i])){
#     field$NewTreeID[i] <- NA
#   }
# }

bp[,6:9] <- NULL
bp[,1:4] <- NULL
bp$TreeHeight <- as.numeric(bp$TreeHeight)

tls[,6:10] <- NULL
tls[,1] <- NULL
tls[,2:3] <- NULL

uav[,1:3] <- NULL
uav[,2:3] <- NULL
uav$TreeHeight <- as.numeric(uav$TreeHeight)

data <- merge(bp, tls, by.x = "NewID", by.y = "TreeID")
data <- merge(data, field, by.x = "NewID", by.y = "NewTreeID" )
data <- merge(data, uav, by.x = "NewID", by.y = "NewID")
colnames(data) <- c("TreeID", "Height_BP", "Height_TLS", "Height_FieldData", "Height_UAV")
data$Height_FieldData <- as.numeric(data$Height_FieldData)



data_long <- pivot_longer(data, cols = c("Height_BP", "Height_TLS", "Height_FieldData", "Height_UAV"), names_to = "Source", values_to = "Height")


#### 1. Descriptive statistics


## Mean
bp_height_mean <- mean(na.omit(data$Height_BP))
# [1] 32.99812
tls_height_mean <- mean(data$Height_TLS)
# [1] 35.354
field_height_mean <- mean(na.omit(data$Height_FieldData))
# 31.68571
uav_height_mean <- mean(na.omit(data$Height_UAV))
# 35.3948


## Median
bp_median <- median(na.omit(data$Height_BP))
# [1] 34.093
tls_median <- median(data$Height_TLS)
# [1] 35.539
field_median <- median(na.omit(data$Height_FieldData))
# [1] 32
uav_median <- median(na.omit(data$Height_UAV))
# [1] 35.769

## Standard Deviation
bp_sd <- sd(na.omit(data$Height_BP))
# [1] 3.535993
tls_sd <- sd(data$Height_TLS)
# [1] 2.746601
field_sd <- sd(na.omit(data$Height_FieldData))
# [1] 3.965134
uav_sd <- sd(na.omit(data$Height_UAV))
# [1] 3.056836

## Range
bp_range <- range(na.omit(data$Height_BP))
# [1] 15.361 36.460
tls_range <- range(data$Height_TLS)
# [1] 20.599 38.620
field_range <- range(na.omit(data$Height_FieldData))
# [1] 18.4 39.0
uav_range <- range(na.omit(data$Height_UAV))
# [1] 23.308 39.620

## Viz
ggplot(data_long, aes(x = Source, y = Height))+
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
  labs(title = "Height Measurement Comparison", )+
  xlab("") +
  ylab("DBH [m]") +
  theme_minimal()+
  theme(plot.title = element_text(size = 15))

# export in 6.28 6.4, cubes quadratic



#### 2. Paired Comparison Tests

differences <- data$Height_BP - data$Height_TLS
differences1 <- data$Height_BP - data$Height_FieldData
differences2 <- data$Height_BP - data$Height_UAV
differences3 <- data$Height_FieldData - data$Height_UAV
differences4 <- data$Height_FieldData - data$Height_TLS
differences5 <- data$Height_UAV - data$Height_TLS



# Histogram
hist(differences, main = "Histogram of Differences in Height Measurements (Backpack & TLS Data)", xlab = "Difference")
hist(differences1, main = "Histogram of Differences in Height Measurements (Backpack & Field Data)", xlab = "Difference")
hist(differences2, main = "Histogram of Differences in Height Measurements (Backpack & UAV Data)", xlab = "Difference")
hist(differences3, main = "Histogram of Differences in Height Measurements (Field & UAV Data)", xlab = "Difference")
hist(differences4, main = "Histogram of Differences in Height Measurements (Field & TLS Data)", xlab = "Difference")
hist(differences5, main = "Histogram of Differences in Height Measurements (UAV & TLS Data)", xlab = "Difference")


# QQ plot
qqnorm(differences)
qqline(differences)

qqnorm(differences1)
qqline(differences1)

qqnorm(differences2)
qqline(differences2)

qqnorm(differences3)
qqline(differences3)

qqnorm(differences4)
qqline(differences4)

qqnorm(differences5)
qqline(differences5)

t.test(data$Height_BP, data$Height_TLS, paired = TRUE)
t.test(data$Height_BP, data$Height_FieldData, paired = TRUE)
t.test(data$Height_BP, data$Height_UAV, paired = TRUE)
t.test(data$Height_FieldData, data$Height_UAV, paired = TRUE)
t.test(data$Height_FieldData, data$Height_TLS, paired = TRUE)
t.test(data$Height_UAV, data$Height_TLS, paired = TRUE)

t.test(data$Height_TLS, data$Height_FieldData, paired = TRUE)

#### 3. Correlation & Regression

lm <- lm(data$Height_BP ~ data$Height_TLS)
lm <- lm(data$Height_BP ~ data$Height_FieldData)
lm <- lm(data$Height_BP ~ data$Height_UAV)
lm <- lm(data$Height_FieldData ~ data$Height_UAV)
lm <- lm(data$Height_FieldData ~ data$Height_TLS)
lm <- lm(data$Height_UAV ~ data$Height_TLS)

summary.lm(lm)


#### 4. pairwise comparison

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Height_FieldData, y = Height_BP), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Height Backpack") +
  xlab("")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))+
  annotate("text", x = 40, y = 15, label = "p-value = 0.003\nmean difference = 1.900", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Height_FieldData, y = Height_TLS), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Height TLS") +
  xlab("")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 40, y = 15, label = "p-value = 0.000\nmean difference = 3.360", hjust = "right")

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Height_FieldData, y = Height_UAV), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Height UAV") +
  xlab("Height Field")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 40, y = 15, label = "p-value = 0.000\nmean difference = 4.290", hjust = "right")

d <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Height_BP, y = Height_TLS), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
  annotate("text", x = 40, y = 15, label = "p-value = 0.000\nmean difference = 2.356", hjust = "right")

e <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Height_BP, y = Height_UAV), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("Height Backpack")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
  annotate("text", x = 40, y = 15, label = "p-value = 0.000\nmean difference = 2.397", hjust = "right")

f <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Height_TLS, y = Height_UAV), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("Height TLS")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"))+
  annotate("text", x = 40, y = 15, label = "p-value = 0.923\nmean difference = 0.041", hjust = "right")

lay2 <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)

grid.arrange(a, NULL, NULL, b, d, NULL, c, e, f,
             layout_matrix = lay2, widths = c(1,0.85,0.9), heights = c(1,0.9,0.95)
             ,top = textGrob("Pairwise Comparison of Height measurements [m]", gp=gpar(fontsize =15))
)
# export in 6.28 6.4, cubes quadratic