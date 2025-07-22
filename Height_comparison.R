####### Script for Comparing Heights from TLS & BP & Field & UAV Data ##########

#### 1. descriptive statistics
#### 2. Paired comparison test


################################################################################

library(ggplot2)
library(tidyr)
library(grid)
library(patchwork)

setwd("E:/Sonja/Msc_Thesis/data/Metrics/")

bp <- read.csv2("bp/Tree_segmentation_LiDAR360/bp_feb5.csv")
bp_summer <- read.csv2("bp/Tree_segmentation_LiDAR360/bp_jul2.csv")
tls <- read.csv("tls/tree_segmentation_LiDAR360/tls_winter.csv")
tls_summer <- read.csv2("tls/tree_segmentation_LiDAR360/tls_summer.csv")
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

bp_summer[1:4] <- NULL
bp_summer[2:5] <- NULL


tls[,6:10] <- NULL
tls[,1] <- NULL
tls[,2:3] <- NULL
tls_summer[,1:4] <- NULL
tls_summer[,2:5] <- NULL


uav[,1:3] <- NULL
uav[,2:3] <- NULL
uav$TreeHeight <- as.numeric(uav$TreeHeight)

data <- merge(bp, tls, by.x = "NewID", by.y = "TreeID")
data <- merge(data, field, by.x = "NewID", by.y = "NewTreeID" )
data <- merge(data, uav, by.x = "NewID", by.y = "NewID")
data <- merge(data, bp_summer, by.x = "NewID", by.y = "NewID")
data <- merge(data, tls_summer, by.x = "NewID", by.y = "PreviousID")

colnames(data) <- c("TreeID", "Backpack Winter", "TLS Winter", "Field Data", "UAV Data", "Backpack Summer", "TLS Summer")
data$`Field Data` <- as.numeric(data$`Field Data`)
data$`Backpack Summer` <- as.numeric(data$`Backpack Summer`)
data$`TLS Summer` <- as.numeric(data$`TLS Summer`)



data_long <- pivot_longer(data, cols = c("Backpack Winter", "TLS Winter", "Field Data", "UAV Data", "Backpack Summer", "TLS Summer"), names_to = "Source", values_to = "Height")


#### 1. Descriptive statistics


## Mean
bp_height_mean <- mean(na.omit(data$`Backpack Winter`))
# [1] 32.99812
tls_height_mean <- mean(data$`TLS Winter`)
# [1] 35.37453
field_height_mean <- mean(na.omit(data$`Field Data`))
# 31.68571
uav_height_mean <- mean(na.omit(data$`UAV Data`))
# 35.40883
bp_sommer_mean <- mean(na.omit(data$`Backpack Summer`))
# 26.8756
tls_sommer_mean <- mean(na.omit(data$`TLS Summer`))
# 33.77915


## Median
bp_median <- median(na.omit(data$`Backpack Winter`))
# [1] 34.089
tls_median <- median(data$`TLS Winter`)
# [1] 35.658
field_median <- median(na.omit(data$`Field Data`))
# [1] 32
uav_median <- median(na.omit(data$`UAV Data`))
# [1] 35.814
bp_median_s <- median(na.omit(data$`Backpack Summer`))
tls_median_s <- median(na.omit(data$`TLS Summer`))


## Standard Deviation
bp_sd <- sd(na.omit(data$`Backpack Winter`))
# [1] 3.535993
tls_sd <- sd(data$`TLS Winter`)
# [1] 2.746601
field_sd <- sd(na.omit(data$`Field Data`))
# [1] 3.965134
uav_sd <- sd(na.omit(data$`UAV Data`))
# [1] 3.056836
bp_sd_s <- sd(na.omit(data$`Backpack Summer`))
btls_sd_s <- sd(na.omit(data$`TLS Summer`))


## Range
bp_range <- range(na.omit(data$`Backpack Winter`))
# [1] 15.361 36.460
tls_range <- range(data$`TLS Winter`)
# [1] 20.599 38.620
field_range <- range(na.omit(data$`Field Data`))
# [1] 18.4 39.0
uav_range <- range(na.omit(data$`UAV Data`))
# [1] 23.308 39.620
bp_range_s <- range(na.omit(data$`Backpack Summer`))
tls_range_s <- range(data$`TLS Summer`)

# table
desc_stats <- data.frame(mean = c(bp_height_mean, bp_sommer_mean, tls_height_mean, tls_sommer_mean, uav_height_mean, field_height_mean),
                         median = c(bp_median, bp_median_s, tls_median, tls_median_s, uav_median, field_median),
                         range_min = c(bp_range[[1]], bp_range_s[[1]], tls_range[[1]], tls_range_s[[1]], uav_range[[1]], field_range[[1]]),
                         range_max = c(bp_range[[2]], bp_range_s[[2]], tls_range[[2]], tls_range_s[[2]], uav_range[[2]], field_range[[2]]),
                         sd = c(bp_sd, bp_sd_s, tls_sd, btls_sd_s, uav_sd, field_sd))

rownames(desc_stats) <- c("Backpack Winter", "Backpack Summer", "TLS Winter", "TLS Summer", "UAV Data", "Field Data")

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
  #labs(title = "Height Measurement Comparison", )+
  xlab("") +
  ylab("Height [m]") +
  theme_minimal()+
  theme(plot.title = element_text(size = 15))

# export in 7.5 6.4



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

# winter t-tests

t.test(data$`Backpack Winter`, data$`TLS Winter`, paired = TRUE)
t.test(data$`Backpack Winter`, data$`Field Data`, paired = TRUE)
t.test(data$`Backpack Winter`, data$`UAV Data`, paired = TRUE)
t.test(data$`Field Data`, data$`UAV Data`, paired = TRUE)
t.test(data$`Field Data`, data$`TLS Winter`, paired = TRUE)
t.test(data$`UAV Data`, data$`TLS Winter`, paired = TRUE)

# summer t-tests

t.test(data$`Backpack Summer`, data$`TLS Summer`, paired = TRUE)
t.test(data$`Backpack Summer`, data$`Field Data`, paired = TRUE)
t.test(data$`Field Data`, data$`TLS Summer`, paired = TRUE)

#### 3. Correlation & Regression

lm <- lm(data$Height_BP ~ data$Height_TLS)
lm <- lm(data$Height_BP ~ data$Height_FieldData)
lm <- lm(data$Height_BP ~ data$Height_UAV)
lm <- lm(data$Height_FieldData ~ data$Height_UAV)
lm <- lm(data$Height_FieldData ~ data$Height_TLS)
lm <- lm(data$Height_UAV ~ data$Height_TLS)

summary.lm(lm)


#### 4. pairwise comparison winter

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data`, y = `Backpack Winter`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Height Backpack") +
  xlab("")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))+
  annotate("text", x = 40, y = 15, label = "Paired t-test:\np-value = 0.004\nmean difference = 1.900", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data`, y = `TLS Winter`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Height TLS") +
  xlab("")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 40, y = 15, label = "Paired t-test:\np-value = 9.131e-7\nmean difference = 3.360", hjust = "right")

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data`, y = `UAV Data`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Height UAV") +
  xlab("Height Field")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 40, y = 15, label = "Paired t-test:\np-value = 1.287e-4\nmean difference = 4.290", hjust = "right")

d <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Backpack Winter`, y = `TLS Winter`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
  annotate("text", x = 40, y = 15, label = "Paired t-test:\np-value = 9.967e-7\nmean difference = 2.379", hjust = "right")

e <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Backpack Winter`, y = `UAV Data`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("Height Backpack")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
  annotate("text", x = 40, y = 15, label = "Paired t-test:\np-value = 7.822e-5\nmean difference = 2.413", hjust = "right")

f <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Winter`, y = `UAV Data`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("Height TLS")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"))+
  annotate("text", x = 40, y = 15, label = "Paired t-test:\np-value = 0.933\nmean difference = 0.0343 ", hjust = "right")


# export in 6.28 6.4, cubes quadratic

layout <- "
A##
BC#
DEF
"
combined <- (
  a+b+d+c+e+f+
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right")
)


#### 5. pairwise comparison summer


# summer t-tests

t.test(data$`Backpack Summer`, data$`TLS Summer`, paired = TRUE)
t.test(data$`Backpack Summer`, data$`Field Data`, paired = TRUE)
t.test(data$`Field Data`, data$`TLS Summer`, paired = TRUE)

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data`, y = `Backpack Summer`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Height Backpack") +
  xlab("Height Field Data")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 40, y = 15, label = "Paired t-test:\np-value = 0.002\nmean difference = 3.173", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data`, y = `TLS Summer`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Height TLS") +
  xlab("")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))+
  annotate("text", x = 40, y = 15, label = "Paired t-test:\np-value = 0.003\nmean difference = 2.608", hjust = "right")

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Summer`, y = `Backpack Summer`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("Height TLS")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0.5,0,0), "cm"),
        axis.text.y = element_blank())+
  annotate("text", x = 40, y = 15, label = "Paired t-test:\np-value = 3.953e-15\nmean difference = 6.904", hjust = "right")



layout <- "
A#
BC
"
combined <- (
  b+a+c +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right")
)


# summer -winter comparison for backpack and tls
t.test(data$`Backpack Summer`, data$`Backpack Winter`, paired = TRUE)
t.test(data$`TLS Summer`, data$`TLS Winter`, paired = TRUE)

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Backpack Winter`, y = `Backpack Summer`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Backpack Summer") +
  xlab("Backpack Winter")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"))+
  annotate("text", x = 40, y = 15, label = "Paired t-test:\np-value = 1.904e-12\nmean difference = 6.120", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Winter`, y = `TLS Summer`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("TLS Summer") +
  xlab("TLS Winter")+
  xlim(10,40)+
  ylim(10, 40)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"))+
  annotate("text", x = 40, y = 15, label = "Paired t-test:\np-value = 7.073e-5\nmean difference = 1.595", hjust = "right")


layout <- "
AB
"
combined <- (
  a+b+
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right")
)
# export 6.28x3 in
