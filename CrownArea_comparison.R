######### Script for Comparing Crown Area from TLS & BP & UAV Data ###########

#### 1. descriptive statistics
#### 2. Paired comparison test


################################################################################

library(ggplot2)
library(tidyr)
library(grid)
library(gridExtra)
library(patchwork)

setwd("E:/Sonja/Msc_Thesis/data/Metrics/")

bp <- read.csv2("bp/Tree_segmentation_LiDAR360/bp_feb5.csv")
bp_summer <- read.csv2("bp/Tree_segmentation_LiDAR360/bp_jul2.csv")
tls <- read.csv("tls/tree_segmentation_LiDAR360/tls_winter.csv")
tls_summer <- read.csv2("tls/tree_segmentation_LiDAR360/tls_summer.csv")
uav <- read.csv2("E:/Sonja/Msc_Thesis/data/8_preprocessedData/uav/UAV_feb2_shifted_clipped_CHM_CHM Segmentation.csv")
uav2 <- read.csv("uav/crown_parameters.csv")

bp[,1:7] <- NULL
bp[,2] <- NULL
bp$CrownArea <- as.numeric(bp$CrownArea)

bp_summer <- bp_summer[,c(8,10)]

tls[,3:7] <- NULL
tls[,1] <- NULL
tls[,3] <- NULL

tls_summer <- tls_summer[,c(8,10)]

uav[,1:5] <- NULL
uav$CrownArea <- as.numeric(uav$CrownArea)

uav2 <- uav2[,c(2,5)]
uav2$file_name <- sub("\\.las$", "", uav2$file_name)
uav2$file_name <- as.numeric(uav2$file_name)

data <- merge(bp, tls, by.x = "NewID", by.y = "TreeID")
#data <- merge(data, uav, by.x = "NewID", by.y = "file_name")
data <- merge(data, uav2, by.x = "NewID", by.y = "file_name")
colnames(data) <- c("TreeID", "Backpack Winter", "TLS Winter", "UAV Winter")
# data$CrownArea_UAV <- as.numeric(data$CrownArea_UAV)
data <- merge(data, bp_summer, by.x = "TreeID", by.y = "NewID")
data <- merge(data, tls_summer, by.x = "TreeID", by.y = "PreviousID")
colnames(data) <- c("TreeID", "Backpack Winter", "TLS Winter", "UAV Winter", "Backpack Summer", "TLS Summer")
data$`Backpack Summer` <- as.numeric(data$`Backpack Summer`)
data$`TLS Summer` <- as.numeric(data$`TLS Summer`)


data_long <- pivot_longer(data, cols = c("Backpack Winter", "TLS Winter", "UAV Winter", "Backpack Summer", "TLS Summer"), 
                          names_to = "Source", values_to = "CrownArea")


#### 1. Descriptive statistics


## Mean
bp_mean <- mean(na.omit(data$`Backpack Winter`))
tls_mean <- mean(as.numeric(data$`TLS Winter`))
uav_mean <- mean(na.omit(data$`UAV Winter`))
bp_mean_s <- mean(na.omit(data$`Backpack Summer`))
tls_mean_s <- mean(as.numeric(data$`TLS Summer`))


## Median
bp_median <- median(na.omit(data$`Backpack Winter`))
tls_median <- median(as.numeric(data$`TLS Winter`))
uav_median <- median(na.omit(data$`UAV Winter`))
bp_median_s <- median(na.omit(data$`Backpack Summer`))
tls_median_s <- median(as.numeric(data$`TLS Summer`))


## Standard Deviation
bp_sd <- sd(na.omit(data$`Backpack Winter`))
tls_sd <- sd(as.numeric(data$`TLS Winter`))
uav_sd <- sd(na.omit(data$`UAV Winter`))
bp_sd_s <- sd(na.omit(data$`Backpack Summer`))
tls_sd_s <- sd(as.numeric(data$`TLS Summer`))

## Range## RangeDBH_TLS_summer
bp_range <- range(na.omit(data$`Backpack Winter`))
tls_range <- range(as.numeric(data$`TLS Winter`))
uav_range <- range(na.omit(data$`UAV Winter`))
bp_range_s <- range(na.omit(data$`Backpack Summer`))
tls_range_s <- range(as.numeric(data$`TLS Summer`))

desc_stats <- data.frame(mean = c(bp_mean, bp_mean_s, tls_mean, tls_mean_s, uav_mean),
                         median = c(bp_median, bp_median_s, tls_median, tls_median_s, uav_median),
                         range_min = c(bp_range[[1]], bp_range_s[[1]], tls_range[[1]], tls_range_s[[1]], uav_range[[1]]),
                         range_max = c(bp_range[[2]], bp_range_s[[2]], tls_range[[2]], tls_range_s[[2]], uav_range[[2]]),
                         sd = c(bp_sd, bp_sd_s, tls_sd, tls_sd_s, uav_sd))

rownames(desc_stats) <- c("Backpack Winter", "Backpack Summer", "TLS Winter", "TLS Summer", "Field Data")



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
# # export in 7.5 6.4



#### 2. Paired Comparison Tests

# differences <- data$CrownArea_BP - data$CrownArea_TLS
# differences2 <- data$CrownArea_BP - data$CrownArea_UAV
# differences5 <- data$CrownArea_UAV - data$CrownArea_TLS

# Histogram
# hist(differences, main = "Histogram of Differences in Height Measurements (Backpack & TLS Data)", xlab = "Difference")
# hist(differences2, main = "Histogram of Differences in Height Measurements (Backpack & UAV Data)", xlab = "Difference")
# hist(differences5, main = "Histogram of Differences in Height Measurements (UAV & TLS Data)", xlab = "Difference")
# 
# # QQ plot
# qqnorm(differences)
# qqline(differences)
# 
# qqnorm(differences2)
# qqline(differences2)
# 
# qqnorm(differences5)
# qqline(differences5)
# 
# #### 3. Correlation & Regression
# 
# lm <- lm(data$CrownArea_BP ~ data$CrownArea_TLS)
# lm <- lm(data$CrownArea_BP ~ data$CrownArea_UAV)
# lm <- lm(data$CrownArea_UAV ~ data$CrownArea_TLS)
# 
# summary.lm(lm)


#### 4. paired scatterplots

# winter

t.test(data$`Backpack Winter`, data$`TLS Winter`, paired = TRUE)
t.test(data$`Backpack Winter`, data$`UAV Winter`, paired = TRUE)
t.test(data$`UAV Winter`, data$`TLS Winter`, paired = TRUE)

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Winter`, y = `Backpack Winter`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Crown Area Backpack") +
  xlab("")+
  xlim(0, 210)+
  ylim(0, 210)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))+
  annotate("text", x = 200, y = 25, label = "Paired t-test:\np-value = 0.000\nmean difference = 11.341", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Winter`, y = `UAV Winter`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Crown Area UAV") +
  xlab("Crown Area TLS")+
  xlim(0, 210)+
  ylim(0, 210)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 200, y = 25, label = "Paired t-test:\np-value = 7.407e-6\nmean difference = 19.227", hjust = "right")

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Backpack Winter`, y = `UAV Winter`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("Crown Area Backpack")+
  xlim(0, 210)+
  ylim(0, 210)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"))+
  annotate("text", x = 200, y = 25, label = "Paired t-test:\np-value = 0.0769\nmean difference = 7.885", hjust = "right")


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

# summer

t.test(data$`Backpack Summer`, data$`TLS Summer`, paired = TRUE)

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Summer`, y = `Backpack Summer`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Crown Area Backpack") +
  xlab("Crown Area TLS")+
  xlim(0, 210)+
  ylim(0, 210)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  annotate("text", x = 200, y = 25, label = "Paired t-test:\np-value = 1.536e-8\nmean difference = 30.831", hjust = "right")


# export in 6.28 6.4, cubes quadratic

