############# Script for Comparing DBH from TLS & BP & Field Data ##############

#### 1. descriptive statistics
#### 2. Paired comparison test


################################################################################

library(ggplot2)
library(tidyr)
library(grid)
library(patchwork)
library(fmsb)

setwd("E:/Sonja/Msc_Thesis/data/Metrics/")

bp <- read.csv2("bp/Tree_segmentation_LiDAR360/bp_feb5.csv")
bp_summer <- read.csv2("bp/Tree_segmentation_LiDAR360/bp_jul2.csv")
tls <- read.csv("tls/tree_segmentation_LiDAR360/tls_winter.csv")
tls_summer <- read.csv2("tls/tree_segmentation_LiDAR360/tls_summer.csv")
field <- read.csv2("allData.csv")
field[,6:35] <- NULL
field[,1] <- NULL
field[,2] <- NULL
field[,3] <- NULL
field$fieldData_DBH <- field$fieldData_DBH/pi/100

tls[,3:5] <- NULL
tls[,4:6] <- NULL
tls[,1] <- NULL

tls_summer[,3:5] <- NULL
tls_summer[,4:6] <- NULL
tls_summer[,1] <- NULL
tls_summer[,1] <- NULL

bp[,1:5] <- NULL
bp[,2:4] <- NULL

bp_summer[,1:5] <- NULL
bp_summer[,2:4] <- NULL


# for(i in 1:nrow(field)){
#   if(is.na(field$fieldData_Height[i])){
#     field$NewTreeID[i] <- NA
#   }
# }

data <- merge(bp, tls, by.x = "NewID", by.y = "TreeID")
data <- merge(data, field, by.x = "NewID", by.y = "NewTreeID" )
colnames(data) <- c("TreeID", "DBH_BP", "DBH_TLS", "DBH_Field")
data <- merge(data, bp_summer, by.x = "TreeID", by.y = "NewID" )
data <- merge(data, tls_summer, by.x = "TreeID", by.y = "PreviousID" )
colnames(data) <- c("TreeID", "Backpack Winter", "TLS Winter", "Field Data", "Backpack Summer", "TLS Summer")


data$`Backpack Winter` <- as.numeric(data$`Backpack Winter`)
data$`Backpack Summer` <- as.numeric(data$`Backpack Summer`)
data$`TLS Summer` <- as.numeric(data$`TLS Summer`)
data_long <- pivot_longer(data, cols = c("Backpack Winter", "TLS Winter", "Field Data", "Backpack Summer", "TLS Summer"), names_to = "Source", values_to = "DBH")


#### 1. Descriptive statistics


## Mean
bp_mean <- mean(na.omit(data$`Backpack Winter`))
# [1] 0.4671705
tls_mean <- mean(as.numeric(data$`TLS Winter`))
# [1] 0.488
field_mean <- mean(na.omit(data$`Field Data`))
# [1] 0.484
bp_mean_s <- mean(na.omit(data$`Backpack Summer`))
tls_mean_s <- mean(as.numeric(data$`TLS Summer`))


## Median
bp_median <- median(na.omit(data$`Backpack Winter`))
# [1] 0.462
tls_median <- median(as.numeric(data$`TLS Winter`))
# [1] 0.47
field_median <- median(na.omit(data$`Field Data`))
# [1] 0.4901972
bp_median_s <- median(na.omit(data$`Backpack Summer`))
tls_median_s <- median(as.numeric(data$`TLS Summer`))


## Standard Deviation
bp_sd <- sd(na.omit(data$`Backpack Winter`))
# [1] 0.1363557
tls_sd <- sd(as.numeric(data$`TLS Winter`))
# [1] 0.117
field_sd <- sd(na.omit(data$`Field Data`))
# [1] 0.1101464
bp_sd_s <- sd(na.omit(data$`Backpack Summer`))
tls_sd_s <- sd(as.numeric(data$`TLS Summer`))

## Range## RangeDBH_TLS_summer
bp_range <- range(na.omit(data$`Backpack Winter`))
# [1] 0.147 0.819
tls_range <- range(as.numeric(data$`TLS Winter`))
# [1] 0.186 0.807
field_range <- range(na.omit(data$`Field Data`))
# [1] 0.2005352 0.7639437
bp_range_s <- range(na.omit(data$`Backpack Summer`))
tls_range_s <- range(as.numeric(data$`TLS Summer`))

desc_stats <- data.frame(mean = c(bp_mean, bp_mean_s, tls_mean, tls_mean_s, field_mean),
                         median = c(bp_median, bp_median_s, tls_median, tls_median_s, field_median),
                         range_min = c(bp_range[[1]], bp_range_s[[1]], tls_range[[1]], tls_range_s[[1]], field_range[[1]]),
                         range_max = c(bp_range[[2]], bp_range_s[[2]], tls_range[[2]], tls_range_s[[2]], field_range[[2]]),
                         sd = c(bp_sd, bp_sd_s, tls_sd, tls_sd_s, field_sd))

rownames(desc_stats) <- c("Backpack Winter", "Backpack Summer", "TLS Winter", "TLS Summer", "Field Data")


## Viz
ggplot(data_long, aes(x = Source, y = DBH))+
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
  #labs(title = "DBH Measurement Comparison", )+
  xlab("") +
  ylab("DBH [m]") +
  theme_minimal()+
  theme(plot.title = element_text(size = 15))

# export in 6.28 6.4, cubes quadratic



#### 2. Paired Comparison Tests

differences1 <- data$DBH_BP - data$DBH_TLS
differences2 <- data$DBH_BP - data$DBH_Field
differences3 <- data$DBH_TLS - data$DBH_Field


# Histogram
hist(differences1, main = "Histogram of Differences in DBH Measurements (Backpack & TLS Data)", xlab = "Difference")
hist(differences2, main = "Histogram of Differences in DBH Measurements (Backpack & Field Data)", xlab = "Difference")
hist(differences3, main = "Histogram of Differences in DBH Measurements (TLS & Field Data)", xlab = "Difference")

# histograms together
library(gridExtra)
library(cowplot)

color <- adjustcolor("darkolivegreen4", alpha.f = 0.35)
# layout(matrix(c(1, 0,
#                 2, 3), nrow=2, byrow=TRUE),
#        heights = c(1, 1), widths = c(1, 1))
# par(oma = c(0,4,4,2))
# hist1 <- hist(differences1, xlab = "Difference (Backpack - TLS", main = "", col = color, border = "darkolivegreen")
# hist2 <- hist(differences2, xlab = "Difference (Backpack - Field Data)", main = "", col = color, border = "darkolivegreen")
# hist3 <- hist(differences3, xlab = "Difference", main = "", col = color, border = "darkolivegreen")

hist1 <- ggplot(data.frame(differences1 = differences1), aes(x = differences1)) +
  geom_histogram(fill = color, color = "darkolivegreen", binwidth = 0.01) +
  labs(x = "Difference [m] (Backpack - TLS)", y = "Count", title = "") +
  xlim(c(-0.15, 0.15))+
  ylim(c(0,10))+
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 18))
hist2 <- ggplot(data.frame(differences2 = differences2), aes(x = differences2)) +
  geom_histogram(fill = color, color = "darkolivegreen", binwidth = 0.01) +
  labs(x = "Difference [m] (Backpack - Field Data)", y = "Count", title = "") +
  xlim(c(-0.15, 0.15))+
  ylim(c(0,10))+
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 18))
hist3 <- ggplot(data.frame(differences3 = differences3), aes(x = differences3)) +
  geom_histogram(fill = color, color = "darkolivegreen", binwidth = 0.01) +
  labs(x = "Difference [m] (Field Data - TLS)", y = "Count", title = "") +
  xlim(c(-0.15, 0.15))+
  ylim(c(0,10))+
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 18))

# mtext("Backpack", side=3, line=1, at=0.25, outer=TRUE, cex=1.2)
# mtext("TLS", side=3, line=1, at=0.75, outer=TRUE, cex=1.2)
# mtext("TLS", side=2, line=1, at=0.75, outer=TRUE, cex=1.2)
# mtext("Field Data", side=2, line=1, at=0.25, outer=TRUE, cex=1.2)

# paired boxplot
# layout(matrix(c(1, 0,
#                 2, 3), nrow=2, byrow=TRUE),
#        heights = c(1, 1), widths = c(1, 1))

bp_tls <- ggplot(data = subset(data_long, Source != "DBH_Field"), aes(Source, DBH, fill = Source)) +
  # geom_boxplot(color = "darkolivegreen", fill = "darkolivegreen4", alpha = 0.3, notch = TRUE, notchwidth = 0.8, outlier.colour="red", outlier.fill="red", outlier.size=4) +
  geom_line(aes(group=TreeID), position = position_dodge(0.2), col = "darkgrey") +
  geom_point(aes(group = TreeID),fill = "darkolivegreen4", size=2,shape=21, position = position_dodge(0.2)) +
  theme_minimal() +
  ylab("DBH [m]") + 
  xlab("Source")+
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 18))

bp_field <- ggplot(data = subset(data_long, Source != "DBH_TLS"), aes(Source, DBH, fill = Source)) +
  # geom_boxplot(color = "darkolivegreen", fill = "darkolivegreen4", alpha = 0.3, notch = TRUE, notchwidth = 0.8, outlier.colour="red", outlier.fill="red", outlier.size=4) +
  geom_line(aes(group=TreeID), position = position_dodge(0.2), col = "darkgrey") +
  geom_point(aes(group = TreeID),fill = "darkolivegreen4", size=2,shape=21, position = position_dodge(0.2)) +
  theme_minimal() +
  ylab("DBH [m]") +
  xlab("Source")+
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 18))

field_tls <- ggplot(data = subset(data_long, Source != "DBH_BP"), aes(Source, DBH, fill = Source)) +
  # geom_boxplot(color = "darkolivegreen", fill = "darkolivegreen4", alpha = 0.3, notch = TRUE, notchwidth = 0.8, outlier.colour="red", outlier.fill="red", outlier.size=4) +
  geom_line(aes(group=TreeID), position = position_dodge(0.2), col = "darkgrey") +
  geom_point(aes(group = TreeID),fill = "darkolivegreen4", size=2,shape=21, position = position_dodge(0.2)) +
  theme_minimal() +
  ylab("DBH [m]") +
  xlab("Source")+
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 18))
 
# grid.arrange(bp_tls, bp_field, field_tls, layout_matrix = matrix(c(0, 3, 1, 2), nrow=2, byrow=TRUE), heights = c(1, 1), widths = c(1, 1))
lay <- matrix(1:12, nrow = 6, ncol = 2, byrow = TRUE)
library(grid)
grid.arrange(NULL, NULL, bp_tls, hist1, NULL, NULL, bp_field, hist2, NULL, NULL, field_tls, hist3, 
             layout_matrix = lay, widths = c(1,1), heights = c(0.2,2,0.2,2,0.2,2)
             ,top = textGrob("Pairwise Comparison of DBH measurements", gp=gpar(fontsize =20))
             )

# QQ plot
qqnorm(differences)
qqline(differences)

t.test(data$`Backpack Winter`, data$`TLS Winter`, paired = TRUE)
# Paired t-test
# 
# data:  data$`Backpack Winter` and data$`TLS Winter`
# t = -4.5697, df = 88, p-value = 1.584e-05
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.02977791 -0.01172770
# sample estimates:
#   mean difference 
# -0.02075281

t.test(data$`Backpack Winter`, data$`Field Data`, paired = TRUE)
# data:  data$`Backpack Winter` and data$`Field Data`
# t = -3.6291, df = 32, p-value = 0.0009802
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.04435541 -0.01246414
# sample estimates:
#   mean difference 
# -0.02840977

t.test(data$`TLS Winter`, data$`Field Data`, paired = TRUE)
# data:  data$`TLS Winter` and data$`Field Data`
# t = 0.068393, df = 32, p-value = 0.9459
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.01001071  0.01070632
# sample estimates:
#   mean difference 
# 0.0003478035 

t.test(data$`Backpack Summer`, data$`TLS Summer`, paired = TRUE)
# data:  data$`Backpack Summer` and data$`TLS Summer`
# t = 0.84181, df = 88, p-value = 0.4022
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.01152791  0.02847173
# sample estimates:
#   mean difference 
# 0.00847191 

t.test(data$`Backpack Summer`, data$`Field Data`, paired = TRUE)
# data:  data$`Backpack Summer` and data$`Field Data`
# t = 1.4707, df = 32, p-value = 0.1511
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.009188023  0.056913933
# sample estimates:
#   mean difference 
# 0.02386296 

t.test(data$`TLS Summer`, data$`Field Data`, paired = TRUE)
# data:  data$`TLS Summer` and data$`Field Data`
# t = 0.60484, df = 32, p-value = 0.5496
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.008859506  0.016342992
# sample estimates:
#   mean difference 
# 0.003741743 



#### 3. Correlation & Regression

lm <- lm(dbh_data$DBH_BP ~ dbh_data$DBH_TLS)
lm <- lm(dbh_data_full$DBH_BP ~ dbh_data_full$DBH_Field)
lm <- lm(dbh_data_full$DBH_TLS ~ dbh_data_full$DBH_Field)

summary.lm(lm)
# Call:
#   lm(formula = dbh_data$DBH_BP ~ dbh_data$DBH_TLS)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.126598 -0.031764 -0.006567  0.024752  0.183477 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -0.01720    0.02014  -0.854    0.395    
# dbh_data$DBH_TLS  1.01354    0.04037  25.104   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.04896 on 90 degrees of freedom
# (1 Beobachtung als fehlend gelöscht)
# Multiple R-squared:  0.875,	Adjusted R-squared:  0.8737 
# F-statistic: 630.2 on 1 and 90 DF,  p-value: < 2.2e-16


#### 4. pairwise plots

# plots for winter comparison

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data` , y = `Backpack Winter`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Backpack") +
  xlab("Field Data")+
  xlim(0,0.8)+
  ylim(0, 0.8)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))+
  annotate("text", x = 0.8, y = 0.1, label = "Paired t-test:\np-value = 9.802e-4\nmean difference = 0.028", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data`, y = `TLS Winter`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("TLS") +
  xlab("")+
  xlim(0,0.8)+
  ylim(0, 0.8)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 0.8, y = 0.1, label = "Paired t-test:\np-value = 0.946\nmean difference = 3.478e-4", hjust = "right")

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Winter`, y = `Backpack Winter`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("TLS")+
  xlim(0,0.8)+
  ylim(0, 0.8)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"))+
  annotate("text", x = 0.8, y = 0.1, label = "Paired t-test:\np-value = 1.584e-5\nmean difference = 0.021", hjust = "right")

# export in 6.28 6.4, cubes quadratic
layout <- "
A#
BC
"
combined <- (
  b+a+c +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right")
)


# for summer

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data` , y = `Backpack Summer`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Backpack") +
  xlab("Field Data")+
  xlim(0,0.8)+
  ylim(0, 0.8)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))+
  annotate("text", x = 0.8, y = 0.1, label = "Paired t-test:\np-value = 0.151\nmean difference = 0.024", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data`, y = `TLS Summer`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("TLS") +
  xlab("")+
  xlim(0,0.8)+
  ylim(0, 0.8)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 0.8, y = 0.1, label = "Paired t-test:\np-value = 0.550\nmean difference = 0.004", hjust = "right")

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Summer`, y = `Backpack Summer`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("TLS")+
  xlim(0,0.8)+
  ylim(0, 0.8)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"))+
  annotate("text", x = 0.8, y = 0.1, label = "Paired t-test:\np-value = 0.402\nmean difference = 0.008", hjust = "right")

# export in 6.28 6.4, cubes quadratic
layout <- "
A#
BC
"
combined <- (
  b+a+c +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right")
)



# compare summer and winter for individual sensors
# backpack
t.test(data$`Backpack Summer`, data$`Backpack Winter`, paired = TRUE)

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Backpack Winter` , y = `Backpack Summer`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Backpack Summer") +
  xlab("Backpack Winter")+
  xlim(0,0.8)+
  ylim(0, 0.8)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"))+
  annotate("text", x = 0.8, y = 0.1, label = "Paired t-test:\np-value = 0.003\nmean difference = 0.030", hjust = "right")


# backpack
t.test(data$`TLS Summer`, data$`TLS Winter`, paired = TRUE)

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Winter` , y = `TLS Summer`), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("TLS Summer") +
  xlab("TLS Winter")+
  xlim(0,0.8)+
  ylim(0, 0.8)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"))+
  annotate("text", x = 0.8, y = 0.1, label = "Paired t-test:\np-value = 0.665\nmean difference = 0.001", hjust = "right")

layout <- "
AB
"
combined <- (
  a+b+
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right")
)

