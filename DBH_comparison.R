############# Script for Comparing DBH from TLS & BP & Field Data ##############

#### 1. descriptive statistics
#### 2. Paired comparison test


################################################################################

library(ggplot2)
library(tidyr)

setwd("E:/Sonja/Msc_Thesis/data/Metrics/")

bp <- read.csv2("bp/Tree_segmentation_LiDAR360/bp_feb5.csv")
tls <- read.csv("tls/tree_segmentation_LiDAR360/tls_winter.csv")
field <- read.csv2("allData.csv")
field[,6:35] <- NULL
field[,1] <- NULL
field[,2] <- NULL
field[,3] <- NULL
field$fieldData_DBH <- field$fieldData_DBH/pi/100

tls[,3:5] <- NULL
tls[,4:6] <- NULL
tls[,1] <- NULL

bp[,1:5] <- NULL
bp[,2:4] <- NULL


# for(i in 1:nrow(field)){
#   if(is.na(field$fieldData_Height[i])){
#     field$NewTreeID[i] <- NA
#   }
# }

data <- merge(bp, tls, by.x = "NewID", by.y = "TreeID")
data <- merge(data, field, by.x = "NewID", by.y = "NewTreeID" )
colnames(data) <- c("TreeID", "DBH_BP", "DBH_TLS", "DBH_Field")

data$DBH_BP <- as.numeric(data$DBH_BP)
data_long <- pivot_longer(data, cols = c("DBH_BP", "DBH_TLS", "DBH_Field"), names_to = "Source", values_to = "DBH")


#### 1. Descriptive statistics


## Mean
bp_mean <- mean(na.omit(data$DBH_BP))
# [1] 0.4671705
tls_mean <- mean(as.numeric(data$DBH_TLS))
# [1] 0.4881705
field_mean <- mean(na.omit(data$DBH_Field))
# [1] 0.4844098

## Median
bp_median <- median(na.omit(data$DBH_BP))
# [1] 0.462
tls_median <- median(as.numeric(data$DBH_TLS))
# [1] 0.469
field_median <- median(na.omit(data$DBH_Field))
# [1] 0.4901972


## Standard Deviation
bp_sd <- sd(na.omit(data$DBH_BP))
# [1] 0.1371365
tls_sd <- sd(as.numeric(data$DBH_TLS))
# [1] 0.1181976
field_sd <- sd(na.omit(data$DBH_Field))
# [1] 0.1101464

## Range
bp_range <- range(na.omit(data$DBH_BP))
# [1] 0.147 0.819
tls_range <- range(as.numeric(data$DBH_TLS))
# [1] 0.186 0.807
field_range <- range(na.omit(data$DBH_Field))
# [1] 0.2005352 0.7639437

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
  labs(title = "DBH Measurement Comparison", )+
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

t.test(dbh_data$DBH_BP, dbh_data$DBH_TLS, paired = TRUE)
# Paired t-test
# 
# data:  dbh_data$DBH_BP and dbh_data$DBH_TLS
# t = -2.0994, df = 91, p-value = 0.03855
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.0207519097 -0.0005741773
# sample estimates:
#   mean difference 
# -0.01066304

t.test(dbh_data_full$DBH_BP, dbh_data_full$DBH_Field, paired = TRUE)
t.test(dbh_data_full$DBH_TLS, dbh_data_full$DBH_Field, paired = TRUE)



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

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = DBH_Field, y = DBH_BP), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("DBH Backpack") +
  xlab("")+
  xlim(0,0.8)+
  ylim(0, 0.8)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = DBH_Field, y = DBH_TLS), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("DBH TLS") +
  xlab("DBH Field")+
  xlim(0,0.8)+
  ylim(0, 0.8)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"))

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = DBH_TLS, y = DBH_BP), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("DBH TLS")+
  xlim(0,0.8)+
  ylim(0, 0.8)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"))

lay2 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
library(grid)
grid.arrange(a, NULL, b, c,
             layout_matrix = lay2, widths = c(1,0.93), heights = c(1,1)
             ,top = textGrob("Pairwise Comparison of DBH measurements [m]", gp=gpar(fontsize =15))
)
# export in 6.28 6.4, cubes quadratic