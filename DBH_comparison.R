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
field$fieldData_DBH <- field$fieldData_DBH/pi/100

for(i in 1:nrow(field)){
  if(is.na(field$fieldData_Height[i])){
    field$NewTreeID[i] <- NA
  }
}

data <- merge(bp, tls, by.x = "NewID", by.y = "TreeID")
dbh_data <- data
dbh_data[,3:4] <- NULL
dbh_data_full <- merge(dbh_data, field, by.x = "TreeID", by.y = "NewTreeID" )
colnames(dbh_data) <- c("TreeID", "DBH_BP", "DBH_TLS")
colnames(dbh_data_full) <- c("TreeID", "DBH_BP", "DBH_TLS", "DBH_Field")

dbh_data$DBH_BP <- as.numeric(dbh_data$DBH_BP)
dbh_data[65,]$DBH_BP <- NA
dbh_data_long <- pivot_longer(dbh_data, cols = c("DBH_BP", "DBH_TLS"), names_to = "Source", values_to = "DBH")
dbh_data_full_long <- pivot_longer(dbh_data_full, cols = c("DBH_BP", "DBH_TLS", "DBH_Field"), names_to = "Source", values_to = "DBH")


#### 1. Descriptive statistics


## Mean
bp_dbh_mean <- mean(na.omit(dbh_data$DBH_BP))
# [1] 0.4719022
tls_dbh_mean <- mean(as.numeric(tls$DBH))
# [1] 0.4810842
field_dbh_mean <- mean(na.omit(field$fieldData_DBH))
# [1] 0.4844098

## Median
bp_dbh_median <- median(na.omit(dbh_data$DBH_BP))
# [1] 0.467
tls_dbh_median <- median(as.numeric(tls$DBH))
# [1] 0.47
field_dbh_median <- median(na.omit(field$fieldData_DBH))
# [1] 0.4901972


## Standard Deviation
bp_dbh_sd <- sd(na.omit(dbh_data$DBH_BP))
# [1] 0.1377271
tls_dbh_sd <- sd(as.numeric(tls$DBH))
# [1] 0.1271605
field_dbh_sd <- sd(na.omit(field$fieldData_DBH))
# [1] 0.1101464

## Range
bp_dbh_range <- range(na.omit(dbh_data$DBH_BP))
# [1] 0.147 0.819
tls_dbh_range <- range(as.numeric(tls$DBH))
# [1] 0.186 0.807
field_dbh_range <- range(na.omit(field$fieldData_DBH))
# [1] 0.2005352 0.7639437

## Viz
ggplot(dbh_data_full_long, aes(x = Source, y = DBH))+
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
  labs(title = "DBH measurement Comparison")



#### 2. Paired Comparison Tests

differences <- dbh_data$DBH_BP - dbh_data$DBH_TLS
differences <- dbh_data_full$DBH_BP - dbh_data_full$DBH_Field
differences <- dbh_data_full$DBH_TLS - dbh_data_full$DBH_Field


# Histogram
hist(differences, main = "Histogram of Differences in DBH Measurements (TLS & Field Data)", xlab = "Difference")

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