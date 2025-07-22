################################################################################
################# script for calculating the basal area ########################
################################################################################

#### Workflow ####
# 1. Prerequisites
# 2. Calculate Stem Area
# 3. Caculate Basal Area
# 4. Paired t-tests
# 5. pairwise plots
# 6. Boxplots
# 7. Descriptive Statistics


########################## 1. Prerequisites ####################################

library(ggplot2)
library(patchwork)
library(tidyr)

# read data
setwd("E:/Sonja/Msc_Thesis/data/Metrics/bp/Tree_segmentation_LiDAR360/")
feb1 <- read.csv2("bp_feb1.csv")
feb2 <- read.csv2("bp_feb2.csv")
feb3 <- read.csv2("bp_feb3.csv")
feb4 <- read.csv2("bp_feb4.csv")
feb5 <- read.csv2("bp_feb5.csv")
feb6 <- read.csv2("bp_feb6.csv")
jul2 <- read.csv2("bp_jul2.csv")
jul5 <- read.csv2("bp_jul5.csv")

# keep only necessary columns
field <- read.csv2("E:/Sonja/Msc_Thesis/data/Metrics/allData.csv")
field[,6:35] <- NULL
field[,1] <- NULL
field[,2] <- NULL
field[,3] <- NULL
field$fieldData_DBH <- field$fieldData_DBH/pi/100
field$StemArea <- ((as.numeric(field$fieldData_DBH)*as.numeric(field$fieldData_DBH)) * pi) / 4
field <- field[,c(1,3)]

setwd("E:/Sonja/Msc_Thesis/data/Metrics/tls/tree_segmentation_LiDAR360/")
tls_winter <- read.csv("tls_winter.csv")
tls_summer <- read.csv2("tls_summer.csv")


###################### 2. Calculate Stem Area ##################################

# A = pi * r² = (pi * d²) / 4
#feb1$StemArea <- ((as.numeric(feb1$DBH)*as.numeric(feb1$DBH)) * pi) / 4
#feb2$StemArea <- ((as.numeric(feb2$DBH)*as.numeric(feb2$DBH)) * pi) / 4
#feb3$StemArea <- ((as.numeric(feb3$DBH)*as.numeric(feb3$DBH)) * pi) / 4
#feb4$StemArea <- ((as.numeric(feb4$DBH)*as.numeric(feb4$DBH)) * pi) / 4
feb5$StemArea <- ((as.numeric(feb5$DBH)*as.numeric(feb5$DBH)) * pi) / 4
#feb6$StemArea <- ((as.numeric(feb6$DBH)*as.numeric(feb6$DBH)) * pi) / 4
jul2$StemArea <- ((as.numeric(jul2$DBH)*as.numeric(jul2$DBH)) * pi) / 4
#jul5$StemArea <- ((as.numeric(jul5$DBH)*as.numeric(jul5$DBH)) * pi) / 4


####################### 3. Calculate Basal Area ################################
# aoi = 0.4 ha
#feb1_basalArea <- sum(feb1$StemArea)/0.4
#feb2_basalArea <- sum(feb2$StemArea)/0.4
#feb3_basalArea <- sum(feb3$StemArea)/0.4
#feb4_basalArea <- sum(feb4$StemArea)/0.4
feb5_basalArea <- sum(feb5$StemArea)/0.4
#feb6_basalArea <- sum(feb6$StemArea)/0.4
jul2_basalArea <- sum(jul2$StemArea)/0.4
#jul5_basalArea <- sum(jul5$StemArea)/0.4

basalArea <- data.frame(name = rbind("feb1", "feb2", "feb3", "feb4", "feb5", 
                                     "feb6", "jul2", "jul5"),
                        basalArea = rbind(feb1_basalArea, feb2_basalArea, 
                                          feb3_basalArea, feb4_basalArea,
                                          feb5_basalArea, feb6_basalArea, 
                                          jul2_basalArea, jul5_basalArea))

# add dif to wzp area
basalArea$dif <- abs(48-basalArea$basalArea)

# basal area bp
bp <- feb5[10:11]
bp_summer <- jul2[10:11]

# basal area of tls data
tls_winter$stemArea <- ((as.numeric(tls_winter$DBH)*as.numeric(tls_winter$DBH))*pi)/4
tls_summer$stemArea <- ((as.numeric(tls_summer$DBH)*as.numeric(tls_summer$DBH))*pi)/4
tls_winter_basalArea <- sum(tls_winter$stemArea)/0.4
tls_summer_basalArea <- sum(tls_summer$stemArea)/0.4

# basal area 
tls <- tls_winter[,c(2,10)]
tls_summer <- tls_summer[,c(10,11)]

data <- merge(tls, bp, by.x = "TreeID", by.y = "NewID")
data <- merge(data, field, by.x = "TreeID", by.y = "NewTreeID")
colnames(data) <- c("TreeID", "Area_TLS", "Area_BP", "Area_Field")
data <- merge(data, tls_summer, by.x = "TreeID", by.y = "PreviousID")
data <- merge(data, bp_summer, by.x = "TreeID", by.y = "NewID")
colnames(data) <- c("TreeID", "TLS Winter", "Backpack Winter", "Field Data", "TLS Summer", "Backpack Summer")


####################### 4. Paired t-tests ######################################

t.test(data$`TLS Winter`, data$`Backpack Winter`, paired = TRUE)
t.test(data$`TLS Winter`, data$`Field Data`, paired = TRUE)
t.test(data$`Field Data`, data$`Backpack Winter`, paired = TRUE)


############################# 5. pairwise plots ################################

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data`, y = `Backpack Winter`), color = "darkolivegreen", 
             fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Stem Area Backpack") +
  xlab("Stem Area Field")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 0.6, y = 0.1, 
           label = "Paired t-test:\np-value = 0.004\nmean difference = 0.018", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data`, y = `TLS Winter`), color = "darkolivegreen", 
             fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Stem Area TLS") +
  xlab("")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        axis.text.x = element_blank())+
  annotate("text", x = 0.6, y = 0.1, 
           label = "Paired t-test:\np-value = 0.994\nmean difference = 3.198e-5", hjust = "right")

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Winter`, y = `Backpack Winter`), color = "darkolivegreen", 
             fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("Stem Area TLS")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"))+
  annotate("text", x = 0.6, y = 0.1, 
           label = "Paired t-test:\np-value = 0.002\nmean difference = 0.012", hjust = "right")

# export in 6.28 6.4, cubes quadratic
layout <- "
A#
BC
"
combined <- (
  b + a + c +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right")
)

# for summer data (tls, backpack, field data)
t.test(data$`TLS Summer`, data$`Backpack Summer`, paired = TRUE)
t.test(data$`TLS Summer`, data$`Field Data`, paired = TRUE)
t.test(data$`Field Data`, data$`Backpack Summer`, paired = TRUE)

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data`, y = `Backpack Summer`), color = "darkolivegreen", 
             fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Stem Area Backpack") +
  xlab("Stem Area Field")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 0.6, y = 0.1, 
           label = "Paired t-test:\np-value = 0.024\nmean difference = 0.028", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Field Data`, y = `TLS Summer`), color = "darkolivegreen", 
             fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Stem Area TLS") +
  xlab("")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        axis.text.x = element_blank())+
  annotate("text", x = 0.6, y = 0.1, 
           label = "Paired t-test:\np-value = 0.533\nmean difference = 0.003", hjust = "right")

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Summer`, y = `Backpack Summer`), color = "darkolivegreen", 
             fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("Stem Area TLS")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"))+
  annotate("text", x = 0.6, y = 0.1, 
           label = "Paired t-test:\np-value = 0.031\nmean difference = 0.016", hjust = "right")

# export in 6.28 6.4, cubes quadratic
layout <- "
A#
BC
"
combined <- (
  b + a + c +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right")
)

# without field data
c<- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Summer`, y = `Backpack Summer`), color = "darkolivegreen", 
             fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Stem Area Backpack") +
  xlab("Stem Area TLS")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        #axis.text.y = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  annotate("text", x = 0.6, y = 0.1, 
           label = "Paired t-test:\np-value = 0.031\nmean difference = 0.016", hjust = "right")


# summer vs winter comparison
t.test(data$`Backpack Winter`, data$`Backpack Summer`, paired = TRUE)
t.test(data$`TLS Winter`, data$`TLS Summer`, paired = TRUE)
a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `Backpack Winter`, y = `Backpack Summer`), color = "darkolivegreen", 
             fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Backpack Summer") +
  xlab("Backpack Winter")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"))+
  annotate("text", x = 0.6, y = 0.1, 
           label = "Paired t-test:\np-value = 4.799e-5\nmean difference = 0.028", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = `TLS Winter`, y = `TLS Summer`), color = "darkolivegreen", 
             fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("TLS Summer") +
  xlab("TLS Winter")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"),
        #axis.text.x = element_blank()
        )+
  annotate("text", x = 0.6, y = 0.1, 
           label = "Paired t-test:\np-value = 0.702\nmean difference = 0.001", hjust = "right")

layout <- "
AB
"
combined <- (
  a + b +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right")
)

############################# 6. Boxplots ######################################

data_long <- pivot_longer(data, cols = c("TLS Winter", "Backpack Winter", "Field Data", "TLS Summer", "Backpack Summer"), 
                          names_to = "Source", values_to = "StemArea")

ggplot(data_long, aes(x = Source, y = StemArea))+
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
  #labs(title = "Stem Area measurement Comparison")+
  xlab("") +
  ylab("Stem Area [m²]") +
  theme_minimal()+
  theme(plot.title = element_text(size = 15))
# export in 7.5 6.4


######################## 7. Descriptive Statistics #############################

## Mean
bp_mean <- mean(na.omit(data$`Backpack Winter`))
tls_mean <- mean(data$`TLS Winter`)
field_mean <- mean(na.omit(data$`Field Data`))
bp_mean_s <- mean(na.omit(data$`Backpack Summer`))
tls_mean_s <- mean(data$`TLS Summer`)

## Median
bp_median <- median(na.omit(data$`Backpack Winter`))
tls_median <- median(data$`TLS Winter`)
field_median <- median(na.omit(data$`Field Data`))
bp_median_s <- median(na.omit(data$`Backpack Summer`))
tls_median_s <- median(data$`TLS Summer`)

## Standard Deviation
bp_sd <- sd(na.omit(data$`Backpack Winter`))
tls_sd <- sd(data$`TLS Winter`)
field_sd <- sd(na.omit(data$`Field Data`))
bp_sd_s <- sd(na.omit(data$`Backpack Summer`))
tls_sd_s <- sd(data$`TLS Summer`)

## Range
bp_range <- range(na.omit(data$`Backpack Winter`))
tls_range <- range(data$`TLS Winter`)
field_range <- range(na.omit(data$`Field Data`))
bp_range_s <- range(na.omit(data$`Backpack Summer`))
tls_range_s <- range(data$`TLS Summer`)



desc_stats <- data.frame(mean = c(bp_mean, bp_mean_s, tls_mean, tls_mean_s, field_mean),
                         median = c(bp_median, bp_median_s, tls_median, tls_median_s, field_median),
                         range_min = c(bp_range[[1]], bp_range_s[[1]], tls_range[[1]], tls_range_s[[1]], field_range[[1]]),
                         range_max = c(bp_range[[2]], bp_range_s[[2]], tls_range[[2]], tls_range_s[[2]], field_range[[2]]),
                         sd = c(bp_sd, bp_sd_s, tls_sd, tls_sd_s, field_sd))

rownames(desc_stats) <- c("Backpack Winter", "Backpack Summer", "TLS Winter", "TLS Summer", "Field Data")
