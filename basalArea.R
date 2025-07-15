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

# read data
setwd("E:/Sonja/Msc_Thesis/data/Metrics/bp/Tree_segmentation_LiDAR360/")
feb1 <- read.csv2("bp_feb1.csv")
feb2 <- read.csv2("bp_feb2.csv")
feb3 <- read.csv2("bp_feb3.csv")
feb4 <- read.csv2("bp_feb4.csv")
feb5 <- read.csv2("bp_feb5.csv")
feb6 <- read.csv2("bp_feb6.csv")
jul2 <- read.csv("bp_jul2.csv")
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
tls_summer <- read.csv("tls_summer.csv")


###################### 2. Calculate Stem Area ##################################

# A = pi * r² = (pi * d²) / 4
feb1$StemArea <- ((as.numeric(feb1$DBH)*as.numeric(feb1$DBH)) * pi) / 4
feb2$StemArea <- ((as.numeric(feb2$DBH)*as.numeric(feb2$DBH)) * pi) / 4
feb3$StemArea <- ((as.numeric(feb3$DBH)*as.numeric(feb3$DBH)) * pi) / 4
feb4$StemArea <- ((as.numeric(feb4$DBH)*as.numeric(feb4$DBH)) * pi) / 4
feb5$StemArea <- ((as.numeric(feb5$DBH)*as.numeric(feb5$DBH)) * pi) / 4
feb6$StemArea <- ((as.numeric(feb6$DBH)*as.numeric(feb6$DBH)) * pi) / 4
jul2$StemArea <- ((as.numeric(jul2$DBH)*as.numeric(jul2$DBH)) * pi) / 4
jul5$StemArea <- ((as.numeric(jul5$DBH)*as.numeric(jul5$DBH)) * pi) / 4


####################### 3. Calculate Basal Area ################################
# aoi = 0.4 ha
feb1_basalArea <- sum(feb1$StemArea)/0.4
feb2_basalArea <- sum(feb2$StemArea)/0.4
feb3_basalArea <- sum(feb3$StemArea)/0.4
feb4_basalArea <- sum(feb4$StemArea)/0.4
feb5_basalArea <- sum(feb5$StemArea)/0.4
feb6_basalArea <- sum(feb6$StemArea)/0.4
jul2_basalArea <- sum(jul2$StemArea)/0.4
jul5_basalArea <- sum(jul5$StemArea)/0.4

basalArea <- data.frame(name = rbind("feb1", "feb2", "feb3", "feb4", "feb5", 
                                     "feb6", "jul2", "jul5"),
                        basalArea = rbind(feb1_basalArea, feb2_basalArea, 
                                          feb3_basalArea, feb4_basalArea,
                                          feb5_basalArea, feb6_basalArea, 
                                          jul2_basalArea, jul5_basalArea))

# add dif to wzp area
basalArea$dif <- abs(48-basalArea$basalArea)

# basal area of feb5
bp <- feb5[10:11]

# basal area of tls data
tls_winter$stemArea <- ((as.numeric(tls_winter$DBH)*as.numeric(tls_winter$DBH))*pi)/4
tls_summer$stemArea <- ((as.numeric(tls_summer$DBH)*as.numeric(tls_summer$DBH))*pi)/4
tls_winter_basalArea <- sum(tls_winter$stemArea)/0.4
tls_summer_basalArea <- sum(tls_summer$stemArea)/0.4

# basal area of tlswinter
tls <- tls_winter[,c(2,10)]

data <- merge(tls, bp, by.x = "TreeID", by.y = "NewID")
data <- merge(data, field, by.x = "TreeID", by.y = "NewTreeID")
colnames(data) <- c("TreeID", "Area_TLS", "Area_BP", "Area_Field")


####################### 4. Paired t-tests ######################################

t.test(data$Area_TLS, data$Area_BP, paired = TRUE)
t.test(data$Area_TLS, data$Area_Field, paired = TRUE)
t.test(data$Area_Field, data$Area_BP, paired = TRUE)


############################# 5. pairwise plots ################################

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Area_Field, y = Area_BP), color = "darkolivegreen", 
             fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Stem Area Backpack") +
  xlab("")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))+
  annotate("text", x = 0.6, y = 0.1, 
           label = "p-value = 0.004\nmean difference = 0.018", hjust = "right")

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Area_Field, y = Area_TLS), color = "darkolivegreen", 
             fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Stem Area TLS") +
  xlab("Stem Area Field")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"))+
  annotate("text", x = 0.6, y = 0.1, 
           label = "p-value = 0.994\nmean difference = 0.000", hjust = "right")

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Area_TLS, y = Area_BP), color = "darkolivegreen", 
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
           label = "p-value = 0.002\nmean difference = 0.012", hjust = "right")

lay2 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
#library(grid)
grid.arrange(a, NULL, b, c,
             layout_matrix = lay2, widths = c(1,0.93), heights = c(1,1)
             ,top = textGrob("Pairwise Comparison of Stem Area measurements [m²]", 
                             gp=gpar(fontsize =15))
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

############################# 6. Boxplots ######################################

data_long <- pivot_longer(data, cols = c("Area_TLS", "Area_BP", "Area_Field"), 
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
# export in 6.28 6.4, cubes quadratic


######################## 7. Descriptive Statistics #############################

## Mean
bp_mean <- mean(na.omit(data$Area_BP))
# [1] 0.1860144
tls_mean <- mean(data$Area_TLS)
# [1] 0.1980164
field_mean <- mean(na.omit(data$Area_Field))
# 0.1935358


## Median
bp_median <- median(na.omit(data$Area_BP))
# [1] 0.1676385
tls_median <- median(data$Area_TLS)
# [1] 0.1727578
field_median <- median(na.omit(data$Area_Field))
# [1] 0.1887259

## Standard Deviation
bp_sd <- sd(na.omit(data$Area_BP))
# [1] 0.103148
tls_sd <- sd(data$Area_TLS)
# [1] 0.09346953
field_sd <- sd(na.omit(data$Area_Field))
# [1] 0.08596721

## Range
bp_range <- range(na.omit(data$Area_BP))
# [1] 0.01697167 0.52681446
tls_range <- range(data$Area_TLS)
# [1] 0.02805521 0.51148977
field_range <- range(na.omit(data$Area_Field))
# [1] 0.0315843 0.4583662