# script for calculating the basal area

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


field <- read.csv2("E:/Sonja/Msc_Thesis/data/Metrics/allData.csv")
field[,6:35] <- NULL
field[,1] <- NULL
field[,2] <- NULL
field[,3] <- NULL
field$fieldData_DBH <- field$fieldData_DBH/pi/100
field$StemArea <- ((as.numeric(field$fieldData_DBH)*as.numeric(field$fieldData_DBH)) * pi) / 4
field <- field[,c(1,3)]


# A = pi * r² = (pi * d²) / 4
feb1$StemArea <- ((as.numeric(feb1$DBH)*as.numeric(feb1$DBH)) * pi) / 4
feb2$StemArea <- ((as.numeric(feb2$DBH)*as.numeric(feb2$DBH)) * pi) / 4
feb3$StemArea <- ((as.numeric(feb3$DBH)*as.numeric(feb3$DBH)) * pi) / 4
feb4$StemArea <- ((as.numeric(feb4$DBH)*as.numeric(feb4$DBH)) * pi) / 4
feb5$StemArea <- ((as.numeric(feb5$DBH)*as.numeric(feb5$DBH)) * pi) / 4
feb6$StemArea <- ((as.numeric(feb6$DBH)*as.numeric(feb6$DBH)) * pi) / 4
jul2$StemArea <- ((as.numeric(jul2$DBH)*as.numeric(jul2$DBH)) * pi) / 4
jul5$StemArea <- ((as.numeric(jul5$DBH)*as.numeric(jul5$DBH)) * pi) / 4


# aoi = 0.4 ha
feb1_basalArea <- sum(feb1$StemArea)/0.4
feb2_basalArea <- sum(feb2$StemArea)/0.4
feb3_basalArea <- sum(feb3$StemArea)/0.4
feb4_basalArea <- sum(feb4$StemArea)/0.4
feb5_basalArea <- sum(feb5$StemArea)/0.4
feb6_basalArea <- sum(feb6$StemArea)/0.4
jul2_basalArea <- sum(jul2$StemArea)/0.4
jul5_basalArea <- sum(jul5$StemArea)/0.4

basalArea <- data.frame(name = rbind("feb1", "feb2", "feb3", "feb4", "feb5", "feb6", "jul2", "jul5"),
                        basalArea = rbind(feb1_basalArea, feb2_basalArea, 
                                          feb3_basalArea, feb4_basalArea,
                                          feb5_basalArea, feb6_basalArea, 
                                          jul2_basalArea, jul5_basalArea))

# add dif to wzp area
basalArea$dif <- abs(48-basalArea$basalArea)

# basal area of feb5
bp <- feb5[10:11]


# basal area of tls data
setwd("E:/Sonja/Msc_Thesis/data/Metrics/tls/tree_segmentation_LiDAR360/")
tls_winter <- read.csv("tls_winter.csv")
tls_summer <- read.csv("tls_summer.csv")
tls_winter$stemArea <- ((as.numeric(tls_winter$DBH)*as.numeric(tls_winter$DBH))*pi)/4
tls_summer$stemArea <- ((as.numeric(tls_summer$DBH)*as.numeric(tls_summer$DBH))*pi)/4
tls_winter_basalArea <- sum(tls_winter$stemArea)/0.4
tls_summer_basalArea <- sum(tls_summer$stemArea)/0.4

# basal area of tlswinter
tls <- tls_winter[,c(2,10)]

data <- merge(tls, bp, by.x = "TreeID", by.y = "NewID")
data <- merge(data, field, by.x = "TreeID", by.y = "NewTreeID")
colnames(data) <- c("TreeID", "Area_TLS", "Area_BP", "Area_Field")

#### 4. pairwise plots

a <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Area_Field, y = Area_BP), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Stem Area Backpack") +
  xlab("")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"))

b <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Area_Field, y = Area_TLS), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("Stem Area TLS") +
  xlab("Stem Area Field")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"))

c <- ggplot(data = data)+
  geom_abline(col = "grey", linewidth = 1)+
  geom_point(aes(x = Area_TLS, y = Area_BP), color = "darkolivegreen", fill = "darkolivegreen3", alpha = 0.6, shape = 21, size = 4)+
  theme_minimal() +
  ylab("") +
  xlab("Stem Area TLS")+
  xlim(0,0.6)+
  ylim(0, 0.6)+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"))

lay2 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
library(grid)
grid.arrange(a, NULL, b, c,
             layout_matrix = lay2, widths = c(1,0.93), heights = c(1,1)
             ,top = textGrob("Pairwise Comparison of Stem Area measurements [m²]", gp=gpar(fontsize =15))
)
# export in 6.28 6.4, cubes quadratic

