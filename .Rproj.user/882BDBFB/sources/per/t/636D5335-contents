# script for calculating the basal area

# read data
setwd("E:/Sonja/Msc_Thesis/data/bp/")
feb1 <- read.csv2("bp_feb1.csv")
feb2 <- read.csv2("bp_feb2.csv")
feb3 <- read.csv2("bp_feb3.csv")
feb4 <- read.csv2("bp_feb4.csv")
feb5 <- read.csv2("bp_feb5.csv")
feb6 <- read.csv2("bp_feb6.csv")
jul2 <- read.csv("bp_jul2.csv")
jul5 <- read.csv2("bp_jul5.csv")


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




# basal area of tls data
setwd("F:/Sonja/Msc_Thesis/data/tls/")
tls_winter <- read.csv("tls_winter.csv")
tls_summer <- read.csv("tls_summer.csv")
tls_winter$stemArea <- ((as.numeric(tls_winter$DBH)*as.numeric(tls_winter$DBH))*pi)/4
tls_summer$stemArea <- ((as.numeric(tls_summer$DBH)*as.numeric(tls_summer$DBH))*pi)/4
tls_winter_basalArea <- sum(tls_winter$stemArea)/0.4
tls_summer_basalArea <- sum(tls_summer$stemArea)/0.4
