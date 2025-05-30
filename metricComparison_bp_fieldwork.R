# comparison of backpack walk metrics (dbh, height) with field work
library(ggplot2)

setwd("F:/Sonja/Msc_Thesis/data/bp/")
setwd("E:/Sonja/Msc_Thesis/data/bp/")

feb1 <- read.csv2("bp_feb1.csv")
feb1$name <- "feb1"
feb2 <- read.csv2("bp_feb2.csv")
feb2$name <- "feb2"
feb3 <- read.csv2("bp_feb3.csv")
feb3$name <- "feb3"
feb4 <- read.csv2("bp_feb4.csv")
feb4$name <- "feb4"
feb5 <- read.csv2("bp_feb5.csv")
feb5$name <- "feb5"
feb5[57,] <- NA
feb6 <- read.csv2("bp_feb6.csv")
feb6$name <- "feb6"
jul2 <- read.csv("bp_jul2.csv")
jul2$name <- "jul2"
jul5 <- read.csv2("bp_jul5.csv")
jul5$name <- "jul5"


fieldwork <- read.csv2("E:/Sonja/InnoLab/1_Data/allData.csv")
fieldwork[8:35] <- NULL
fieldwork$fieldData_DBH <- (fieldwork$fieldData_DBH)/pi/100
fieldwork$name <- "field Data"

# make scatter plot
ggplot()+
  geom_jitter(data = feb1, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_jitter(data = feb2, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_jitter(data = feb3, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_jitter(data = feb4, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_jitter(data = feb5, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_jitter(data = feb6, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_jitter(data = jul2, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_jitter(data = jul5, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_jitter(data = fieldwork, aes(x = name, y = fieldData_DBH, col = name))

# make box plot
ggplot()+
  geom_boxplot(data = feb1, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_boxplot(data = feb2, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_boxplot(data = feb3, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_boxplot(data = feb4, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_boxplot(data = feb5, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_boxplot(data = feb6, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_boxplot(data = jul2, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_boxplot(data = jul5, aes(x = name, y = as.numeric(DBH), col = name))+
  geom_boxplot(data = fieldwork, aes(x = name, y = fieldData_DBH, col = name))


# check for each fieldwork dbh how much the corresponding lidar dbh from each walk differs
# for that make dbh table
feb1_dbh <- subset(feb1, select = c(DBH, NewID))
colnames(feb1_dbh) <- c("feb1_dbh", "NewID")

feb2_dbh <- rbind(subset(feb2, select = c(DBH, NewID)), c("",""))
colnames(feb2_dbh) <- c("feb2_dbh", "NewID")
feb2_dbh$NewID <- as.numeric(feb2_dbh$NewID)

feb3_dbh <- rbind(subset(feb3, select = c(DBH, NewID)), c("",""), c("",""), c("",""), c("",""))
colnames(feb3_dbh) <- c("feb3_dbh", "NewID")
feb3_dbh$NewID <- as.numeric(feb3_dbh$NewID)

feb4_dbh <- rbind(subset(feb4, select = c(DBH, NewID)), c("",""), c("",""), c("",""))
colnames(feb4_dbh) <- c("feb4_dbh", "NewID")
feb4_dbh$NewID <- as.numeric(feb4_dbh$NewID)

feb5_dbh <- rbind(subset(feb5, select = c(DBH, NewID)), c("",""), c("",""))
colnames(feb5_dbh) <- c("feb5_dbh", "NewID")
feb5_dbh$NewID <- as.numeric(feb5_dbh$NewID)

feb6_dbh <- rbind(subset(feb6, select = c(DBH, NewID)), c("",""), c("",""), c("",""), c("",""))
colnames(feb6_dbh) <- c("feb6_dbh", "NewID")
feb6_dbh$NewID <- as.numeric(feb6_dbh$NewID)

jul2_dbh <- subset(jul2, select = c(DBH, NewID))
colnames(jul2_dbh) <- c("jul2_dbh", "NewID")
jul2_dbh$NewID <- as.numeric(jul2_dbh$NewID)

jul5_dbh <- rbind(subset(jul5, select = c(DBH, NewID)), c("",""), c("",""))
colnames(jul5_dbh) <- c("jul5_dbh", "NewID")
jul5_dbh$NewID <- as.numeric(jul5_dbh$NewID)

field_dbh <- rbind(subset(fieldwork, select = c(fieldData_DBH, NewTreeID)), c("",""), c("",""))
colnames(field_dbh) <- c("field_dbh", "NewID")
field_dbh$NewID <- as.numeric(field_dbh$NewID)

# merge into one dataframe by the new ID column
dbh <- merge(feb1_dbh, feb2_dbh, by = "NewID")
dbh <- subset(dbh, !is.na(NewID))
dbh <- merge(dbh, feb3_dbh, by = "NewID")
dbh <- merge(dbh, feb4_dbh, by = "NewID")
dbh <- merge(dbh, feb5_dbh, by = "NewID")
dbh <- merge(dbh, feb6_dbh, by = "NewID")
dbh <- merge(dbh, jul2_dbh, by = "NewID")
dbh <- merge(dbh, jul5_dbh, by = "NewID")
dbh <- merge(dbh, field_dbh, by = "NewID")

# subset data with the fieldwork values
dbh_subset <- subset(dbh, !is.na(field_dbh))

# make new columns for the difference between measurements

dbh_subset$feb1_dif <- abs(as.numeric(dbh_subset$feb1_dbh) - as.numeric(dbh_subset$field_dbh))
dbh_subset$feb2_dif <- abs(as.numeric(dbh_subset$feb2_dbh) - as.numeric(dbh_subset$field_dbh))
dbh_subset$feb3_dif <- abs(as.numeric(dbh_subset$feb3_dbh) - as.numeric(dbh_subset$field_dbh))
dbh_subset$feb4_dif <- abs(as.numeric(dbh_subset$feb4_dbh) - as.numeric(dbh_subset$field_dbh))
dbh_subset$feb5_dif <- abs(as.numeric(dbh_subset$feb5_dbh) - as.numeric(dbh_subset$field_dbh))
dbh_subset$feb6_dif <- abs(as.numeric(dbh_subset$feb6_dbh) - as.numeric(dbh_subset$field_dbh))
dbh_subset$jul2_dif <- abs(as.numeric(dbh_subset$jul2_dbh) - as.numeric(dbh_subset$field_dbh))
dbh_subset$jul5_dif <- abs(as.numeric(dbh_subset$jul5_dbh) - as.numeric(dbh_subset$field_dbh))

# compute mean difference to field work data
meanDif <- data.frame(name = rbind("feb1", "feb2", "feb3", "feb4", "feb5", "feb6", "jul2", "jul5"), 
                      dif = c(mean(dbh_subset$feb1_dif), 
                              mean(dbh_subset$feb2_dif),
                              mean(dbh_subset$feb3_dif),
                              mean(dbh_subset$feb4_dif),
                              mean(dbh_subset$feb5_dif),
                              mean(dbh_subset$feb6_dif),
                              mean(dbh_subset$jul2_dif),
                              mean(dbh_subset$jul5_dif)))





# same thing for height
feb1_height <- subset(feb1, select = c(TreeHeight, NewID))
colnames(feb1_height) <- c("feb1_height", "NewID")

feb2_height <- rbind(subset(feb2, select = c(TreeHeight, NewID)), c("",""))
colnames(feb2_height) <- c("feb2_height", "NewID")
feb2_height$NewID <- as.numeric(feb2_height$NewID)

feb3_height <- rbind(subset(feb3, select = c(TreeHeight, NewID)), c("",""), c("",""), c("",""), c("",""))
colnames(feb3_height) <- c("feb3_height", "NewID")
feb3_height$NewID <- as.numeric(feb3_height$NewID)

feb4_height <- rbind(subset(feb4, select = c(TreeHeight, NewID)), c("",""), c("",""), c("",""))
colnames(feb4_height) <- c("feb4_height", "NewID")
feb4_height$NewID <- as.numeric(feb4_height$NewID)

feb5_height <- rbind(subset(feb5, select = c(TreeHeight, NewID)), c("",""), c("",""))
colnames(feb5_height) <- c("feb5_height", "NewID")
feb5_height$NewID <- as.numeric(feb5_height$NewID)

feb6_height <- rbind(subset(feb6, select = c(TreeHeight, NewID)), c("",""), c("",""), c("",""), c("",""))
colnames(feb6_height) <- c("feb6_height", "NewID")
feb6_height$NewID <- as.numeric(feb6_height$NewID)

jul2_height <- subset(jul2, select = c(TreeHeight, NewID))
colnames(jul2_height) <- c("jul2_height", "NewID")
jul2_height$NewID <- as.numeric(jul2_height$NewID)

jul5_height <- rbind(subset(jul5, select = c(TreeHeight, NewID)), c("",""), c("",""))
colnames(jul5_height) <- c("jul5_height", "NewID")
jul5_height$NewID <- as.numeric(jul5_height$NewID)

field_height <- rbind(subset(fieldwork, select = c(fieldData_Height, NewTreeID)), c("",""), c("",""))
colnames(field_height) <- c("field_height", "NewID")
field_height$NewID <- as.numeric(field_height$NewID)

# merge into one dataframe by the new ID column
height <- merge(feb1_height, feb2_height, by = "NewID")
height <- subset(height, !is.na(NewID))
height <- merge(height, feb3_height, by = "NewID")
height <- merge(height, feb4_height, by = "NewID")
height <- merge(height, feb5_height, by = "NewID")
height <- merge(height, feb6_height, by = "NewID")
height <- merge(height, jul2_height, by = "NewID")
height <- merge(height, jul5_height, by = "NewID")
height <- merge(height, field_height, by = "NewID")

# subset data with the fieldwork values
height_subset <- subset(height, !is.na(field_height))

# make new columns for the difference between measurements

height_subset$feb1_dif <- abs(as.numeric(height_subset$feb1_height) - as.numeric(height_subset$field_height))
height_subset$feb2_dif <- abs(as.numeric(height_subset$feb2_height) - as.numeric(height_subset$field_height))
height_subset$feb3_dif <- abs(as.numeric(height_subset$feb3_height) - as.numeric(height_subset$field_height))
height_subset$feb4_dif <- abs(as.numeric(height_subset$feb4_height) - as.numeric(height_subset$field_height))
height_subset$feb5_dif <- abs(as.numeric(height_subset$feb5_height) - as.numeric(height_subset$field_height))
height_subset$feb6_dif <- abs(as.numeric(height_subset$feb6_height) - as.numeric(height_subset$field_height))
height_subset$jul2_dif <- abs(as.numeric(height_subset$jul2_height) - as.numeric(height_subset$field_height))
height_subset$jul5_dif <- abs(as.numeric(height_subset$jul5_height) - as.numeric(height_subset$field_height))

# compute mean difference to field work data
meanDif_height <- data.frame(name = rbind("feb1", "feb2", "feb3", "feb4", "feb5", "feb6", "jul2", "jul5"), 
                      dif = c(mean(height_subset$feb1_dif), 
                              mean(height_subset$feb2_dif),
                              mean(height_subset$feb3_dif),
                              mean(height_subset$feb4_dif),
                              mean(height_subset$feb5_dif),
                              mean(height_subset$feb6_dif),
                              mean(height_subset$jul2_dif),
                              mean(height_subset$jul5_dif)))

                      