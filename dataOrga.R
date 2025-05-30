#### data organisation

setwd("E:/1_Projects/Spessart/InnoLab_Sonja2025/InnoLab_Data/6_LiDAR360_metrics/")
setwd("F:/Sonja/InnoLab/1_Data/6_LiDAR360_metrics/")


# make neat data tables from LiDAR data 

# ALS data
data <- read.csv("BHD_Height_CrownParameters/ALS_first_last_merged_Cloud_Clip_byRectangle_NormalizebyGroundPoints_PointCloudSegmentation.csv")

data$PreviousID <- NULL

write.csv(data, "F:/Sonja/Msc_Thesis/data/als/als.csv")

# TLS data
tls_winter <- read.csv("BHD_Height_CrownParameters/TLS_Hain_20_21_subsample_0_05_Normalize by Ground Points_Point Cloud Segmentation.csv")
tls_winter$PreviousID <- NULL

write.csv(tls_winter, "F:/Sonja/Msc_Thesis/data/tls/tls_winter.csv")

tls_summer <- read.csv("BHD_Height_CrownParameters/TLS_HainLaub_Sommer_21_subsampled_0_05_Normalize by Ground Points_Point Cloud Segmentation.csv")
write.csv(tls_summer, "F:/Sonja/Msc_Thesis/data/tls/tls_summer.csv")

# UAV data
# uav_summer <- read.csv("BHD_Height_CrownParameters/UAV_jul1_subsampled_0_05_Normalize by Ground Points_Point Cloud Segmentation_ALS.csv")

# backpack data
feb1 <- read.csv("BHD_Height_CrownParameters/backpack_feb1_subsampled_0_05_Normalize by Ground Points_Point Cloud Segmentation.csv")
feb2 <- read.csv("BHD_Height_CrownParameters/backpack_feb2_subsampled_0_05_Normalize by Ground Points_Point Cloud Segmentation.csv")
feb3 <- read.csv("BHD_Height_CrownParameters/backpack_feb3_subsampled_0_05_Normalize by Ground Points_Point Cloud Segmentation.csv")
feb4 <- read.csv("BHD_Height_CrownParameters/backpack_feb4_subsampled_0_05_Normalize by Ground Points_Point Cloud Segmentation.csv")
feb5 <- read.csv("BHD_Height_CrownParameters/backpack_feb5_subsampled_0_05_Normalize by Ground Points_Point Cloud Segmentation.csv")
feb6 <- read.csv("BHD_Height_CrownParameters/backpack_feb6_subsampled_0_05_Normalize by Ground Points_Point Cloud Segmentation.csv")
jul1 <- read.csv("BHD_Height_CrownParameters/backpack_july1_subsampled_0_05_Normalize by Ground Points_Point Cloud Segmentation.csv")
jul3 <- read.csv("BHD_Height_CrownParameters/backpack_jul3_subsampled_0_05_Point Cloud Segmentation.csv")
jul5 <- read.csv("BHD_Height_CrownParameters/backpack_july5_subsampled_0_05 - Cloud_Normalize by Ground Points_Point Cloud Segmentation.csv")
jul2 <- read.csv("F:/Sonja/Msc_Thesis/data/bp/bp_jul2_Noise Filter_Remove Outliers_Normalize by Ground Points_Point Cloud Segmentation.csv")

feb1$PreviousID <- NULL
feb2$PreviousID <- NULL
feb3$PreviousID <- NULL
feb4$PreviousID <- NULL
feb5$PreviousID <- NULL
feb6$PreviousID <- NULL
jul1$PreviousID <- NULL
jul3$PreviousID <- NULL
jul5$PreviousID <- NULL
jul2$PreviousID <- NULL

write.csv(jul2, "F:/Sonja/Msc_Thesis/data/bp/bp_jul2.csv")

