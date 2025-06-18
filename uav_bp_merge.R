################ Script for merging UAV with matching BP data ##################


setwd("E:/Sonja/Msc_Thesis/data/")

library(lidR)
library(sf)

tree_uav <- readLAS("8_preprocessedData/tree_6_matched46_54.las")
tree_bp1 <- read.csv("9_individualTrees/tree46.csv")

tree_bp1_las <- LAS(tree_bp1)


x <- plot(tree_bp1_las)
plot(tree_uav, add = x)

extend <- st_bbox(tree_bp1_las)

tree_uav_clip_bp1 <- clip_roi(tree_uav, extend)
# tree_uav_clip_bp2 <- filter_poi(tree_uav, X < extend[[1]] | X > extend[[2]] | Y < extend[[3]] | Y > extend[[4]])

tree_uav_segmented <- segment_trees(tree_uav, li2012(), attribute = "ID")

y <- plot(tree_uav_clip_bp1)

tree_merged <- rbind(tree_uav@data, tree_bp1_las@data, fill = TRUE)
tree_merged <- tree_merged[,1:3]

tree_merged <- LAS(tree_merged)
plot(tree_merged)

writeLAS(tree_merged, "uav_bp_merge/tree13_merged.las")
