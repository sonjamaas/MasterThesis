library(data.table)
library(rgl)

setwd("E:/Sonja/Msc_Thesis/data/9_individualTrees/")

# make list of all tree files
tree_files <- list.files(pattern = "\\.csv$", full.names = TRUE)

# read data in those files
# data <- lapply(tree_files, read.csv)

# filter tree files
point_counts <- sapply(tree_files, function(f){
  nrow(fread(f, select = 1L, nThread = parallel::detectCores()))
})

has_valid_dbh <- sapply(tree_files, function(f){
  pc <- fread(f)
  if(ncol(pc)<3)return(FALSE)
  dbh_points <- sum(pc[[3]]>= 1.25 & pc[[3]]<=1.35)
  dbh_points >= 20
})

valid_trees <- tree_files[point_counts>=3000 & point_counts<=50000 & has_valid_dbh]
#invalid_trees <- tree_files[!valid_trees]

plot3d(read.csv(valid_trees[[1]]))
# invalid trees: 1, (43), 74, 83
