# Install required packages if not already installed
if (!requireNamespace("lidR", quietly = TRUE)) install.packages("lidR")
install.packages("aRchi")
remotes::install_github('umr-amap/aRchi')

library(lidR)
library(aRchi)

# 1. Load the .las file (replace with your file path)
setwd("E:/Sonja/Msc_Thesis/data/9_individualTrees/")
las_file <- "tree45.las"
tls <- readLAS(las_file)
tls@data[,4:15] <- NULL
  

# 2. Build an empty aRchi object and add the point cloud
archi <- build_aRchi()
archi <- add_pointcloud(archi, point_cloud = tls)

# 3. (Optional) Visualize the point cloud
plot(archi@pointcloud)

# 4. Skeletonize the point cloud
# You may want to adjust D, cl_dist, and max_d for your data
archi <- skeletonize_pc(archi, D = 0.05, cl_dist = 0.1, max_d = 1, progressive = TRUE)

# 5. (Optional) Smooth the skeleton for better visualization
archi <- smooth_skeleton(archi)

# 6. Visualize the result: skeleton overlaid on the point cloud
plot(archi,show_point_cloud = TRUE)

# 7. compute dominant axis of the tree
archi <- add_radius(archi, sec_length = 0.5, by_axis = TRUE, method = "median")
archi@QSM$radius_cyl

# volume estimation (way too much?!)
vol <- sum(archi@QSM$volume)
vol <- sum(subset(archi@QSM, archi@QSM$radius_cyl > 0.09)$volume)
