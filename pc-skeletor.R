# from utility.py

# # vizualizations
# library(rgl)
# 
# visualize <- function(points, color = "blue", point_size = 3) {
#   # points: Nx3 matrix
#   open3d()
#   plot3d(points, col=color, size=point_size, type="s")
# }
# 
# #timing decorator
# timeit <- function(f) {
#   function(...) {
#     start <- Sys.time()
#     result <- f(...)
#     end <- Sys.time()
#     print(paste("Function took", round(as.numeric(difftime(end, start, units="secs")), 4), "seconds"))
#     return(result)
#   }
# }
# 
# #Numpy Array to Point Cloud
# points2pcd <- function(points) {
#   # Just return the matrix; visualization will use plot3d
#   return(points)
# }
# 
# #Load Point Cloud
# load_pcd <- function(filename) {
#   # For .xyz files
#   points <- as.matrix(read.table(filename))
#   return(points)
# }
# 
# # Normalize Point Cloud
# normalize_pcd <- function(points) {
#   bbox_min <- apply(points, 2, min)
#   bbox_max <- apply(points, 2, max)
#   scale <- 1.6 / max(bbox_max - bbox_min)
#   centered <- sweep(points, 2, colMeans(points))
#   normalized <- centered * scale
#   return(normalized)
# }
# 
# #Display Inliers/Outliers
# display_inlier_outlier <- function(points, inlier_idx) {
#   inliers <- points[inlier_idx, ]
#   outliers <- points[-inlier_idx, ]
#   open3d()
#   plot3d(inliers, col="blue", size=3, type="s")
#   points3d(outliers, col="red", size=3)
# }
# 
# # Generate GIF from Images
# library(magick)
# 
# generate_gif <- function(filenames, output_name) {
#   imgs <- image_read(filenames)
#   gif <- image_animate(imgs, fps = 10)
#   image_write(gif, paste0(output_name, ".gif"))
# }
# 
# #Simplify Graph
# library(igraph)
# 
# simplify_graph <- function(g) {
#   repeat {
#     deg <- degree(g)
#     nodes_to_remove <- which(deg == 2)
#     if (length(nodes_to_remove) == 0) break
#     for (node in nodes_to_remove) {
#       neighbors <- neighbors(g, node)
#       if (length(neighbors) == 2) {
#         g <- add_edges(g, c(neighbors[1], neighbors[2]))
#         g <- delete_vertices(g, node)
#       }
#     }
#   }
#   return(g)
# }
# 
# #Least Squares Sparse Solver
# library(Matrix)
# 
# least_squares_sparse <- function(pcd_points, laplacian, laplacian_weighting, positional_weighting, debug=FALSE) {
#   n <- nrow(pcd_points)
#   I <- Diagonal(n)
#   WL <- I * laplacian_weighting
#   WH <- Diagonal(x = positional_weighting)
#   A <- rbind(laplacian %*% WL, WH)
#   b <- rbind(matrix(0, n, 3), WH %*% pcd_points)
#   A_new <- t(A) %*% A
#   result <- matrix(NA, n, 3)
#   for (i in 1:3) {
#     result[,i] <- solve(A_new, t(A) %*% b[,i])
#   }
#   return(result)
# }




library(lidR)    # For LAS/LAZ point cloud processing
library(rgl)     # For 3D visualization
library(igraph)  # For graph operations
library(Matrix)  # For sparse matrices

# Load .las file
setwd("E:/Sonja/Msc_Thesis/data/9_individualTrees/")
las <- readLAS("tree45.las")
if (is.empty(las)) stop("LAS file is empty or could not be read.")

# Extract XYZ matrix
points <- as.matrix(las@data[, c("X", "Y", "Z")])

# Visualize
open3d()
plot3d(points, col = "grey", size = 2)

# Remove statistical outliers (optional, adjust parameters as needed)
#las <- decimate_points(las, homogenize(2)) # Decimate for uniformity
#las <- classify_noise(las, algorithm = ivf(5,2))        # Mark noise
#las <- filter_poi(las, Classification != LASNOISE)
#points <- as.matrix(las@data[, c("X", "Y", "Z")])

# Downsample to speed up computation
las_ds <- decimate_points(las, homogenize(30)) # 1cm grid, adjust as needed
points_ds <- as.matrix(las_ds@data[, c("X", "Y", "Z")])

library(FNN)
k <- 10
n <- nrow(points_ds)
if (k >= n) stop("k must be less than the number of points!")

knn <- get.knn(points_ds, k = k)
from <- rep(1:n, each = k)
to <- as.vector(knn$nn.index)
if (length(from) != length(to)) stop("Index vectors do not match in length!")

edges <- cbind(from, to)
g <- graph_from_edgelist(edges, directed = FALSE)

# Adjacency matrix
A <- as_adjacency_matrix(g, sparse = TRUE)
D <- Diagonal(x = rowSums(A))
L <- D - A  # Unnormalized Laplacian

# Laplacian contraction: minimize ||Lx||^2 + lambda*||x - x0||^2
lambda <- 100
n <- nrow(points_ds)
I <- Diagonal(n)
A_contr <- rbind(L, sqrt(lambda) * I)
b_contr <- rbind(matrix(0, n, 3), sqrt(lambda) * points_ds)

# Solve for new positions (contracted points)
contracted <- matrix(NA, n, 3)
for (i in 1:3) {
  contracted[, i] <- as.vector(solve(t(A_contr) %*% A_contr, t(A_contr) %*% b_contr[, i]))
}

# Optionally, prune degree-2 nodes to simplify the skeleton
deg <- degree(g)
to_keep <- which(deg != 2)
skeleton_points <- contracted[to_keep, ]

# Build new skeleton graph
skeleton_edges <- edges[edges[,1] %in% to_keep & edges[,2] %in% to_keep, ]
skeleton_g <- graph_from_edgelist(skeleton_edges, directed = FALSE)

open3d()
plot3d(points_ds, col = "grey")
points3d(skeleton_points, col = "red", size = 5)
