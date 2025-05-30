# code for a tree skeletonization in R like pypetree did in python
# Load required libraries
library(lidR)    # For point cloud processing
library(igraph)  # For graph operations
library(rgl)     # For 3D visualization
library(pracma)  # For geometric fitting

setwd("E:/Sonja/Msc_Thesis/data/9_individualTrees/")

library(lidR)      # For point cloud processing
library(RANN)      # For nearest neighbors
library(igraph)    # For graph operations
library(pracma)    # For geometric calculations

# ---- 1. Load point cloud data ----
las <- readLAS("tree45.las")
pts <- as.matrix(las@data[, c("X", "Y", "Z")])

# ---- 2. Compute nearest neighbors graph ----
k <- 6
r <- 0.5 # Adjust for your data
nn <- nn2(pts, k = k + 1, searchtype = "radius", radius = r)
edges <- data.frame()
for (i in 1:nrow(pts)) {
  for (j in 2:(k + 1)) {
    idx <- nn$nn.idx[i, j]
    if (idx > 0) {
      d <- nn$nn.dists[i, j]
      if (d < Inf) {
        edges <- rbind(edges, data.frame(from = i, to = idx, weight = d))
      }
    }
  }
}
g <- graph_from_data_frame(edges, directed = FALSE)

# ---- 3. Extract largest connected component ----
components <- components(g)
biggest <- which.max(components$csize)
keep <- which(components$membership == biggest)
g <- induced_subgraph(g, keep)
pts <- pts[keep, , drop = FALSE]

# ---- 4. Shortest path from lowest point (assume Y axis is up) ----
ydim <- 2 # 2 = Y in (X,Y,Z)
source <- which.min(pts[, ydim])
sp <- shortest_paths(g, from = source, output = "both")
distances <- distances(g, v = source)
max_path_len <- max(distances[is.finite(distances)])

# ---- 5. Level set segmentation ----
n_levels <- 10
level_size <- max_path_len / n_levels
level_sets <- vector("list", n_levels)
pt_to_level <- rep(NA, nrow(pts))
distances_vec <- distances(g, v = source)
for (i in 1:nrow(pts)) {
  d <- distances_vec[i]
  if (!is.finite(d)) next
  level <- min(n_levels, max(1, floor(d / level_size) + 1))
  level_sets[[level]] <- c(level_sets[[level]], i)
  pt_to_level[i] <- level
}

# ---- 6. Segment each level into connected components ----
segmentation <- list()
for (level in 1:n_levels) {
  inds <- level_sets[[level]]
  if (length(inds) < 3) next
  subg <- induced_subgraph(g, inds)
  segs <- components(subg)
  for (seg in unique(segs$membership)) {
    nodes <- which(segs$membership == seg)
    if (length(nodes) < 3) next
    segment_pts <- inds[nodes]
    centroid <- colMeans(pts[segment_pts, , drop = FALSE])
    segmentation <- append(segmentation, list(list(
      points = segment_pts,
      centroid = centroid,
      level = level
    )))
  }
}

# ---- 7. Estimate radius for each segment ----
for (i in seq_along(segmentation)) {
  seg_pts <- segmentation[[i]]$points
  centroid <- segmentation[[i]]$centroid
  dists <- sqrt(rowSums((pts[seg_pts, , drop = FALSE] - matrix(centroid, nrow = length(seg_pts), ncol = 3, byrow = TRUE))^2))
  segmentation[[i]]$radius <- mean(dists)
}

# ---- 8. Visualize skeleton ----
library(rgl)
open3d()
plot3d(pts, col = "grey", size = 2)
for (seg in segmentation) {
  spheres3d(seg$centroid[1], seg$centroid[2], seg$centroid[3], radius = seg$radius, color = "red", alpha = 0.5)
}
