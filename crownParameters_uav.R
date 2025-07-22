##### script for calculating the crown parameters from segmented uav data ######

library(lidR)
library(data.table)
library(sf)

las_folder <- "E:/Sonja/Msc_Thesis/data/9_individualTrees/uav_trees_manual/matched/"
las_files <- list.files(las_folder, pattern = "\\.las$", full.names = TRUE)

get_tree_metrics <- function(file) {
  las <- readLAS(file)
  #if (is.empty(las)) return(NULL)
  
  # Normalize height (ground = 0)
  #las <- normalize_height(las, tin())
  
  # Filter ground returns and points above a minimum height (e.g., 2m)
  las <- filter_poi(las, Z > 2)
  
  #if (is.empty(las)) return(NULL)
  
  # Compute convex hull of the crown
  ch <- st_convex_hull(las)
    
  
  #if (is.null(ch)) return(NULL)
  
  # Tree height
  height <- max(las$Z, na.rm = TRUE)
  
  # Crown diameter (bounding box diagonal or max distance between points)
  xy <- las@data[, .(X, Y)]
  chull_poly <- concaveman(xy, length_threshold = 1)
  n <- nrow(chull_poly)
  opposite_distances <- c()
  for (i in 1:floor(n / 2)) {
    j <- (i + floor(n / 2)) %% n
    j <- ifelse(j == 0, n, j)
    dist <- sqrt(sum((chull_poly[i, ] - chull_poly[j, ])^2))
    opposite_distances <- c(opposite_distances, dist)
  }
  diameter <- mean(opposite_distances)
  
  # Crown area from convex hull
    pol <- st_sfc(st_polygon(list(cbind(chull_poly[,1], chull_poly[,2]))))
  crown_area <- st_area(pol)

  # Crown volume approximation (paraboloid shape)
  #crown_volume <- (1/2) * crown_area * height
  
  data.table(
    file_name = basename(file),
    tree_height = round(height, 3),
    crown_diameter = round(diameter, 3),
    crown_area = round(crown_area, 3)
    #crown_volume = round(crown_volume, 2)
  )
}

tree_metrics_list <- lapply(las_files, get_tree_metrics)
tree_metrics <- rbindlist(tree_metrics_list, fill = TRUE)
write.csv(tree_metrics, "E:/Sonja/Msc_Thesis/data/Metrics/uav/crown_parameters.csv")
