################################################################################
#### Script for extracting the main stem of a tree and assessing its quality ###
################################################################################



################################ Workflow ######################################
  
  # 1. Prerequisites
  # 2. Functions
  # 3. Build aRchi object point cloud
  # 4. extract main stem
  # 5. segment main stem into parts with one-sided curvature and DBH > 15cm and 
  #    assess quality based on curvature and branches
  # 6. extract crown from aRchi object
  # 7. estimate volume of energy wood from crown
  # 8. Make neat conclusion table

  # TO DO: expand to work with multiple trees at once


############################ 1. Prerequisites ##################################


  ### 1.1 load required libraries
    library(aRchi)
    library(lidR)
    library(rgl)
    library(scatterplot3d)
    library(alphashape3d)
    library(data.table)
    library(circular)
    library(dplyr)
    library(stringr)
  
  ### 1.2 Load and prepare .las file
    setwd("E:/Sonja/Msc_Thesis/data/9_individualTrees/")
    
    # make list of all tree files
    tree_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
    
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
    
    # plot3d(read.csv(valid_trees[[1]]))
    # invalid trees: 1, (43), 74, 83
    
    
  ### 1.3 Initialize variables and objects
    # set minimum segment length (here: 3m, since its the min. sell-able length)
    min_segment_length <- 3  
    allowed_lengths <- c(3,4,6,10)
    
    # initialize segment stats table

    
    segment_stats_final <- data.frame(Tree = numeric(),
                                Segment = numeric(),
                                Length = numeric(),
                                Max_Deviation = numeric(), 
                                Class = character(),
                                Start_Z = numeric(), End_Z = numeric(),
                                Diameter = numeric(),
                                Diameter_noBark = numeric(),
                                Diameter_class = character(),
                                FirstBranch_height = numeric(),
                                radial_variance = numeric(),
                                volume = numeric(),
                                # Has_Branch = logical(),
                                Branch_Count = numeric())

    
    
    # set first branch variable to 0
    first_branch <- 0
    
    # Define a threshold for branch detection (adjust as needed)
    branch_threshold <- 0.33  # Adjust this 
    
    # set parameters for start slice of each segment
    slice_size <- 0.1
    min_points <- 15
    max_window <- 1.0
    
    # set parameters for middle slice of each segment
    slice_size <- 0.1
    min_points <- 10
    max_window <- 2.0
    
    crown_fm <- 0
  
  
  
########################## 2. initialize functions #############################
  
  
  ### 2.1 Function to fit a circle to 2D points 
    fitSS <- function(xy, a0 = mean(xy[, 1]), b0 = mean(xy[, 2]), 
                      r0 = mean(sqrt((xy[, 1] - a0)^2 + (xy[, 2] - b0)^2))) {
      SS <- function(abr) {
        sum((abr[3] - sqrt((xy[, 1] - abr[1])^2 + (xy[, 2] - abr[2])^2))^2)
      }
      optim(c(a0, b0, r0), SS)
    }
  
  
  ### 2.2 Function for detecting branches in aRchi object
    ForkRateWithHeights <- function(aRchi) {
      QSM <- as.data.table(archi_test@QSM)
      Paths <- as.data.table(archi_test@Paths)
      
      segment_table <- plyr::ddply(QSM, ("segment_ID "), function(x) {
        segment = x
        segment_P = QSM[segment_ID == unique(x$node_ID)]
        cbind(
          segment_ID = unique(segment$segment_ID),
          Angle = circular::deg(angle3d(
            as.numeric(segment[1, c("startX", "startY", "startZ")]),
            as.numeric(c(segment[1, c("startX", "startY")], 
                         segment[1, "startZ"] + 1)),
            as.numeric(segment[nrow(segment), c("endX", "endY", "endZ")]))),
          Angle_P = circular::deg(angle3d(
            as.numeric(segment_P[1, c("startX", "startY", "startZ")]),
            as.numeric(c(segment_P[1, c("startX", "startY")], 
                         segment_P[1, "startZ"] + 1)),
            as.numeric(segment_P[nrow(segment_P), c("endX", "endY", "endZ")]))),
          radius = segment[1, "radius_cyl"],
          radius_parent = segment_P[nrow(segment_P), "radius_cyl"],
          length = sum(segment$length),
          length_parent = sum(segment_P$length),
          z_min = segment[1, "startZ"],
          z_max = segment[nrow(segment), "endZ"],
          node_ID = x$node_ID[1]
        )
      })
      
      names(segment_table)[5] <- "radius_parent"
      
      # Search for the maximum radius between the daughters of a same node
      node_max_radius_table <- plyr::ddply(segment_table,
                                          ("node_ID"),
                                          dplyr::summarise,
                                          node_max_radius = max(radius))
      
      # Add it to the segment table
      segment_table <- data.table::data.table(merge(segment_table, 
                                                   node_max_radius_table, 
                                                   by = "node_ID"))
      
      # Compute daughter radius/ max(daughter radius)
      segment_table$percent_diam <- segment_table$radius / 
        segment_table$node_max_radius
      
      # Retrieve Paths table with all the variable of Segment table
      Perenial_structure_table <- merge(segment_table, 
                                       unique(Paths[, c("segment_ID", 
                                                        "ID_Path")]), 
                                       by = "segment_ID")
      
      # Keep only paths for which  daughter radius / max(daughter radius) > to the 
      # threshold
      Perenial_structure_table <- data.table::data.table(dplyr::anti_join(
        Perenial_structure_table,
        unique(Perenial_structure_table[percent_diam < 0.75, "ID_Path"]), 
        by = "ID_Path"))
      Perenial_structure_table <- unique(Perenial_structure_table[, -"ID_Path"])
      
      cleanedQSM <- QSM[segment_ID %in% Perenial_structure_table$segment_ID]
      
      # Rectification of the segment and node ID following the suppression of 
      # certain subtree
      # How many daughters per node ?
      nb_br_node <- cleanedQSM[, .(n_branches = length(unique(segment_ID)),
                                  fork_startZ = min(startZ, na.rm = TRUE)), 
                               by = "node_ID"]
      
      # How many fork in the QSM
      N_Fork = nrow(nb_br_node[n_branches > 1])
      Forkrate = N_Fork / (max(QSM$startZ) - min(QSM$endZ))
      Forkheights <-  nb_br_node[n_branches > 1, fork_startZ]
      return(c(
        N_Fork = N_Fork,
        Forkrate = Forkrate,
        Forkheights = Forkheights
      ))
    }
  
  
  ### 2.3 Function to compute radial variance
    compute_radial_variance <- function(points, x_center, y_center) {
      # Compute distances of points from the estimated center
      distances <- sqrt((points[, 1] - x_center)^2 + (points[, 2] - y_center)^2)
      
      # Compute mean radius
      mean_radius <- mean(distances)
      
      # Compute radial variance
      radial_variance <- mean((distances - mean_radius)^2)
      
      return(radial_variance)
    }
  
    
  ### 2.4 Function to sort segments into RVR classes based on branches & curvature
    rvr_classification <- function(Length,
                                   Max_Deviation,
                                   Diameter_noBark,
                                   branches) {
      class <- ""
      if (!is.na(Max_Deviation) && !is.na(Length) && !is.na(branches) &&
          Length != 0 && (Max_Deviation / Length) <= 0.02 &&
          (branches / Length) <= 0.5) {
        class <- "A"
      } else if (!is.na(Max_Deviation) && !is.na(Length) && !is.na(branches) && (Max_Deviation / Length) <= 0.03) {
        class <- "B"
      } else if (!is.na(Max_Deviation) && !is.na(Length) && !is.na(branches) && (Max_Deviation / Length) > 0.03 &&
                 (Max_Deviation / Length) <= 0.04 && Diameter_noBark <= 0.49) {
        class <- "C"
      } else if (!is.na(Max_Deviation) && !is.na(Length) &&!is.na(branches) && (Max_Deviation / Length) > 0.04 &&
                 (Max_Deviation / Length) <= 0.06 &&
                 Diameter_noBark >= 0.5) {
        class <- "C"
      } else if (!is.na(Max_Deviation) && !is.na(Length) && !is.na(branches) && (Max_Deviation / Length) >= 0.06){
        class <- "D"
      } else {class <- ""}
      return(class)
    }
  
  ### 2.5 Function to determine diameter class
    diameter_classification <- function(Diameter_noBark) {
      if (Diameter_noBark < 0) {
        diameter_class <- NA
      } else if (Diameter_noBark >= 0 & Diameter_noBark <= 0.09) {
        diameter_class <- "0"
      } else if (Diameter_noBark > 0.09 & Diameter_noBark <= 0.14) {
        diameter_class <- "1a"
      } else if (Diameter_noBark > 0.14 & Diameter_noBark <= 0.19) {
        diameter_class <- "1b"
      } else if (Diameter_noBark > 0.19 & Diameter_noBark <= 0.24) {
        diameter_class <- "2a"
      } else if (Diameter_noBark > 0.24 & Diameter_noBark <= 0.29) {
        diameter_class <- "2b"
      } else if (Diameter_noBark > 0.29 & Diameter_noBark <= 0.34) {
        diameter_class <- "3a"
      } else if (Diameter_noBark > 0.34 & Diameter_noBark <= 0.39) {
        diameter_class <- "3b"
      } else if (Diameter_noBark > 0.39 & Diameter_noBark <= 0.49) {
        diameter_class <- "4"
      } else if (Diameter_noBark > 0.49 & Diameter_noBark <= 0.59) {
        diameter_class <- "5"
      } else if (Diameter_noBark > 0.59 & Diameter_noBark <= 0.69) {
        diameter_class <- "6"
      } else if (Diameter_noBark > 0.69 & Diameter_noBark <= 0.79) {
        diameter_class <- "7"
      } else {
        diameter_class <- "8"
      }
    }
    
    
  ### 2.6 Function to check if points are within a cylinder
    points_in_cylinder <- function(points, start, end, max_radius,
                                   min_radius, buffer = 0) {
      # Vector along axis
      axis_vec <- end - start
      axis_len <- sqrt(sum(axis_vec^2))
      if (axis_len < 1e-6)
        return(rep(FALSE, nrow(points))) # skip degenerate
      axis_unit <- axis_vec / axis_len
      
      v <- sweep(points, 2, start) # points - start
      t_proj <- v %*% axis_unit    # projection along axis
      
      # Points within the length of the cylinder
      mask_length <- (t_proj >= 0) & (t_proj <= axis_len)
      
      # Closest point on axis to each point
      closest <- sweep(matrix(t_proj, ncol = 1) %*% t(axis_unit), 2, start, "+")
      dist_to_axis <- sqrt(rowSums((points - closest)^2))
      mask_radius <- (dist_to_axis > (min_radius + buffer)) &
        (dist_to_axis <= (max_radius + buffer))
      mask <- mask_length & mask_radius
      return(mask)
    }
  
    
  ### 2.7 Function to find the fitting cylinder for a point
    find_cylinder_for_point <- function(point, qsm_table) {
      
      p <- as.numeric(point)
      
      for (i in 1:nrow(qsm_table)) {
        
        # Cylinder axis
        start <- as.numeric(qsm_table[i, c("startX", "startY", "startZ")])
        end   <- as.numeric(qsm_table[i, c("endX", "endY", "endZ")])
        radius <- qsm_table[i, "radius_cyl"]
        
        # Vector from start to end
        axis_vec <- end - start
        axis_len <- sqrt(sum(axis_vec^2))
        axis_dir <- axis_vec / axis_len
        if (axis_len < 1e-6)
          return(rep(FALSE, nrow(points))) # skip degenerate
        
        # Vector from start to point
        v <- as.numeric(point) - start
        proj_len <- sum(v * axis_dir)
        # Check if projection is within cylinder segment
        if (proj_len >= 0 && proj_len <= axis_len) {
          # Closest point on axis
          closest <- start + proj_len * axis_dir
          dist_to_axis <- sqrt(sum((as.numeric(point) - closest)^2))
          if (dist_to_axis <= radius) {
            return(i) # Cylinder index
          }
        }
      }
      return(NA) # No containing cylinder found
    }


##################### 3. Build aRchi object from point cloud ###################
# l <- 9
for (l in 1:length(valid_trees)){
  
  segment_stats <- data.frame(Tree = numeric(),
                              Segment = numeric(),
                              Length = numeric(),
                              Max_Deviation = numeric(), 
                              Class = character(),
                              Start_Z = numeric(), End_Z = numeric(),
                              Diameter = numeric(),
                              Diameter_noBark = numeric(),
                              Diameter_class = character(),
                              FirstBranch_height = numeric(),
                              radial_variance = numeric(),
                              volume = numeric(),
                              # Has_Branch = logical(),
                              Branch_Count = numeric())
  
  # empty list for the final segments
  final_segments <- list()
  #i <- 2
    
    slice_size <- 0.1
  
  ### 3.1 Read data and clean
    data <- read.csv(valid_trees[[l]])
    
    # remove unneccesary columns
    data[,4:15] <- NULL
    
    # convert to .las
    las <- LAS(data)
    
    # set tree ID
    tree_i <- as.numeric(str_extract(valid_trees[[l]], "\\d+"))
    
    # filter outliers to correct possible errors in clustering
    las <- classify_noise(las, sor(10,3))
    las <- filter_poi(las, Classification != 18)
    
    
  ### 3.2 Build aRchi object
    archi <- build_aRchi()
    archi <- add_pointcloud(archi, point_cloud = las)
    
    
  ### 3.3 skeletonize aRchi object and add radius
    archi <- skeletonize_pc(archi, D = 1, cl_dist = 0.3, max_d = 1.5,
      progressive = TRUE)

    
  ### 3.4 Add radius to aRchi object  
    archi <- add_radius(archi, sec_length = 0.5, by_axis = TRUE, 
                        method = "median")
  
    
  ### 3.5 Calculate paths of the aRchi object
    archi_test <- Make_Path(archi)
  
    
  ### 3.6 Clean the QSM table
    Clean_QSM(archi_test, threshold = 0.1, plotresult = FALSE)
    
  ### 3.7 Get branch heights
    fork_heights <- ForkRateWithHeights(archi_test)
  
  
  ### 3.8 Extract QSM table
    qsm_table <- archi_test@QSM


    
############ 4. Identify main stem (axis_ID == 1) and extract points ###########

  ### 4.1 subset the QSM table for main axis (axisID = 1)
    main_stem <- subset(qsm_table, axis_ID == 1)  

    
  ### 4.2 Extract start/end coordinates of main stem cylinders
    main_stem_points <- rbind(
      as.matrix(main_stem[, c("startX", "startY", "startZ")]),
      as.matrix(main_stem[, c("endX", "endY", "endZ")])
    )
  
    
  ### 4.3 Remove duplicates (if needed)
    main_stem_points <- unique(main_stem_points)
    # crown_points <- unique(crown_points)

    
    # # Plot main stem axis to check for correctness
    # plot <- scatterplot3d(main_stem_points,
    #               color = "red",
    #               pch = 16,
    #               main = "Main Stem Axis",
    #               xlab = "X", ylab = "Y", zlab = "Z")
    # 
    # # Add original point cloud in background (grey)
    # original_points <- as.matrix(archi@pointcloud@data[, c("X", "Y", "Z")])
    # plot$points3d(original_points,
    #               color = "grey")

    
  ### 4.4 get all "stem" points from original cloud
    # Get original point cloud as a matrix
    pc <- as.matrix(archi@pointcloud@data[, c("X", "Y", "Z")])

    # generate empty matrix
    in_main_axis <- rep(FALSE, nrow(pc))
  
    # check if points are within the determined main axis cylinders
    for (m in seq_len(nrow(main_stem))) {
      
      cyl <- main_stem[m, ]
      start <- as.numeric(c(cyl$startX, cyl$startY, cyl$startZ))
      end <- as.numeric(c(cyl$endX, cyl$endY, cyl$endZ))
      radius <- cyl$radius_cyl
      axis_vec <- end - start
      axis_len <- sqrt(sum(axis_vec^2))
      
      if (axis_len < 1e-6) next  # skip degenerate cylinders
      
      mask <- points_in_cylinder(pc, start, end, 
                                 max_radius = radius+0.1, min_radius = 0, 
                                 buffer = 0)
      cat("Cylinder", m, "selected", sum(mask), "points\n")
      
      in_main_axis <- in_main_axis | mask
    }
  
    # gather main axis points
    stem_points <- pc[in_main_axis, ]
  
    # cat("Total main axis points found:", nrow(main_axis_points), "\n")
  
  
  
############################# 5. Segment stem points ###########################


  ### 5.1 PCA for main stem axis 
  
    # find the dominant direction of the data which corresponds with the general 
    # orientation of the stem, enabling to analyze the trees shape independently 
    # of its initial orientation in the data set.
    
    # Principal component analysis
    pca <- prcomp(stem_points, center = TRUE)
    axis_vector <- pca$rotation[,1]
    stem_center <- colMeans(stem_points)
    
    # Project stem points onto PCA axis to simplify curvature analysis and allow 
    # to sort points by height.
    projected_points <- apply(stem_points, 1, function(pt) {
      t <- sum((pt - stem_center) * axis_vector)
      stem_center + t * axis_vector
    })
    projected_points <- t(projected_points)
  
  
  ### 5.2 Sort stem points by height for segmentation ##
  
    sorted_indices <- order(stem_points[,3])  # Sort by Z (height)
    stem_sorted <- stem_points[sorted_indices, ]
    projected_sorted <- projected_points[sorted_indices, ]
    
  
  ### 5.3 Compute local curvature ##
  
    deviations <- stem_sorted[,1:2] - projected_sorted[,1:2]  # XY-plane dev.
    signed_curvature <- deviations[,1] * axis_vector[2] - 
      deviations[,2] * axis_vector[1]  # Signed curvature in 2D
    
    smooth_curvature <- function(curvature, window = 5) {
      filter(curvature, rep(1 / window, window), sides = 2)
    }
    
    # Apply smoothing 
    # smoothed_curvature <- smooth_curvature(signed_curvature, window = 5)
    
  
  ### 5.4 Segment the stem into sections with constant curvature sign 
  
    # Points where curvature sign changes
    # change_points <- which(diff(sign(smoothed_curvature)) != 0) 
    
    # without smoothing
    change_points <- which(diff(sign(signed_curvature)) != 0) 
    
    # Start and end of segments
    segment_indices <- c(1, change_points, nrow(stem_sorted))  
    
    # Initialize a new list of segment start indices
    merged_segment_indices <- c(segment_indices[1])
    
    
    n <- 2
    # segment the stem 
    while (n < length(segment_indices)) {
      start_idx <- segment_indices[n]
      segment_length <- 0
      j <- n  # Keep track of a separate index for merging
      
      while (j < length(segment_indices) &&
             segment_length < min_segment_length) {
        j <- j + 1
        if (j > length(segment_indices))
          break  # Prevent out-of-bounds indexing
        end_idx <- segment_indices[j]
        segment_length <- abs(stem_sorted[end_idx, 3] - stem_sorted[start_idx, 3])
      }
      
      # Find the closest allowed segment length (≥ 3m)
      possible_lengths <- allowed_lengths[allowed_lengths >= segment_length]
      if (length(possible_lengths) > 0 & segment_length >= 3) {
        final_segments <- rbind(final_segments, c(start_idx, end_idx))
      }
      n <- j  # Move to the next unmerged segment
    }
    
    # Convert to a proper list of indices
    merged_segment_indices <- do.call(rbind, final_segments[,1])
    merged_segment_indices <- unique(merged_segment_indices)
    
    # Assign new segment labels
    segment_labels <- rep(0, nrow(stem_sorted))
    
    # Convert segment labels to positive integers
    segment_labels <- as.integer(segment_labels) + 1  # Offset to avoid zeros
    
  
  ### 5.6 Compute segment parameters 
    
    # initialize lists for export
    las_segments_list <- list()
    las_centerlines_list <- list()
    mid_slices_list <- list()
    mid_circles_list <- list()
  
    # find center line of each stem segment and calculate parameters
    for (o in 1:(length(merged_segment_indices) - 1)) {
      
      segment_i <- stem_sorted[merged_segment_indices[o]:
                                 merged_segment_indices[o + 1], ]
      
      # initialize centerline dataframe
      centerline <- data.frame(Z = numeric(), Xc = numeric(), Yc = numeric())
      
      # Define height slices
      slice_heights <- seq(min(segment_i[, 3]), max(segment_i[, 3]), by = 0.1)
      
      # loop through slices
      for (h in slice_heights) {
        slice <- subset(segment_i, segment_i[, 3] >= h - 0.1 &
                          segment_i[, 3] < h + 0.1)
        if (nrow(slice) > 3) {
          # Ensure enough points for fitting
          circle <- fitSS(slice)
          center <- cbind(circle$par[[1]], circle$par[[2]])
          centerline <- rbind(centerline, 
                              data.frame(Z = h, 
                                         Xc = center[1, 1], 
                                         Yc = center[1, 2]))
        }
      }
      
      # Create LAS data with RGB attributes for segment
      las_data <- data.frame(X = segment_i[, 1], 
                             Y = segment_i[, 2], 
                             Z = segment_i[, 3])
      
      # Create LAS data with RGB attributes for center line
      las_data2 <- data.frame(X = centerline$Xc, 
                              Y = centerline$Yc, 
                              Z = centerline$Z)
      
      # # filter for outliers in data frame
      # Q1_x <- quantile(las_data2$X, .25)
      # Q3_x <- quantile(las_data2$X, .75)
      # IQR_x <- IQR(las_data2$X)
      # las_data2 <- subset(las_data2,
      #                     las_data2$X > (Q1_x - 1.5 * IQR_x) &
      #                       las_data2$X < (Q3_x + 1.5 * IQR_x))
      
      # filter outliers with boxplot method
      outliers_las_data2_x <- boxplot.stats(las_data2$X)$out
      outliers_las_data2_y <- boxplot.stats(las_data2$Y)$out
      outliers_las_data2_z <- boxplot.stats(las_data2$Z)$out
      las_data2 <- las_data2[!(las_data2$X %in% outliers_las_data2_x |las_data2$Y %in% outliers_las_data2_y | las_data2$Z %in% outliers_las_data2_z),]
      las_data2 <- subset(las_data2, X >= 0 & Y >= 0 & Z >= 0)
      
      las_segment <- LAS(las_data)
      las_centerline <- LAS(las_data2)
      
      
      las_segments_list[[o]] <- las_segment
      las_centerlines_list[[o]] <- las_centerline
      
      # compute segment parameters
      segment_start <- merged_segment_indices[o]
      segment_end <- merged_segment_indices[o + 1]
      
      if (is.na(segment_start) | is.na(segment_end)) {
        # print(paste("Warning: NA values found in segment indices at row", i))
        next  # Skip this segment
      }
      
      # calculate segment length
      segment_length <- abs(stem_sorted[segment_end, 3] - 
                              stem_sorted[segment_start, 3])
      
      repeat {
        start_slice <- subset(segment_i,
                              segment_i[, 3] >= stem_sorted[segment_start, 3] &
                                segment_i[, 3] < 
                                (stem_sorted[segment_start, 3] + slice_size))
        # Check if enough points are selected or window is too large
        if (nrow(start_slice) >= min_points ||
            slice_size >= max_window) {
          break
        }
        # Increase window size incrementally
        slice_size <- slice_size + 0.05
      }
      
      # calculate segment diameter at the start of the segment
      start_circle <- fitSS(start_slice)
      diameter_start <- start_circle[[1]][3] * 2
      
      # exclude segments with a smaller starting diameter than 15cm
      # if (diameter_start < 0.15) {
      #   segment_crown <- rbind(segment_crown, segment_i)
      #   next
      # }
      
      repeat {
        mid_z <- (stem_sorted[segment_end, 3] +
                    stem_sorted[segment_start, 3]) / 2
        mid_slice <- subset(segment_i,
                            segment_i[, 3] >= (mid_z - slice_size) &
                              segment_i[, 3] < (mid_z + slice_size))
        
        # Check if enough points are selected or window is too large
        if (nrow(mid_slice) >= min_points ||
            slice_size >= max_window) {
          break
        }
        # Increase window size incrementally (e.g., by 0.05 m)
        slice_size <- slice_size + 0.05
      }
      
      mid_circle <- fitSS(mid_slice)
      diameter <- mid_circle[[1]][3] * 2
      
      # calculate diameter without bark
      diameter_noBark <- if (diameter <= 0.41) {
        diameter - 0.01
      } else {
        diameter - 0.02
      }
      
      # diameter class
      diameter_class <- diameter_classification(diameter_noBark)
      
      mid_slices_list[[o]] <- mid_slice
      mid_circles_list[[o]] <- mid_circle
      
      # calculate Curvature as deviation from a straight line
      if (nrow(centerline) > 2) {
        # Ensure there are enough points
        
        # First and last points of centerline
        P1 <- as.numeric(centerline[1, c("Xc", "Yc", "Z")])
        P2 <- as.numeric(centerline[nrow(centerline), c("Xc", "Yc", "Z")])
        
        # Compute distances of each centerline point to the line P1-P2
        distances <- sapply(1:nrow(centerline), function(j) {
          P <- as.numeric(centerline[j, c("Xc", "Yc", "Z")])
          
          # Vector representations
          v <- P2 - P1  # Line direction vector
          w <- P - P1   # Vector from P1 to P
          
          # Projection of w onto v
          projection_factor <- sum(w * v) / sum(v * v)
          projection <- P1 + projection_factor * v  # Closest point on the line
          
          # Perpendicular distance
          sqrt(sum((P - projection)^2))
        })
        
        # Maximum deviation
        max_curvature <- max(distances, na.rm = TRUE)
      }
      
      # calculate radial variance for each segment
      radial_variance <- compute_radial_variance(mid_slice, mid_circle[[1]][1], 
                                                 mid_circle[[1]][2])
      
      # calculate volume of segment (fm)
      # segment_i_df <- as.data.frame(segment_i)  # Ensure it's a data frame
      # segment_i_df[, 1:3] <- sweep(segment_i_df[, 1:3], 2, 
      #                              colMeans(segment_i_df[, 1:3]), "-")
      # ashape <- ashape3d(as.matrix(segment_i_df), alpha = 1)
      # fm <- volume_ashape3d(ashape)
      fm <- pi * (diameter_noBark/2)^2 * as.numeric(segment_length)
      
      branches <- sum(fork_heights[c(3:length(fork_heights))] >= 
                        stem_sorted[segment_start, 3] &
                        fork_heights[c(3:length(fork_heights))] <  
                        stem_sorted[segment_end, 3])
      if(is.na(branches)){branches <- 0}
      
      # rvr classification
      class <- rvr_classification(segment_length, max_curvature, 
                                  diameter_noBark, branches)

      # make a plot to vizualise the stem segment cross section
      circlexy <- function(xyr, n = 180) {
        theta = seq(0, 2 * pi, len = n)
        cbind(xyr[1] + xyr[3] * cos(theta), xyr[2] + xyr[3] * sin(theta))
      }
      
      # collect segment stats in table
      segment_stats <- rbind(segment_stats,
          data.frame(
          Tree = tree_i,
          Segment = o,
          Length = segment_length,
          Max_Deviation = max_curvature,
          Class = class,
          Start_Z = stem_sorted[segment_start, 3],
          End_Z = stem_sorted[segment_end, 3],
          Diameter = diameter,
          Diameter_noBark = diameter_noBark,
          Diameter_class = diameter_class,
          radial_variance = round(radial_variance, digits = 10),
          volume = fm,
          Branch_Count = branches
      ))
    } 
    

  ### 5.7 Plausibility check for segments
    plausible <- logical(nrow(segment_stats))
    plausible[1] <- TRUE  # First segment is always plausible
    last_plausible <- 1

    for (i in 2:nrow(segment_stats)) {
      plausible[i] <- segment_stats$Diameter_noBark[i] <
        segment_stats$Diameter_noBark[last_plausible] * 1.2 
      if(plausible[i]){
        last_plausible <- i
      }
    }
    
    for (i in length(plausible):1) {  # Iterate backwards!
      if(plausible[i]) {
        writeLAS(las_segments_list[[i]], paste("tree_", tree_i, "_segment_", i, ".las"))
        writeLAS(las_centerlines_list[[i]], paste("tree_", tree_i, "_centerline_", i, ".las"))
        plot(
          mid_slices_list[[i]],
          asp = 1,
          main = paste("Centre cross-section of segment ", i),
          sub = paste("Diameter: ", round(segment_stats$Diameter_noBark[i], 3), "m")
        )
        lines(circlexy(mid_circles_list[[i]]$par))
      } else {
        # Find next plausible segment
        replacement <- NA
        if (i < length(plausible)) {
          for (j in (i+1):length(plausible)) {
            if (isTRUE(plausible[j])) {
              replacement <- j
              break
            }
          }
        }
        if (!is.na(replacement)) {
          segment_stats[i, "Diameter"] <- segment_stats[replacement, "Diameter"]
          segment_stats[i, "Diameter_noBark"] <- if (segment_stats[replacement, "Diameter"] <= 0.41) {
            segment_stats[replacement, "Diameter"] - 0.01
          } else {
            segment_stats[replacement, "Diameter"] - 0.02
          }
          segment_stats[i, "Diameter_class"] <- diameter_classification(segment_stats[i, "Diameter_noBark"])
          segment_stats[i, "volume"] <- pi * (segment_stats[i, "Diameter_noBark"]/2)^2 * segment_stats[i, "Length"]
          segment_stats[i, "Class"] <- rvr_classification(
            segment_stats[i, "Length"],
            segment_stats[i, "Max_Deviation"],
            segment_stats[i, "Diameter_noBark"],
            segment_stats[i, "Branch_Count"]
          )
          plausible[i] <- TRUE  # Optionally mark as plausible now
        } else {
          # Delete row i from all relevant objects
          segment_stats <- segment_stats[-i, ]
          plausible <- plausible[-i]
          las_segments_list <- las_segments_list[-i]
          las_centerlines_list <- las_centerlines_list[-i]
          mid_slices_list <- mid_slices_list[-i]
          mid_circles_list <- mid_circles_list[-i]
        }
      }
    }
    
    
    if(!isTRUE(plausible[length(plausible)])) {
      segment_stats <- segment_stats[-length(plausible), ]
      # If you have associated lists, remove the last element as well:
      las_segments_list <- las_segments_list[-length(plausible)]
      las_centerlines_list <- las_centerlines_list[-length(plausible)]
      mid_slices_list <- mid_slices_list[-length(plausible)]
      mid_circles_list <- mid_circles_list[-length(plausible)]
    }
    
    segment_stats_final <- rbind(segment_stats_final, segment_stats)
    
    print(segment_stats_final)
    
    cat("Total Stem Volume: ", 
        round(sum(segment_stats_final$volume), 
              digits = 3), "m³", "\n" , 
        "Volume Quality A: ", 
        round(sum(subset(segment_stats_final, segment_stats_final$Class == "A")$volume),
              digits = 3), "m³", "\n",
        "Volume Quality B: ", 
        round(sum(subset(segment_stats_final, segment_stats_final$Class == "B")$volume), 
              digits = 3), "m³", "\n",
        "Volume Quality C: ", 
        round(sum(subset(segment_stats_final, segment_stats_final$Class == "C")$volume), 
              digits = 3), "m³", "\n",
        "Volume Quality D: ", 
        round(sum(subset(segment_stats_final, segment_stats_final$Class == "D")$volume), 
              digits = 3), "m³")



########################### 6. Extract crown points ############################

    archi_ForCrown <- build_aRchi()
    archi_ForCrown <- add_pointcloud(archi_ForCrown, point_cloud = las)
    
    
    ### 3.3 skeletonize aRchi object and add radius
    archi_ForCrown <- skeletonize_pc(archi_ForCrown, D = 0.5, cl_dist = 0.3, max_d = 1.5,
                            progressive = TRUE)
    # plot(archi_ForCrown)
    ### 3.4 Add radius to aRchi object  
    archi_ForCrown <- add_radius(archi_ForCrown, sec_length = 0.5, by_axis = TRUE, 
                        method = "median")
    
    
    ### 3.5 Calculate paths of the aRchi object
    archi_test <- Make_Path(archi_ForCrown)
    
    
    ### 3.6 Clean the QSM table
    Clean_QSM(archi_test, threshold = 0.1, plotresult = FALSE)
    qsm_table_ForCrown <- archi_ForCrown@QSM
    
    
    
    qsm_segment_summary <- qsm_table_ForCrown %>%
      group_by(segment_ID) %>%
      summarise(
        axis_ID = first(axis_ID),                  # Retain axis_ID for the segment
        branching_order = first(branching_order),  # Retain branching_order
        total_length = sum(length, na.rm = TRUE),  # Sum length for the segment
        mean_radius = mean(radius_cyl, na.rm = TRUE), # Mean radius for the segment
        volume = pi * mean_radius^2 * total_length # Calculate volume as cylinder
      )
    
    # View the result
    # print(qsm_segment_summary)
    v <- sum(subset(qsm_segment_summary, branching_order == 2 & axis_ID > 1 & total_length >= 1 & mean_radius >= 0.15)$volume)

  crown_fm <- crown_fm + v
  
########################### 7. Make Conclusion Table ###########################
  
  # 1. Column: Quality Class
  # 2. Column: Volume of the respective class
  
  summary <- segment_stats_final %>%
    group_by(Class) %>%
    summarize(Volume = sum(volume))
  
  sum(segment_stats_final$volume)

}
  