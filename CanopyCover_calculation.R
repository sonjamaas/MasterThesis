################################################################################
############### Script to calculate canopy cover from LiDAR data ###############
################################################################################

#### Workflow ####
# 1. Prerequisites
# 2. Define Functions
# 3. Calculate Canopy Cover
# 4. Clip to extent
# 5. Save raster files


############################# 1. Prerequisites #################################

# load required libraries
library(lidR)

# read files
setwd("E:/Sonja/Msc_Thesis/data/8_preprocessedData/")

las_bp <- 
  readLAS("bp/backpack_feb5_subsampled_0_05_Normalize by Ground Points.las")
las_uav <- 
  readLAS("uav/UAV_feb2_shifted_clipped.las")
las_als <- 
  readLAS("als/ALS_first_last_merged_shifted.las")
las_tls <- 
  readLAS("tls/TLS_Hain_20_21_subsample_0_05_Normalize by Ground Points.las")
plot(las_tls)

############################# 2. Define Function ###############################

# function for canopy cover
myCanopyCover <- function(z, rn) {
  first = rn == 1L
  zfirst = z[first]
  nfirst = length(zfirst)
  above2 = sum(zfirst > 10)
  cover = (above2 / nfirst) * 100
  return(cover)
}


####################### 3. Calculate Canopy Cover ##############################

cover_grid_bp <- grid_metrics(las_bp, 
                              ~myCanopyCover(Z, ReturnNumber), res = 0.6)
cover_grid_uav <- grid_metrics(las_uav, 
                               ~myCanopyCover(Z, ReturnNumber), res = 0.6)
cover_grid_als <- grid_metrics(las_als, 
                               ~myCanopyCover(Z, ReturnNumber), res = 0.6)
cover_grid_tls <- grid_metrics(las_tls, 
                               ~myCanopyCover(Z, ReturnNumber), res = 0.6)


###################### 4. Clip to same extent ##################################

cover_als_clipped <- crop(cover_grid_als, cover_grid_uav)
cover_uav_clipped <- crop(cover_grid_uav, cover_als_clipped)
cover_bp_clipped <- crop(cover_grid_bp, cover_als_clipped)
cover_tls_clipped <- crop(cover_grid_tls, cover_als_clipped)

cover_uav_clipped <- crop(cover_uav_clipped, cover_tls_clipped)
cover_als_clipped <- crop(cover_als_clipped, cover_tls_clipped)


######################## 5. Save raster files ##################################
writeRaster(cover_als_clipped, "cover_als.tif", overwrite=TRUE)
writeRaster(cover_uav_clipped, "cover_uav.tif", overwrite=TRUE)
writeRaster(cover_bp_clipped, "cover_bp.tif", overwrite=TRUE)
writeRaster(cover_tls_clipped, "cover_tls.tif", overwrite=TRUE)