# Canopy Cover
cc_als <- raster("als/forest_metrics_lidar360/ALS_first_last_merged_shifted_CanopyCover.tif")
plot(cc_als)
cc_uav <- raster("uav/forest_metrics_LiDAR360/UAV_feb2_shifted_clipped_CanopyCover.tif")
plot(cc_uav)

cc_uav_resampled <- resample(cc_uav, cc_als, method = "bilinear")

stack_metrics <- stack(cc_uav_resampled, cc_als)
cor_matrix <- layerStats(stack_metrics, 'pearson', na.rm = TRUE)
# pearson correlation coefficient
#         UAV CC       ALS CC
# UAV CC  1.0000000    0.3107444
# ALS CC  0.3107444    1.0000000
#
# mean
# UAV CC        ALS CC 
# 0.4912051     0.6175862 

plot(cc_als-cc_uav_resampled, main = "Difference Map")
spplot(stack_metrics, col.regions = heat.colors(100))

corr_coeff <- cor_matrix$`pearson correlation coefficient`[2,1]
# [1] 0.3107444

p_value <- corLocal(cc_uav_resampled, cc_als, test = TRUE)[[2]]
plot(p_value)

cellStats(abs(cc_als - cc_uav_resampled), mean)
# [1] 0.1749809

cc_uav_rast <- rast(cc_uav_resampled)
cc_als_rast <- rast(cc_als)
cc_difference <- raster.change(cc_als_rast, cc_uav_rast, s = 5, stat ="t.test")
plot(cc_difference)

lm_model <- lm(as.matrix(cc_als_rast) ~ as.matrix(cc_uav_rast))
summary(lm_model)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.51866 -0.11326  0.01373  0.12254  0.57201 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              0.408054   0.005997   68.05   <2e-16 ***
# as.matrix(cc_uav_rast)   0.407749   0.011206   36.39   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1679 on 9636 degrees of freedom
# (1736 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.1208,	Adjusted R-squared:  0.1207 
# F-statistic:  1324 on 1 and 9636 DF,  p-value: < 2.2e-16
