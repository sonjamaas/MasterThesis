############## Script for comparing gap fraction rasters #######################

setwd("E:/Sonja/Msc_Thesis/data/Metrics/GapFraction/")


# LAI
gf_uav <- rast("gf_uav.tif")
gf_als <- rast("gf_als.tif")
gf_tls <- rast("gf_tls.tif")
gf_bp <- rast("gf_bp.tif")

plot(gf_tls)

plot(gf_uav)
ggplot(data = as.data.frame(gf_uav, xy = TRUE), aes(x = x, y = y, fill = gf_uav))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    limits = c(0,1),
    name = "Gap Fraction"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "Gap Fracion derived from UAV data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))

plot(gf_als)
ggplot(data = as.data.frame(gf_als, xy = TRUE), aes(x = x, y = y, fill = gf_als))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    limits = c(0,1),
    name = "Gap Fraction"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "Gap Fracion derived from ALS data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))

plot(gf_tls)
ggplot(data = as.data.frame(gf_tls, xy = TRUE), aes(x = x, y = y, fill = gf_tls))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    limits = c(0,1),
    name = "Gap Fraction"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "Gap Fracion derived from TLS data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))

plot(gf_bp)
ggplot(data = as.data.frame(gf_bp, xy = TRUE), aes(x = x, y = y, fill = gf_bp))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    limits = c(0,1),
    name = "Gap Fraction"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "Gap Fracion derived from Backpack data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))



# extract values
vals_uav <- values(gf_uav)
vals_als <- values(gf_als)
vals_tls <- values(gf_tls)
vals_bp <- values(gf_bp)
valid <- complete.cases(vals_uav, vals_als, vals_bp, vals_tls)

# plot values to check them
plot(vals_uav[valid], vals_als[valid],
     xlab="UAV GF", ylab="ALS GF",
     main="Pixel-wise Comparison (UAV + ALS)")
abline(0, 1, col="red")

plot(vals_uav[valid], vals_tls[valid],
     xlab="UAV GF", ylab="TLS GF",
     main="Pixel-wise Comparison (UAV + TLS)")
abline(0, 1, col="red")

plot(vals_uav[valid], vals_bp[valid],
     xlab="UAV GF", ylab="Backpack GF",
     main="Pixel-wise Comparison (UAV + Backpack)")
abline(0, 1, col="red")

plot(vals_als[valid], vals_tls[valid],
     xlab="ALS Cover", ylab="TLS Cover",
     main="Pixel-wise Comparison (ALS + TLS)")
abline(0, 1, col="red")

plot(vals_als[valid], vals_bp[valid],
     xlab="ALS Cover", ylab="Backpack Cover",
     main="Pixel-wise Comparison (ALS + Backpack)")
abline(0, 1, col="red")

plot(vals_tls[valid], vals_bp[valid],
     xlab="TLS Cover", ylab="Backpack Cover",
     main="Pixel-wise Comparison (TLS + Backpack)")
abline(0, 1, col="red")


# Layer summaries
metrics <- tibble(
  Metric = c("Mean", "Median", "SD", "Min", "Max"),
  UAV = c(
    global(gf_uav, mean, na.rm = TRUE)[[1]],
    global(gf_uav, median, na.rm = TRUE)[[1]],
    global(gf_uav, sd, na.rm = TRUE)[[1]],
    global(gf_uav, range, na.rm = TRUE)[1],
    global(gf_uav, range, na.rm = TRUE)[2]
  ),
  ALS = c(
    global(gf_als, mean, na.rm = TRUE)[[1]],
    global(gf_als, median, na.rm = TRUE)[[1]],
    global(gf_als, sd, na.rm = TRUE)[[1]],
    global(gf_als, range, na.rm = TRUE)[1],
    global(gf_als, range, na.rm = TRUE)[2]
  ),
  TLS = c(
    global(gf_tls, mean, na.rm = TRUE)[[1]],
    global(gf_tls, median, na.rm = TRUE)[[1]],
    global(gf_tls, sd, na.rm = TRUE)[[1]],
    global(gf_tls, range, na.rm = TRUE)[1],
    global(gf_tls, range, na.rm = TRUE)[2]
  ),
  BP = c(
    global(gf_bp, mean, na.rm = TRUE)[[1]],
    global(gf_bp, median, na.rm = TRUE)[[1]],
    global(gf_bp, sd, na.rm = TRUE)[[1]],
    global(gf_bp, range, na.rm = TRUE)[1],
    global(gf_bp, range, na.rm = TRUE)[2]
  )
)

print(metrics)






# DIFFERENCE OF ALS - UAV

# run linear model to check relationship
lm_model_als_uav <- lm(vals_als[valid] ~ vals_uav[valid])
summary(lm_model_als_uav)

vals_df_als_uav <- data.frame(
  als = vals_als,
  uav = vals_uav
)

vals_df_als_uav <- na.omit(vals_df_als_uav)
# library(ggplot2)
ggplot(data = vals_df_als_uav, aes(x=cover_als , y = cover_uav))+
  geom_tile()

# difference map
gf_uav_resampled <- resample(gf_uav, gf_als, method = "bilinear")
gf_tls_resampled <- resample(gf_tls, gf_als, method = "bilinear")
gf_bp_resampled <- resample(gf_bp, gf_als, method = "bilinear")
gf_tls_resampled2 <- resample(gf_tls, gf_uav, method = "bilinear")
gf_bp_resampled2 <- resample(gf_bp, gf_uav, method = "bilinear")
gf_bp_resampled3 <- resample(gf_bp, gf_tls, method = "bilinear")



diff_raster_als_uav <- gf_als - gf_uav_resampled
diff_raster_als_tls <- gf_als - gf_tls_resampled
diff_raster_als_bp <- gf_als - gf_tls_resampled
diff_raster_uav_tls <- gf_uav - gf_tls_resampled2
diff_raster_uav_bp <- gf_uav - gf_bp_resampled2
diff_raster_tls_bp <- gf_tls - gf_bp_resampled3


x <- plot(diff_raster_als_uav, main = "ALS - UAV LAI")


global(diff_raster_als_uav, fun = mean, na.rm=TRUE)
global(diff_raster_als_uav, fun = sd, na.rm=TRUE)

# extract diff raster values
vals_diff_als_uav <- values(diff_raster_als_uav)
vals_diff_als_tls <- values(diff_raster_als_tls)
vals_diff_als_bp <- values(diff_raster_als_bp)
vals_diff_uav_tls <- values(diff_raster_uav_tls)
vals_diff_uav_bp <- values(diff_raster_uav_bp)
vals_diff_tls_bp <- values(diff_raster_tls_bp)

vals_diff_clean_als_uav <- vals_diff_als_uav[!is.na(vals_diff_als_uav)]
vals_diff_clean_als_tls <- vals_diff_als_tls[!is.na(vals_diff_als_tls)]
vals_diff_clean_als_bp <- vals_diff_als_bp[!is.na(vals_diff_als_bp)]
vals_diff_clean_uav_tls <- vals_diff_uav_tls[!is.na(vals_diff_uav_tls)]
vals_diff_clean_uav_bp <- vals_diff_uav_bp[!is.na(vals_diff_uav_bp)]
vals_diff_clean_tls_bp <- vals_diff_tls_bp[!is.na(vals_diff_tls_bp)]


metrics_combi <- tibble(
  Metric = c("Mean", "Median", "SD", "Range Min", "Range Max", "RSME", "MAE"),
  ALS_UAV = c(
    mean(vals_diff_clean_als_uav)[[1]],
    median(vals_diff_clean_als_uav)[[1]],
    sd(vals_diff_clean_als_uav)[[1]],
    range(vals_diff_clean_als_uav)[1],
    range(vals_diff_clean_als_uav)[2],
    sqrt(mean((vals_als[valid] - vals_uav[valid])^2)),
    mean(abs(vals_als[valid] - vals_uav[valid]))
  ),
  ALS_TLS = c(
    mean(vals_diff_clean_als_tls)[[1]],
    median(vals_diff_clean_als_tls)[[1]],
    sd(vals_diff_clean_als_tls)[[1]],
    range(vals_diff_clean_als_tls)[1],
    range(vals_diff_clean_als_tls)[2],
    sqrt(mean((vals_als[valid] - vals_tls[valid])^2)),
    mean(abs(vals_als[valid] - vals_tls[valid]))
  ),
  ALS_BP = c(
    mean(vals_diff_clean_als_bp)[[1]],
    median(vals_diff_clean_als_bp)[[1]],
    sd(vals_diff_clean_als_bp)[[1]],
    range(vals_diff_clean_als_bp)[1],
    range(vals_diff_clean_als_bp)[2],
    sqrt(mean((vals_als[valid] - vals_bp[valid])^2)),
    mean(abs(vals_als[valid] - vals_bp[valid]))
  ),
  UAV_TLS = c(
    mean(vals_diff_clean_uav_tls)[[1]],
    median(vals_diff_clean_uav_tls)[[1]],
    sd(vals_diff_clean_uav_tls)[[1]],
    range(vals_diff_clean_uav_tls)[1],
    range(vals_diff_clean_uav_tls)[2],
    sqrt(mean((vals_uav[valid] - vals_tls[valid])^2)),
    mean(abs(vals_uav[valid] - vals_tls[valid]))
  ),
  UAV_BP = c(
    mean(vals_diff_clean_uav_bp)[[1]],
    median(vals_diff_clean_uav_bp)[[1]],
    sd(vals_diff_clean_uav_bp)[[1]],
    range(vals_diff_clean_uav_bp)[1],
    range(vals_diff_clean_uav_bp)[2],
    sqrt(mean((vals_uav[valid] - vals_bp[valid])^2)),
    mean(abs(vals_uav[valid] - vals_bp[valid]))
  ),
  TLS_BP = c(
    mean(vals_diff_clean_tls_bp)[[1]],
    median(vals_diff_clean_tls_bp)[[1]],
    sd(vals_diff_clean_tls_bp)[[1]],
    range(vals_diff_clean_tls_bp)[1],
    range(vals_diff_clean_tls_bp)[2],
    sqrt(mean((vals_tls[valid] - vals_bp[valid])^2)),
    mean(abs(vals_tls[valid] - vals_bp[valid]))
  )
)

print(metrics)









# paired t-test
t.test(vals_als[valid], vals_uav[valid], paired=TRUE)
t.test(vals_als[valid], vals_tls[valid], paired=TRUE)
t.test(vals_als[valid], vals_bp[valid], paired=TRUE)
t.test(vals_uav[valid], vals_tls[valid], paired=TRUE)
t.test(vals_uav[valid], vals_bp[valid], paired=TRUE)
t.test(vals_tls[valid], vals_bp[valid], paired=TRUE)


# correlation 
lm_model <- lm(vals_als[valid] ~ vals_uav[valid])
summary(lm_model)


# histogram of differences
hist(vals_diff_clean,
     breaks = 50,
     main = "Histogram of Differences (ALS - UAV)",
     xlab = "Difference in LAI")

# density plot
plot(density(vals_diff_clean, na.rm=TRUE),
     main="Density of Differences",
     xlab="Difference in LAI")
abline(v=0, col="red", lty=2)

# scatter plot
plot(vals_uav[valid], vals_als[valid],
     xlab="Scaled UAV LAI",
     ylab="ALS LAI",
     main="Pixel-wise Comparison")
abline(0,1,col="red")

residuals <- vals_als[valid] - vals_uav[valid]

plot(residuals, main="Residuals (ALS - UAV)",
     ylab="Difference (LAI)")
abline(h=0, col="red", lty=2)








### make values df into long format
data <- data.frame()
data <- rbind(data, vals_als)
data <- cbind(data, vals_tls)
data <- cbind(data, vals_uav)
data <- cbind(data, vals_bp)

data_long <- pivot_longer(data, cols = c("gf_als", "gf_tls", "gf_uav", "gf_bp"), names_to = "Source", values_to = "gf")


## Viz
ggplot(data_long, aes(x = Source, y = gf))+
  geom_jitter(color = "grey",
              alpha = 0.7,
              size = 1)+
  geom_boxplot(color = "darkolivegreen",
               fill = "darkolivegreen4",
               alpha = 0.3,
               notch = TRUE,
               notchwidth = 0.8,
               outlier.colour="red",
               outlier.fill="red",
               outlier.size=2)+
  labs(title = "Gap Fraction Measurement Comparison", )+
  xlab("") +
  ylab("Gap Fraction") +
  theme_minimal()+
  theme(plot.title = element_text(size = 15))

# export in 6.28 6.4, cubes quadratic



#### 4. pairwise comparison

common_scale <- scale_fill_gradient2(
  low = "blue",
  mid = "white",
  high = "red",
  midpoint = 0,
  limits = c(-1, 1),
  oob = scales::squish # avoids warnings from small differences
)
library(dplyr)
diff_raster_als_uav <- rename(as.data.frame(diff_raster_als_uav, xy = TRUE, na.rm = TRUE), gf_diff = gf_als )
diff_raster_als_tls <- rename(as.data.frame(diff_raster_als_tls, xy = TRUE, na.rm = TRUE), gf_diff = gf_als )
diff_raster_als_bp <- rename(as.data.frame(diff_raster_als_bp, xy = TRUE, na.rm = TRUE), gf_diff = gf_als )
diff_raster_uav_tls <- rename(as.data.frame(diff_raster_uav_tls, xy = TRUE, na.rm = TRUE), gf_diff = gf_uav  )
diff_raster_uav_bp <- rename(as.data.frame(diff_raster_uav_bp, xy = TRUE, na.rm = TRUE), gf_diff = gf_uav  )
diff_raster_tls_bp <- rename(as.data.frame(diff_raster_tls_bp, xy = TRUE, na.rm = TRUE), gf_diff = gf_tls)


a <- ggplot(data = diff_raster_als_uav, aes(x = x, y = y, fill = gf_diff))+
  geom_tile()+
  coord_equal() +
  ylab("UAV") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "ALS - UAV", hjust = "right")+ labs(fill = 'Gap Fraction')

b <- ggplot(data = diff_raster_als_tls, aes(x = x, y = y, fill = gf_diff))+
  geom_tile()+
  coord_equal() +
  ylab("TLS") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "ALS - TLS", hjust = "right")+ labs(fill = 'Gap Fraction')

c <- ggplot(data = diff_raster_als_bp, aes(x = x, y = y, fill = gf_diff))+
  geom_tile()+
  coord_equal() +
  ylab("Backpack") +
  xlab("ALS")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "ALS - Backpack", hjust = "right")+ labs(fill = 'Gap Fraction')


d <- ggplot(data = diff_raster_uav_tls, aes(x = x, y = y, fill = gf_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "UAV - TLS", hjust = "right")+ labs(fill = 'Gap Fraction')

e <- ggplot(data = diff_raster_uav_bp, aes(x = x, y = y, fill = gf_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("UAV")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "UAV - Backpack", hjust = "right")+ labs(fill = 'Gap Fraction')

f <- ggplot(data = diff_raster_tls_bp, aes(x = x, y = y, fill = gf_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("TLS")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "TLS - Backpack", hjust = "right")+ labs(fill = 'Gap Fraction')

# lay2 <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
# 
# grid.arrange(a, NULL, NULL, b, d, NULL, c, e, f, 
#              layout_matrix = lay2, widths = c(1,1,1), heights = c(1.01,0.94,1)
#              ,top = textGrob("Pairwise Comparison of LAI measurements", gp=gpar(fontsize =15))
# )
# export in 6.28 6.4, cubes quadratic


layout <- "
A##
BD#
CEF
"
combined <- (
  a + b + c + d + e + f +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right") 
)

combined +
  plot_annotation(
    title = "Pairwise Comparison of Gap Fraction measurements"
  )


