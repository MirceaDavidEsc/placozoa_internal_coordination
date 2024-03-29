# Defining figure 1
## Figure 1 Figures ##
# frame1 = readJPEG("Figures/snap1_nobg_rotate_narrow_scale_arrow.jpg")
# frame2 = readJPEG("Figures/snap2_nobg_rotate_narrow_arrow.jpg")
# frame3 = readJPEG("Figures/snap3_nobg_rotate_narrow_arrow.jpg")
# 
# 
# # Rasterize the images
# imgRaster1 = rasterGrob(frame1)
# imgRaster2 = rasterGrob(frame2)
# imgRaster3 = rasterGrob(frame3)
# 
# (frame1Plot = ggplot(conversionFactors, aes()) + annotation_custom(imgRaster1) + coord_fixed(ratio=1) + theme_minimal())
# (frame2Plot = ggplot(conversionFactors, aes()) + annotation_custom(imgRaster2) + coord_fixed(ratio=1) + theme_minimal())
# (frame3Plot = ggplot(conversionFactors, aes()) + annotation_custom(imgRaster3) + coord_fixed(ratio=1) + theme_minimal())
# 
# (snapshotsPlot = plot_grid(frame1Plot, frame2Plot, frame3Plot, ncol = 3, scale = 2))

Hammamatsu_calibration = 0.616 # px / micrometer
CoolSnap_calibration = 0.859 # px / micrometer

trajectory_data = read_csv("Data_processed/trajectory_example/trajectoryML.csv")
bgimg = readPNG("Data_processed/trajectory_example/2015-02-13-run_1_out1669_nobg.png")
imgRaster = rasterGrob(bgimg)

trajectory_centered = trajectory_data %>% filter(Frame <= 1669) %>% mutate(CentroidY = -CentroidY) %>% mutate(CentroidX = CentroidX / Hammamatsu_calibration, CentroidY = CentroidY / Hammamatsu_calibration, Time = Frame / 120) %>%
  mutate(CentroidY = CentroidY + 1550, CentroidX = CentroidX + 1100)

imgSize = 2048
imgymin = 0
imgxmin = 0
(movementExample = ggplot(filter(trajectory_centered, Frame <= 1669)) + annotation_custom(imgRaster, ymin = imgymin, ymax = imgymin + imgSize, xmin = imgxmin, xmax = imgxmin + imgSize) + coord_fixed(ratio = 1) +
    geom_point(aes(CentroidX, CentroidY, color = Time)) + scale_color_viridis_c("Time (min)") + 
    scale_x_continuous(TeX("Distance ($\\mu m$)"), limits = c(0, 1800)) + 
    scale_y_continuous(TeX("Distance ($\\mu m$)"), limits = c(0,1800)) +
    theme_minimal())
movementExample = increase_text_size(movementExample)


### FUNCTION: Standardized function for plotting velocity fields from animal data.
plot_animal_velocity_field = function(frameData, arrows = F, arrows_round = 5, arrowl = 1, scalebar = F, scalebar_l = 100, scalebar_text = "", scalebar_w = 3) {
  colnames(frameData) = c("X", "Y", "vX", "vY", "speed", "angle")
  
  segmentData = frameData %>% mutate(Xround = round(X/arrows_round) * arrows_round, Yround = round(Y/arrows_round) * arrows_round) %>%
    mutate(outBounds = Xround^2 + Yround^2 > X^2 + Y^2) %>%
    group_by(Xround, Yround) %>% summarise(meanVX = mean(vX * arrowl), meanVY = mean(vY * arrowl), allOut = min(outBounds)) %>%
    filter(!allOut)
  
  (return_plot = ggplot(frameData) + geom_tile(aes(X, Y, fill=angle)) +
    scale_fill_gradientn(name = expression(theta), colours = cbPalette, limits = c(-pi,pi), breaks=c(-pi, 0, pi)) +
    theme_classic() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank()) + 
    coord_fixed() + guides(fill = F))
    
  if (arrows) {
    return_plot = return_plot + 
      geom_segment(data = segmentData, aes(Xround, Yround, 
                                           xend = Xround + meanVX * arrowl, yend = Yround + meanVY * arrowl),
                   lineend = "round", size = 1,
                   arrow = arrow(angle = 15, length = unit(0.3, "cm"), type = "closed"))
  }
  
  if (scalebar) {
    return_plot = return_plot + annotate("rect", alpha = 1, fill = "black", xmin = max(frameData$X) - scalebar_l, xmax = max(frameData$X), ymin = min(frameData$Y), ymax = min(frameData$Y) + scalebar_w) +
      annotate("text", x = max(frameData$X) / 6, y = min(frameData$Y) - 3 * scalebar_w, label = scalebar_text, size = 6) + scale_y_continuous(limits = c(min(frameData$Y) * 1.5, max(frameData$Y)))
  }
  return(return_plot)
}

# Make the velocity field plots.
polarizedSmall = read_csv(paste(projectFolder, "Data_Processed", "polarizedFrame.csv", sep = "/"))
polarizedSmall = mutate(polarizedSmall, X = X - mean(X), Y = Y - mean(Y))

# Calculate the scale bar needed for 500 micrometers
# 1 voxel / 8 pixels
1 * CoolSnap_calibration

scale_bar_length = 500 # in micrometers

(arrow_plot_small_polarized = plot_animal_velocity_field(polarizedSmall, arrows = F, arrows_round = 10, arrowl = 1, scalebar = T, scalebar_l = 500 * CoolSnap_calibration / 8, scalebar_text = TeX("500$\\mu$m"), scalebar_w = 2))


rotatingSmall = read_csv(paste(projectFolder, "Figures", "Small_Rotation.csv", sep = "/"))
rotatingSmall = mutate(rotatingSmall, X = X - mean(X), Y = Y - mean(Y))
(arrow_plot_small_rotating = plot_animal_velocity_field(rotatingSmall, arrows = F, arrows_round = 10, arrowl = 1, scalebar = T, scalebar_l = 500 * CoolSnap_calibration / 8, scalebar_text = TeX("500$\\mu$m"), scalebar_w = 2))



polarizedBig = read_csv(paste(projectFolder, "Figures", "Big_Polarized.csv", sep = "/")) %>% 
  mutate(speed = sqrt(vX^2 + vY^2), angle = atan2(x = vX, y = vY)) %>% 
  mutate(X = X - mean(X), Y = Y - mean(Y))

## Define boundaries of image, based on min and max of frame.

(arrow_plot_big_order = plot_animal_velocity_field(polarizedBig, arrows = F, arrows_round = 30, arrowl = 2, scalebar = T, scalebar_l = 500 * Hammamatsu_calibration / 8, scalebar_text = TeX("500$\\mu$m"), scalebar_w = 5))


disorganizedBig = read_csv(paste(projectFolder, "Data_Processed", "Big_Disorganzed.csv", sep = "/")) %>% 
  mutate(X = X - mean(X), Y = Y - mean(Y))

(arrow_plot_big_disorder = plot_animal_velocity_field(disorganizedBig, arrows = F, arrows_round = 30, arrowl = 2, scalebar = T, scalebar_l = 500 * Hammamatsu_calibration / 8, scalebar_text = TeX("500$\\mu$m"), scalebar_w = 5))

(velocitySnaps = plot_grid(arrow_plot_small_polarized, arrow_plot_small_rotating, arrow_plot_big_order, arrow_plot_big_disorder, ncol = 2, labels = c("B", "C", "D", "E"), scale = 1, label_size = 20))

# Order vs size plot
relevantOrderData = read_rds(paste(projectFolder, processedDataFolder, "relevantOrderData_filtered.rds", sep = "/"))
(collectiveOrderSizePlot = ggplot(relevantOrderData, aes(EquivalentDiameter_mean, crystalMeasure_mean)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = crystalMeasure_mean - crystalMeasure_sd/sqrt(count), 
                      ymax = crystalMeasure_mean + crystalMeasure_sd/sqrt(count))) +
    geom_errorbarh(aes(xmin = EquivalentDiameter_mean - EquivalentDiameter_sd/sqrt(count), 
                       xmax = EquivalentDiameter_mean + EquivalentDiameter_sd/sqrt(count))) +
    xlab(expression(paste("Diameter (",mu,"m)",sep=""))) + 
    scale_y_continuous("Order", breaks = c(0.7, 0.8, 0.9)) + stat_smooth(method="lm") +
    theme_classic())
collectiveOrderSizePlot = increase_text_size(collectiveOrderSizePlot)


## Try making a radial guide
legendGrid = expand.grid(X = seq(from = -10, to = 10, by = 0.1), Y = seq(from = -10, to = 10, by = 0.25)) %>%
  mutate(vX = X, vY = Y) %>%
  mutate(Speed = sqrt(X^2 + Y^2), Heading = atan2(Y,X)) %>% filter(Speed <= 10)

plot_animal_velocity_field(nextGrid, arrows = T, arrowl = 0.5, arrows_round = 3)

legendArrows = data.frame(X = 0, Y = 0, Heading = seq(from = -pi, to = pi, by = pi / 4)) %>%
  mutate(Xend = 8*sin(Heading), Yend = 8*cos(Heading))


(colorWheelGuide = ggplot(legendGrid, aes(X, Y)) + geom_tile(aes(fill = Heading)) + scale_fill_gradientn(colours = cbPalette, limits = c(-pi,pi)) + guides(fill=F,alpha=F) +
    geom_segment(data = legendArrows, aes(X, Y, xend = Xend, yend = Yend), 
                 arrow = arrow(angle = 15, type = "closed", length = unit(0.3, "cm")), size = 0.5) +
  annotate("rect", xmin = -11, xmax = 11, ymin = -11, ymax = 11, fill = NA, color = "black") + coord_fixed() +
  theme_classic() +
  scale_fill_gradientn(name = expression(theta), colours = cbPalette, limits = c(-pi,pi), breaks=c(-pi, 0, pi)) +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.line = element_blank(), axis.ticks = element_blank()))

(figVFPlot = ggdraw() + draw_plot(velocitySnaps) + draw_plot(colorWheelGuide, x = 0.4, y = 0, width = 0.25, height = 0.25))


(fullFigure = plot_grid(movementExample, figVFPlot, collectiveOrderSizePlot, ncol = 1, rel_heights = c(0.25, 0.5, 0.25), labels = c("A", "", "F"), label_size = 20))
save_plot(filename = paste(projectFolder, "figures/Figure1.pdf", sep = "/"), fullFigure, base_width = 6, base_height = 12)
