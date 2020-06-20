# Defining figure 1
## Figure 1 Figures ##
frame1 = readJPEG("Figures/snap1_nobg_rotate_narrow_scale_arrow.jpg")
frame2 = readJPEG("Figures/snap2_nobg_rotate_narrow_arrow.jpg")
frame3 = readJPEG("Figures/snap3_nobg_rotate_narrow_arrow.jpg")


# Rasterize the images
imgRaster1 = rasterGrob(frame1)
imgRaster2 = rasterGrob(frame2)
imgRaster3 = rasterGrob(frame3)

(frame1Plot = ggplot(conversionFactors, aes()) + annotation_custom(imgRaster1) + coord_fixed(ratio=1))
frame2Plot = ggplot(conversionFactors, aes()) + annotation_custom(imgRaster2) + coord_fixed(ratio=1)
frame3Plot = ggplot(conversionFactors, aes()) + annotation_custom(imgRaster3) + coord_fixed(ratio=1)

(snapshotsPlot = plot_grid(frame1Plot, frame2Plot, frame3Plot, ncol = 3, labels = c("(i)", "(ii)", "(iii)"), scale = 2))



# Make the velocity field plots.
frameData = read_csv(paste(projectFolder, "Data_Processed", "polarizedFrame.csv", sep = "/"))
frameDataArrows = frameData %>% filter(X %% 10 == 0 & Y %% 10 == 0) %>% mutate(X = X - mean(frameData$X), Y = Y - mean(frameData$Y))
frameData = mutate(frameData, X = X - mean(X), Y = Y - mean(Y))

zoom = 36

(arrow_plot_small_polarized = ggplot(frameData) + geom_tile(aes(X, Y, fill=angle, alpha=speed)) +
    scale_fill_gradientn(name = expression(theta), colours = cbPalette, limits = c(-pi,pi), breaks=c(-pi, 0, pi)) +
    geom_segment(data = data.frame(XStart = min(frameData$X), XEnd = min(frameData$X) + 47, YStart = min(frameData$Y) - 5, YEnd = min(frameData$Y) - 5), 
                 aes(XStart, YStart, xend = XEnd, yend = YEnd), size = 3) + xlim(-zoom,zoom) + ylim(-zoom,zoom) + coord_fixed() + 
    guides(alpha = F, fill = F) + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
    geom_segment(data = frameDataArrows, aes(X, Y, xend = X+vX, yend = Y+vY), size = 1, color = "black", alpha = 1,
                 arrow=arrow(angle=20,length=unit(0.4,"cm"))))

frameData = read_csv(paste(projectFolder, "Figures", "Small_Rotation.csv", sep = "/"))
## Center the data
frameDataArrows = frameData %>% filter(X %% 10 == 0 & Y %% 10 == 0)
frameDataArrows = mutate(frameDataArrows, X = X - mean(frameData$X), Y = Y - mean(frameData$Y))
frameData = mutate(frameData, X = X - mean(X), Y = Y - mean(Y))

(arrow_plot_small_rotating = ggplot(frameData) + geom_tile(aes(X, Y, fill=angle, alpha=speed)) +
    scale_fill_gradientn(name = expression(theta), colours = cbPalette, limits = c(-pi,pi), breaks=c(-pi, 0, pi),
                         labels=c(expression(paste("-",pi,sep="")), 0, expression(paste(pi)))) +
    xlim(-zoom,zoom) + ylim(-zoom,zoom) + coord_fixed() + 
    guides(alpha = F, fill = F) + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
    geom_segment(data = frameDataArrows, aes(X, Y, xend = X+vX, yend = Y+vY), size = 1, color = "black", alpha = 1,
                 arrow=arrow(angle=20,length=unit(0.4,"cm"))))


polarizedFrame = read_csv(paste(projectFolder, "Figures", "Big_Polarized.csv", sep = "/"))
frameData = polarizedFrame %>% mutate(speed = sqrt(vX^2 + vY^2), angle = atan2(x = vX, y = vY))
frameDataArrows = frameData %>% filter(X %% 10 == 0 & Y %% 10 == 0) %>%
  mutate(X = X - mean(frameData$X), Y = Y - mean(frameData$Y))
frameData = frameData %>% mutate(X = X - mean(X), Y = Y - mean(Y))

## Define boundaries of image, based on min and max of frame.
boundary = max(abs(c(range(frameData$X), range(frameData$Y))))

(arrow_plot_big_order = ggplot(frameData) + geom_tile(aes(X, Y, fill=angle, alpha=speed)) +
    scale_fill_gradientn(name = expression(theta), colours = cbPalette, limits = c(-pi,pi), breaks=c(-pi, 0, pi)) + coord_fixed() + 
    guides(alpha = F, fill = F) + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
    geom_segment(data = frameDataArrows, aes(X, Y, xend = X+vX, yend = Y+vY), size = 1, color = "black", alpha = 1,
                 arrow=arrow(angle=20,length=unit(0.4,"cm"))) +
    geom_segment(data = data.frame(XStart = min(frameData$X), XEnd = min(frameData$X) + 47, YStart = min(frameData$Y) - 5, YEnd = min(frameData$Y) - 5), 
                 aes(XStart, YStart, xend = XEnd, yend = YEnd), size = 3) +
    xlim(-boundary, boundary) + ylim(-boundary,boundary))


frameData = read_csv(paste(projectFolder, "Data_Processed", "Big_Disorganzed.csv", sep = "/"))
frameDataArrows = frameData %>% filter(X %% 10 == 0 & Y %% 10 == 0)
frameDataArrows = frameDataArrows %>% mutate(X = X - mean(frameData$X), Y = Y - mean(frameData$Y))
frameData = frameData %>% mutate(X = X - mean(X), Y = Y - mean(Y))
(arrow_plot_big_disorder = ggplot(frameData) + geom_tile(aes(X, Y, fill=angle, alpha=speed)) +
    scale_fill_gradientn(name = expression(theta), colours = cbPalette, limits = c(-pi,pi), breaks=c(-pi, 0, pi))  + coord_fixed() + 
    guides(alpha = F, fill = F) + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
    geom_segment(data = frameDataArrows, aes(X, Y, xend = X+vX, yend = Y+vY), size = 1, color = "black", alpha = 1,
                 arrow=arrow(angle=20,length=unit(0.4,"cm"))) +
    xlim(-boundary, boundary) + ylim(-boundary,boundary))

(velocitySnaps = plot_grid(arrow_plot_small_polarized, arrow_plot_small_rotating, arrow_plot_big_order, arrow_plot_big_disorder, ncol = 2, labels = c("B", "C", "D", "E"), scale = 1, label_size = 32))

# Order vs size plot
relevantOrderData = read_rds(paste(projectFolder, processedDataFolder, "relevantOrderData.rds", sep = "/"))
# Summary data for relevant order frames.
uncurledOrderSummary = relevantOrderData %>% group_by(folder) %>% summarize_each(funs(mean,sd), c(EquivalentDiameter, polarization, rotation, crystalMeasure))
uncurledCount = relevantOrderData %>% group_by(folder) %>% summarise(count=n())
uncurledOrderSummary = inner_join(uncurledOrderSummary, uncurledCount)

(collectiveOrderSizePlot = ggplot(uncurledOrderSummary, aes(EquivalentDiameter_mean, crystalMeasure_mean)) + geom_point() + 
    xlab(expression(paste("D (",mu,"m)",sep=""))) + 
    scale_y_continuous("Order", breaks = c(0.7, 0.8, 0.9)) + stat_smooth(method="lm"))

# Use if you want order plot side-by-side with snapshots.
# (bottomHalf = plot_grid(velocitySnaps, collectiveOrderSizePlot, labels = c("", "F"), label_size = 32))

fullFigure = plot_grid(snapshotsPlot, velocitySnaps, collectiveOrderSizePlot, ncol = 1, rel_heights = c(0.4, 0.4, 0.2), labels = c("A", "", "F"), label_size = 32)
save_plot(filename = paste(projectFolder, "Figures/Figure1.pdf", sep = ""), fullFigure, base_width = 6, base_height = 12)
