# Create figure 2 for the paper
library(png)
simDir = 'D:/Mircea/Projects/RESEARCH/InternalCoordination/Data_Original/PawelSimulations/SimDataPlacozoa_nonnorm_spring'


particleDFs = list.files(path = simDir, pattern="*sampledFrames.rds", full.names=T, recursive = T)
metadata = data.frame(filename = particleDFs, stringsAsFactors = F) %>% mutate(processedFiles = str_replace(filename, "sampledFrames", "nestedFrames"))
numericals = str_extract_all(basename(particleDFs), "[0-9.]+", simplify = T)[,1:3]
numericals = matrix(as.numeric(unlist(numericals)), ncol = ncol(numericals))
metadata = cbind(metadata, numericals)
colnames(metadata) = c("filename", "processedFile", "Noise", "Size", "Replicate")
metadata = arrange(metadata, Noise, Size)


criticalityFiles = filter(metadata, Size == 16384 & Noise == 3.4)
criticalityFrames = read_rds(criticalityFiles$filename[[1]])
criticalityFrame = criticalityFrames %>% filter(Frame == unique(Frame)[190]) %>% select(-Frame) %>% calculateFluctuationField() %>% mutate(angle = atan2(x = fluctX, y = fluctY), speed = sqrt(fluctX^2 + fluctY^2))
(criticalNoisePlot = ggplot(criticalityFrame, aes(X, Y, color = angle, alpha = speed)) + geom_point() + coord_fixed() + scale_color_gradientn(name = expression(theta), colours = cbPalette, limits = c(-pi,pi), breaks=c(-pi, 0, pi), labels=c(expression(paste("-",pi,sep="")), 0, expression(paste(pi)))) + guides(alpha = F) + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()))


schemaPlot = readPNG(paste(projectFolder, "Figures/model_scheme.png", sep = ""))
schemaPlotRaster = rasterGrob(schemaPlot)
(schemaPlotImg = ggplot(orderDataSummary, aes()) + annotation_custom(schemaPlotRaster) + coord_fixed(ratio=1))


simDir = 'D:/Mircea/Projects/RESEARCH/InternalCoordination/Data_Original/PawelSimulations/SimDataPlacozoa_nonnorm_spring'
orderData = read_rds(paste(simDir, "orderData.rds", sep = "/"))
orderSuscept = read_rds(paste(projectFolder,"simOrderSusceptibility.rds", sep = "/"))

orderDataSummary = group_by(orderData, Noise, Size) %>% summarise(meanOrder = mean(Order), sdOrder = sd(Order), count = n())

(orderNoisePlot = ggplot(orderDataSummary, aes(Noise, meanOrder, color = sqrt(Size), group = Size)) + geom_point() + geom_line() + 
    geom_errorbar(aes(ymax = meanOrder + sdOrder/sqrt(count), ymin = meanOrder - sdOrder/sqrt(count)), width = 0.05) + 
    scale_y_continuous("Order") + scale_x_continuous(expression(mu)) + 
    scale_color_gradient(TeX("\\sqrt{N}")) + annotate("rect", ymax = Inf, ymin = -Inf, xmin = 3.2, xmax = 3.9, fill = "red", alpha = 0.5))



(susceptOrderPlot = ggplot(orderSuscept, aes(Noise, susceptibility, group = sqrt(Size), color = sqrt(Size))) + geom_point() + geom_line() + scale_y_continuous(expression(chi[o])) + 
    scale_x_continuous(expression(mu)) + scale_color_gradient(TeX("\\sqrt{N}")) + guides(color = F) +
    annotate("rect", ymax = Inf, ymin = -Inf, xmin = 3.2, xmax = 3.9, fill = "red", alpha = 0.5))

(panelC = ggdraw() + draw_plot(orderNoisePlot) + draw_plot(susceptOrderPlot, 0.35, 0.15, 0.5, 0.6))


## Find the same data for the noise simulations
simDir = "D:/Mircea/Projects/RESEARCH/InternalCoordination/Data_Original/PawelSimulations/SimData_nonnorm_fluid/"
orderData = read_rds(paste(simDir, "orderData_sim_noise_fluid.rds", sep = "/"))
orderData_noise = read_rds(path = paste(simDir, "orderData_sim_noise_fluid.rds", sep = "/"))
orderSuscept_noise = read_rds(path = paste(projectFolder,"simOrderSusceptibility_noise.rds", sep = "/"))
orderDataSummary = group_by(orderData, Noise, Size) %>% summarise(meanOrder = mean(Order), sdOrder = sd(Order), count = n())
(orderNoisePlot2 = ggplot(orderDataSummary, aes(Noise, meanOrder, color = sqrt(Size), group = Size)) + geom_point() + geom_line() + 
    geom_errorbar(aes(ymax = meanOrder + sdOrder/sqrt(count), ymin = meanOrder - sdOrder/sqrt(count)), width = 0.05) + 
    scale_y_continuous("Order") + scale_x_continuous(expression(eta), limits = c(0, 0.6)) + 
    scale_color_gradient(TeX("\\sqrt{N}")) + annotate("rect", ymax = Inf, ymin = -Inf, xmin = 0.32, xmax = 0.38, fill = "red", alpha = 0.5))

(susceptOrderPlot2 = ggplot(orderSuscept_noise, aes(Noise, susceptibility, group = sqrt(Size), color = sqrt(Size))) + geom_point() + geom_line() + scale_y_continuous(expression(chi[o]), breaks = c(0, 100, 200)) + 
    scale_x_continuous(expression(eta), breaks = c(0.20, 0.60)) + scale_color_gradient(TeX("\\sqrt{N}")) + guides(color = F) +
    annotate("rect", ymax = Inf, ymin = -Inf, xmin = 0.32, xmax = 0.38, fill = "red", alpha = 0.5))
(panelD = ggdraw() + draw_plot(orderNoisePlot2 + guides(color = F)) + draw_plot(susceptOrderPlot2, 0.18, 0.15, 0.4, 0.6))

figure2Plot = plot_grid(schemaPlotImg, criticalNoisePlot, panelC, panelD, labels = c("A", "B", "C", "D"), label_size = 32)
save_plot(paste(projectFolder, "Figures/Figure2.pdf", sep = ""), figure2Plot, base_width = 10, base_height = 10)

