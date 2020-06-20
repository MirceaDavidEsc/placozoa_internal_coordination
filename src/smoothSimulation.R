# Make continuous simulation figure
library(cowplot)
library(readr)
library(dplyr)
setwd("D:/Mircea/Projects/RESEARCH/InternalCoordination/Data_Original/SimData_nonnorm_cont")
continuousSim = read_rds("Dphi0.32_N16384_1_sampledFramesContinuous.rds")
nrow(continuousSim)/length(unique(continuousSim$Frame))
continuousSim$particle = 1:16384



smoothedSim = continuousSim %>% filter(Frame %in% 2795:2804) %>% group_by(particle) %>% summarise_all(.funs = "mean")

frameData = smoothedSim %>% select(-particle, -Frame)

library(scalefree)
library(collective)
fluctField = frameData %>% select(-speed, -angle) %>% scalefree::calculateFluctuationField()

quiverPlot(fluctField, 4, colormapped = T)

colnames(fluctField) = c("x","y","u","v")
frameData = fluctField %>% mutate(speed = sqrt(v^2 + u^2), angle = atan2(x = u, y = v))

write_rds(frameData, "D:/Mircea/Projects/RESEARCH/InternalCoordination/Data_Processed/SimulationSmoothed.rds")
cbPalette = c("magenta","red","yellow","green","cyan","blue","magenta")

(simulations = ggplot(frameData, aes(x, y, color=angle, alpha=speed)) + geom_point() +
  scale_color_gradientn(name = expression(theta), colours = cbPalette, limits = c(-pi,pi), breaks=c(-pi, 0, pi)) + guides(color = F, alpha = F))

save_plot(filename = "D:/Mircea/Projects/RESEARCH/InternalCoordination/Figures/SimulationSmooth.pdf", plot = simulations, base_width = 6, base_height = 6)

 
