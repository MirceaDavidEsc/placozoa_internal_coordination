# Make figure 3

## Critical slope - real animal
modelSlopes = read_rds(paste(projectFolder, "modelSlopes_realdata.rds", sep = ""))
diameterSlopePlot = ggplot(modelSlopes, aes(EquivalentDiameter, slope)) + geom_point() + 
  geom_line(aes(EquivalentDiameter, linear), size = 1, alpha = 0.5, color = "red") + geom_line(aes(EquivalentDiameter, exponential), size = 1) +
  xlab(expression(paste("D (",mu,"m)",sep=""))) + ylab(expression(paste("O'(",eta,"*)", sep = ""))) + guides(color = F) +
  theme(axis.title = element_text(size= 24), axis.text = element_text(size = 20))

energyOrderSize = read_rds(paste(projectFolder, "energyOrderSize.rds", sep = ""))
slopesPlotRatio = energyOrderSize %>% filter(complete.cases(.)) %>% ggplot(aes(energyRatio, order, group = folder, color = EquivalentDiameter)) + 
  geom_point(alpha = 0.2, color = "black") + stat_smooth(method = "lm", se = F) + 
  scale_color_gradient(expression(paste("D (",mu,"m)", sep = "")), high = "blue", low = "red") +
  scale_x_continuous(expression(paste(eta,"*",sep="")), breaks = c(0,0.5,1), limits = c(0,1)) +
  scale_y_continuous("Order", breaks = c(0,0.5,1), limits = c(0,1)) + guides(color = F) +
  theme_bw() + theme(axis.title = element_text(size= 20), axis.text = element_text(size = 18))

(panel2B = ggdraw() + draw_plot(diameterSlopePlot) + draw_plot(slopesPlotRatio, 0.5, 0.5, 0.5, 0.5))

## critical slope, simulation
criticalRange = read_rds(paste(projectFolder, "3C_NoiseOrderRelation_strength.rds", sep = ""))
(criticalSlopesPlot = ggplot(criticalRange, aes(energyRatio, polarization, group = Size)) + geom_point(color = "black", alpha = 0.2) + 
    stat_smooth(aes(color = Size), method = "lm", se = F) + scale_color_gradient(high = "blue", low = "red") + 
    scale_x_continuous(expression(paste(eta,"*",sep="")), breaks = c(0,0.5,1), limits = c(0,1)) + scale_y_continuous("Order", breaks = c(0,0.5,1), limits = c(0,1)) + 
    scale_linetype_discrete(TeX("\\sqrt{N}")) + guides(color = F))


modelSlopes = read_rds(paste(projectFolder, "3D_NoiseSlope_strength.rds", sep = ""))
(diameterSlopePlot_sim = ggplot(modelSlopes, aes(Diam, slope)) + geom_point(alpha = 0.5) + xlab(TeX("Size (\\sqrt{N})")) +
    ylab("S'(r)") + geom_line(aes(Diam, linear), size = 1, alpha = 0.5, color = "red") + 
    geom_line(aes(Diam, exponential), size = 1))

(panel2B_sim = ggdraw() + draw_plot(diameterSlopePlot_sim) + draw_plot(criticalSlopesPlot, 0.5, 0.5, 0.5, 0.5))


#################

## Scale free correlations - real animal
meanSizes = read_rds(paste(projectFolder, "Data_Processed", "meanSizes.rds", sep = "/"))
meanProfiles = read_rds(paste(projectFolder, "meanProfiles.rds", sep = "/"))
meanProfilesStats = read_csv(paste(projectFolder, "Data_Processed", "meanProfileStats.csv", sep = "/"))
perFrameAvgCorrStats = read_csv("perFrameAvgCorrStats.csv") %>% mutate(Area = pi*(EquivalentDiameter_mean/2)^2)

profilesWithCrossing = inner_join(meanProfiles, meanProfilesStats) %>% mutate(rescaledDist = domain/vZero) %>% 
  filter(rescaledDist <= 2 & vCorr_mean > -1)

(originalVelocityProfiles = profilesWithCrossing %>% ggplot(aes(domain, vCorr_mean, group=folder, color=EquivalentDiameter)) + geom_line() + 
    geom_hline(yintercept=0, linetype=2) + scale_x_continuous(expression(paste("D (",mu,"m)")), breaks = c(200,1000)) + 
    scale_y_continuous(name = expression(paste(C[V],"(x)", sep="")), breaks = c(0,0.5,1)) + 
    scale_color_gradient(name = expression(paste("Size (",mu,"m)",sep="")), limits = c(200,1600), breaks = c(200,900,1600)))


(vZeroPlot = ggplot(perFrameAvgCorrStats, aes(EquivalentDiameter_mean, vZero_mean)) + geom_point(size=2) + 
    geom_errorbar(aes(ymax=vZero_mean + vZero_sd / sqrt(numFrames), ymin=vZero_mean - vZero_sd / sqrt(numFrames))) +
    geom_errorbarh(aes(xmax=EquivalentDiameter_mean + EquivalentDiameter_sd / sqrt(numFrames), xmin=EquivalentDiameter_mean-EquivalentDiameter_sd / sqrt(numFrames))) + stat_smooth(method="lm", formula = y ~ -1+x, se = F,size=1,alpha=0.3, color="blue") + 
    scale_x_continuous(expression(paste("D (",mu,"m)",sep="")), breaks= c(200, 900, 1600)) + ylab(expression(paste(phi[V]," (",mu,"m)",sep=""))))

(panel3B_sim = ggdraw() + draw_plot(vZeroPlot) + draw_plot(originalVelocityProfiles + guides(color = F), 0.1, 0.6, 0.4, 0.4))

## Scale free correlations - simulation
perFrameAvgCorrStats_sim = read_rds(paste(projectFolder, "perFrameAvgStats_sim.rds", sep = ""))

(vZeroPlot = filter(perFrameAvgCorrStats_sim, Noise %in% c(3.7, 2.0, 5.0)) %>% ggplot(aes(sqrt(Size), vZero_mean, color = as.factor(Noise), group = as.factor(Noise))) + 
    geom_point(size=2) + geom_line(size = 1) + xlab(TeX("Size (\\sqrt{N})")) + ylab(expression(paste(phi[V]," (a.u.)",sep=""))) + 
    scale_color_manual(expression(mu), values = c("skyblue", "black", "green")))


#################

## Correlation integral - real animal

(rescaledProfilesPlot = profilesWithCrossing %>% ggplot(aes(rescaledDist, vCorr_mean, group=folder, color=EquivalentDiameter)) + geom_line() +  geom_hline(yintercept = 0, linetype=2) + ylab("K(x)") +
   scale_x_continuous(name = expression(paste("x/",phi,sep="")), breaks = c(0, 1.0, 2.0)) +
   scale_color_gradient(name = expression(paste("D (",mu,"m)",sep="")), limits = c(200,1600), breaks = c(200,900,1600)) + theme(legend.position = c(0.9, 0.8)))

(susceptibilityRelation = ggplot(susceptibilityValues, aes(EquivalentDiameter_mean, vSuscept_mean)) + geom_point() +
    geom_line(aes(EquivalentDiameter_mean, expFit), size = 1) + 
    # geom_line(aes(Area, linearFit), size = 1, alpha = 0.5, color = "red") + # No need for linear relationship
    scale_x_continuous(TeX(paste("D ($\\mu m$)",sep="")), breaks = c(200,900,1600)) +
    scale_y_continuous(expression(chi)))

(panel4B_sim = ggdraw() + draw_plot(susceptibilityRelation) + draw_plot(rescaledProfilesPlot + guides(color = F), 0.1, 0.6, 0.4, 0.4))


## correlation integral - simulation

(vSusceptPlot = filter(perFrameAvgCorrStats_sim, Noise %in% c(3.7, 2.0, 5.0)) %>% ggplot(aes(sqrt(Size), vSuscept_mean, color = as.factor(Noise), group = as.factor(Noise))) + 
    geom_point(size=2) + geom_line(size = 1) +
    xlab(TeX("Size (\\sqrt{N})")) + ylab(expression(paste(chi[V]," (a.u.)",sep=""))) + 
    scale_color_manual(expression(mu), values = c("skyblue", "black", "green")))

fillFigure = plot_grid(panel2B, panel3B_sim, panel4B_sim, panel2B_sim, vZeroPlot, vSusceptPlot, ncol = 3, labels = "AUTO", label_size = 32)
save_plot(fillFigure, filename = paste(projectFolder, "Figures/Figure3.pdf", sep = ""), base_width = 18, base_height = 10)
