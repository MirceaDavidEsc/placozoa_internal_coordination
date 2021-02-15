# Make figure 3
slope_regression_plot = function(slopesData) {
  diameterSlopePlot = ggplot(modelSlopes, aes(EquivalentDiameter, slope)) + geom_point() + 
    # geom_line(aes(EquivalentDiameter, linear), size = 1, alpha = 0.5, color = "red") + 
    geom_line(aes(EquivalentDiameter, exponential), size = 1) +
    xlab(expression(paste("Diameter (",mu,"m)",sep=""))) + 
    ylab(expression(paste("O'(",eta,"*)", sep = ""))) + 
    guides(color = F) + theme_classic()
  return(diameterSlopePlot)
}


noise_order_slopes_plot = function(noiseOrderData, grouping) {
  ggplot(noiseOrderData, aes(energyRatio, order, color = EquivalentDiameter)) + 
    geom_point(alpha = 0.2, color = "black") +
    stat_smooth(aes_string(group = grouping), method = "lm", se = F) + 
    scale_color_gradient(expression(paste("Diameter (",mu,"m)", sep = ""))) +
    scale_x_continuous(expression(paste(eta,"*",sep="")), breaks = c(0,0.5,1), limits = c(0,1)) +
    scale_y_continuous("Order", breaks = c(0,0.5,1), limits = c(0,1)) + guides(color = F) +
    theme_classic()
}


## Critical slope - real animal
modelSlopes = read_rds(paste(projectFolder, "modelSlopes_realdata.rds", sep = "/"))
(modelSlopesPlot = slope_regression_plot(modelSlopes) %>% increase_text_size())

energyOrderSize_actual = read_rds(paste(projectFolder, "energyOrderSize.rds", sep = "/")) %>% filter(complete.cases(.))
(slopesPlotRatio = energyOrderSize_actual %>% noise_order_slopes_plot(grouping = "folder") %>% increase_text_size())

(diameterSlopes_real = ggdraw() + draw_plot(modelSlopesPlot) + draw_plot(slopesPlotRatio, 0.5, 0.5, 0.5, 0.5))

## critical slope, simulation
energyOrderSize_sim = read_rds(paste(projectFolder, "3C_NoiseOrderRelation_strength.rds", sep = "/")) %>%
  rename(order = Order) %>% mutate(EquivalentDiameter = sqrt(Size))
(slopesPlotRatio_sim = energyOrderSize_sim %>% noise_order_slopes_plot(grouping = "Size") %>% increase_text_size())

modelSlopes = read_rds(paste(projectFolder, "3D_NoiseSlope_strength.rds", sep = "/")) %>%
  mutate(EquivalentDiameter = sqrt(Size))
modelSlopesPlot_sim = (slope_regression_plot(modelSlopes) + xlab(TeX("Size (\\sqrt{N})"))) %>% increase_text_size()

(diameterSlopes_sim = ggdraw() + draw_plot(modelSlopesPlot_sim) + draw_plot(slopesPlotRatio_sim, 0.55, 0.55, 0.45, 0.45))


#################

## Scale free correlations - real animal
meanSizes = read_rds(paste(projectFolder, "Data_Processed", "meanSizes.rds", sep = "/")) %>% ungroup()
meanProfiles = read_rds(paste(projectFolder, "meanProfiles.rds", sep = "/")) %>% ungroup()
meanProfilesStats = read_csv(paste(projectFolder, "meanProfileStats.csv", sep = "/")) %>% ungroup()
perFrameAvgCorrStats = read_csv("perFrameAvgCorrStats.csv") %>% mutate(Area = pi*(EquivalentDiameter_mean/2)^2) %>% ungroup()

profilesWithCrossing = inner_join(meanProfiles, meanProfilesStats) %>% mutate(rescaledDist = domain/vZero) %>% 
  filter(rescaledDist <= 2 & vCorr_mean > -1)

profiles_plot = function(profilesDF) {
   ggplot(profilesDF, aes(domain, vCorr_mean, group=folder, color=EquivalentDiameter)) + geom_line() + 
    geom_hline(yintercept=0, linetype=2) + scale_x_continuous(expression(paste("Diameter (",mu,"m)")), breaks = c(200,1000)) + 
    scale_y_continuous(name = expression(paste(C[V],"(x)", sep="")), breaks = c(0,0.5,1)) + 
    scale_color_gradient(name = expression(paste("Size (",mu,"m)",sep="")), limits = c(200,1600), breaks = c(200,900,1600)) +
    theme_classic()
}

(originalVelocityProfiles = profiles_plot(profilesWithCrossing))

originalVelocityProfiles = originalVelocityProfiles %>% increase_text_size()

corrLength_plot = function()

(vZeroPlot = ggplot(perFrameAvgCorrStats, aes(EquivalentDiameter_mean, vZero_mean)) + geom_point(size=2) + theme_classic() +
    geom_errorbar(aes(ymax=vZero_mean + vZero_sd / sqrt(numFrames), ymin=vZero_mean - vZero_sd / sqrt(numFrames))) +
    geom_errorbarh(aes(xmax=EquivalentDiameter_mean + EquivalentDiameter_sd / sqrt(numFrames), 
                       xmin=EquivalentDiameter_mean-EquivalentDiameter_sd / sqrt(numFrames))) + 
   stat_smooth(method="lm", color = "black", formula = y ~ -1+x, se = F,size=1,alpha=0.3, color="blue") + 
    scale_x_continuous(expression(paste("Diameter (",mu,"m)",sep="")), breaks= c(200, 900, 1600)) + ylab(expression(paste(phi[V]," (",mu,"m)",sep=""))))

vZeroPlot = vZeroPlot %>% increase_text_size()

(diamCorrLengthPlot_wprofiles = ggdraw() + draw_plot(vZeroPlot) + draw_plot(originalVelocityProfiles + guides(color = F), 0.15, 0.65, 0.35, 0.35))

## Scale free correlations - simulation
perFrameAvgCorrStats_sim = read_rds(paste(projectFolder, "perFrameAvgStats_sim.rds", sep = "/")) %>% ungroup()

(vZeroPlot_sim = perFrameAvgCorrStats_sim %>% #filter(, Noise %in% c(3.7, 2.0, 5.0)) %>% 
    ggplot(aes(sqrt(Size), vZero_mean, color = Noise, group = as.factor(Noise))) + theme_classic() +
    geom_point(size=2) + geom_line(size = 1) + 
    geom_line(data = filter(perFrameAvgCorrStats_sim, Noise == 3.5), color = "black", size = 2, linetype = 2) +
    xlab(TeX("Size (\\sqrt{N})")) + ylab(expression(paste(phi[V]," (a.u.)",sep=""))) + 
    scale_color_gradient("k", high = "blue", low = "red", breaks = seq(2,8, by = 3)))


#################

## Correlation integral - real animal

(rescaledProfilesPlot = ggplot(profilesWithCrossing, aes(rescaledDist, vCorr_mean, group=folder, color=EquivalentDiameter)) + 
   theme_classic() +  geom_line() + 
   geom_hline(yintercept = 0, linetype=2) + ylab("K(x)") +
   scale_x_continuous(name = expression(paste("x/",phi,sep="")), breaks = c(0, 1.0, 2.0)) +
   scale_color_gradient(name = expression(paste("Diameter (",mu,"m)",sep="")), limits = c(200,1600), breaks = c(200,900,1600)) + 
   theme(legend.position = c(0.9, 0.8)))

rescaledProfilesPlot = rescaledProfilesPlot %>% increase_text_size()

expFit = glm(vSuscept_mean ~ log(EquivalentDiameter_mean), data = perFrameAvgCorrStats)
perFrameAvgCorrStats = broom::augment_columns(expFit, perFrameAvgCorrStats)


(susceptibilityRelation = ggplot(perFrameAvgCorrStats, aes(EquivalentDiameter_mean, vSuscept_mean)) + geom_point() +
    geom_line(aes(EquivalentDiameter_mean, .fitted), size = 1) + theme_classic() +
    # geom_line(aes(Area, linearFit), size = 1, alpha = 0.5, color = "red") + # No need for linear relationship
    scale_x_continuous(TeX(paste("Diameter ($\\mu m$)",sep="")), breaks = c(200,900,1600)) +
    scale_y_continuous(expression(chi)))
susceptibilityRelation = susceptibilityRelation %>% increase_text_size()


(diameterSuscept_wprofiles = ggdraw() + draw_plot(susceptibilityRelation) + 
    draw_plot(rescaledProfilesPlot + guides(color = F), 0.15, 0.65, 0.4, 0.35))


## correlation integral - simulation

(vSusceptPlot = perFrameAvgCorrStats_sim %>% # filter(Noise %in% c(3.7, 2.0, 5.0)) %>% 
    ggplot(aes(sqrt(Size), vSuscept_mean, color = Noise, group = as.factor(Noise))) + theme_classic() +
    geom_point(size=2) + geom_line(size = 1) +
    geom_line(data = filter(perFrameAvgCorrStats_sim, Noise == 3.5), color = "black", size = 2, linetype = 2) +
    xlab(TeX("Size (\\sqrt{N})")) + ylab(expression(paste(chi[V]," (a.u.)",sep=""))) + 
    scale_color_gradient("k", high = "blue", low = "red", breaks = seq(2,8, by = 3)))



fullFigure = plot_grid(diameterSlopes_real, 
                       diamCorrLengthPlot_wprofiles, 
                       diameterSuscept_wprofiles, 
                       diameterSlopes_sim, 
                       increase_text_size(vZeroPlot_sim), 
                       increase_text_size(vSusceptPlot), ncol = 3, labels = "AUTO", label_size = 32)
save_plot(fullFigure, filename = paste(projectFolder, "Figures/Figure3.pdf", sep = "/"), base_width = 18, base_height = 10)


## Debug calculations
perFrameAvgCorrStats %>% arrange(desc(vSuscept_mean)) %>% slice(-1) %>%
  summarise(normalizedDiam = max(EquivalentDiameter_mean) / min(EquivalentDiameter_mean), 
            norm_vZ = max(vZero_mean) / min(vZero_mean),
            norm_vS = max(vSuscept_mean) / min(vSuscept_mean))

perFrameAvgCorrStats_sim %>% filter(Size != min(Size)) %>% group_by(Noise) %>% summarise(sizeScale = sqrt(max(Size) / min(Size)), 
                                                           susceptScale = max(vSuscept_mean) / min(vSuscept_mean),
                                                           vZeroScale = max(vZero_mean) / min(vZero_mean))
