visualizeSpeedAnglePlots = function(workingFolder) {
  originalDirectory = getwd()
  setwd(workingFolder)
  dir.create("SpeedAnglePlots")
  trajectoryData = read.csv("ContiguousTrajectory.csv") %>% mutate(CentroidY = -CentroidY)
  validFrames = filter(trajectoryData, !invalidFrame) %>% select(Frame)
  opticalFlowFrames = h5read("longformOpticalFlow.hdf5","/longformFrameIndices") %>% as.data.frame()
  validFrames = filter(validFrames, Frame %in% opticalFlowFrames$V1)

  for (frameIndex in 1:length(validFrames$Frame)) {
    if (!file.exists(paste('SpeedAnglePlots/Frame',frameIndex,'.jpg',sep=""))) {
      frameNumber = validFrames$Frame[frameIndex]
      thisFrame = readVelocityFieldAtFrame("longformOpticalFlow.hdf5",frameNumber)[[1]]
      thisFrame = thisFrame %>% mutate(R = R*8, C = C*8)
      trackingFrames = filter(trajectoryData,Frame <= frameNumber & Frame)
      thisFrame = thisFrame %>% mutate(speed = sqrt(u^2+v^2), globalR = R - mean(R) + tail(trackingFrames$CentroidY,1), globalC = C - mean(C) + tail(trackingFrames$CentroidX,1))

      rangeBuff = 1000
      frameRanges = c(mean(thisFrame$globalC) - rangeBuff, mean(thisFrame$globalC) + rangeBuff, mean(thisFrame$globalR) - rangeBuff, mean(thisFrame$globalR) + rangeBuff)

      # Get the heading angles of the vectors.
      thisFrameVectors = select(thisFrame,u,v)
      thisFrame = thisFrame %>% mutate(absAngle = getAngles(positions = thisFrameVectors, fromPrevious = F)*180/pi)

      p = plotDirectionSpeed(thisFrame, trackingFrames, frameRanges)
      ggsave(filename = paste('SpeedAnglePlots/Frame',frameIndex,'.jpg',sep=""), plot = p)
    }
  }
  setwd('SpeedAnglePlots/')
  system('ffmpeg -start_number 1 -r 60 -i Frame%d.jpg -q:v 0 movie.avi')
}


plotDirectionSpeed = function(frameData, trackingFrames, frameRanges) {
  require(ggplot2)
  cbPalette = c("magenta","red","yellow","green","cyan","blue","magenta")
  numTrackPoints = 100
  maxFrame = max(trackingFrames$Frame)
  oldFrames = filter(trackingFrames, Frame < maxFrame - numTrackPoints)
  currentFrames = filter(trackingFrames,Frame >= maxFrame - numTrackPoints)

  p = frameData %>% ggplot() +
    theme(axis.title = element_blank(), panel.grid.major = element_line(colour = "black", linetype = "dotted")) +
    geom_path(data = oldFrames,aes(x=CentroidX,y=CentroidY), alpha=0.5, color="gray") +
    geom_tile(aes(x=globalC,y=globalR, fill=absAngle, alpha=speed)) +
    geom_path(data = currentFrames,aes(x=CentroidX,y=CentroidY,col=Frame)) +
    ggtitle(paste(frameData$Frame[1]/2, "s, Frame:", frameData$Frame[1])) + coord_fixed(ratio = 1) +
    xlim(frameRanges[1],frameRanges[2]) + ylim(frameRanges[3],frameRanges[4]) +
    scale_colour_gradient(low="blue", high="red", limits=c(maxFrame - numTrackPoints, maxFrame)) +
    scale_fill_gradientn(colours = cbPalette, limits=c(-180, 180)) + scale_alpha(limits = c(0,6)) +
    guides(colour = guide_colorbar(order=1), fill = guide_colorbar(order = 2), alpha = guide_legend(order = 3))
  return(p)
}


plotSpeedVector = function(frameData, trackingFrames, frameRanges) {
  require(ggplot2)
  cbPalette = c("magenta","red","yellow","green","cyan","blue","magenta")
  numTrackPoints = 100
  maxFrame = max(trackingFrames$Frame)
  sf <- scale_fill_gradientn(colours = myPalette(100), limits=c(0, 15))

  p = frameData %>% ggplot() +
    theme(axis.title = element_blank(), panel.grid.major = element_line(colour = "black", linetype = "dotted")) +
    geom_tile(aes(x=globalC,y=globalR, fill=speed)) +
    geom_path(data = trackingFrames,aes(x=CentroidX,y=CentroidY,col=Frame)) +
    ggtitle(paste(frameData$Frame[1]/2, "s, Frame:", frameData$Frame[1])) +
    coord_fixed(ratio = 1) +
    xlim(frameRanges[1],frameRanges[2]) + ylim(frameRanges[3],frameRanges[4])  +
    scale_colour_gradient(low="blue", high="red", limits=c(maxFrame - numTrackPoints, maxFrame)) +
    scale_fill_gradientn(colours = myPalette(100), limits=c(0, 15)) +
    guides(colour = guide_colorbar(order=1), fill = guide_colorbar(order = 2))
  return(p)
}

# # command line input
# option.list <- list(
#   make_option(c("-f", "--folder"), action="store_true", default=FALSE,
#               help="The folder to be processed.")
# )
#
# parser <- OptionParser(
#   usage = "%prog [options] results-path",
#   option_list = option.list
# )
# arguments <- parse_args(parser, positional_arguments=TRUE)
# opt       <- arguments$options
# path      <- arguments$args
#
# fmt <- function(d) format(d, decimal.mark='d', sci=FALSE)
# file <- paste("results",
#               "_n",      fmt(opt$nodes),
#               "_p",      fmt(opt$edge_probability),
#               "_pmin",   fmt(opt$pmin),
#               "_pmax",   fmt(opt$pmax),
#               "_pinc",   fmt(opt$pinc),
#               "_prange", opt$prange,
#               "_beta",   fmt(opt$beta),
#               "_sigma",  fmt(opt$sigma),
#               "_G",      opt$network_type,
#               "_sim",    opt$sim,
#               "_rep",    opt$rep,
#               "_reps",   fmt(opt$reps),
#               ".Rdata",  sep=''
# )
# if (length(path) > 0) {
#   file <- paste(path, file, sep='/')
# }
#
# message("results")
# message("    ", file)
# message("\nparameters")
# message("    n      = ", opt$nodes)
# message("    pmin   = ", opt$pmin)
# message("    pmax   = ", opt$pmax)
# message("    pinc   = ", opt$pinc)
# message("    prange = ", opt$prange)
# message("    beta   = ", opt$beta)
# message("    sigma  = ", opt$sigma)
# message("    G      = ", opt$network_type)
# message("    sim    = ", opt$sim)
# message("    rep    = ", opt$reps)
# message("    reps   = ", opt$reps)
#
# n     <- opt$nodes
# p     <- opt$edge_probability
# beta  <- opt$beta
# sigma <- opt$sigma
#
# if (opt$prange) {
#   p <- opt$pmin
# }
#
# message("\nrun")
# results <- c()
#
# i <- opt$rep
# #for (i in 1:opt$reps) {
# repeat {
#   message("___________________________________")
#   message("i = ", i, "/", opt$reps, ", p = ", p)
#
#   # network construction
#   G <- NULL
#   if (opt$network_type == "ER") {
#     G <- erdos.renyi.game(n,p,type="gnp")
#   } else if (opt$network_type == "spatial") {
#     G <- spatial.network(n,p,beta,sigma)$G
#   } else {
#     message("Network type doesn't exist.")
#     stop(1)
#   }
#
#   # network properties
#   net.type               <- opt$network_type
#   edges                  <- ecount(G)
#   tris                   <- all.triangles(G)
#   triangles              <- ifelse(length(tris) < 3, 0, nrow(tris))
#   mean.degree            <- mean(degree(G))
#   mean.pair.degree       <- mean(choose(degree(G),2))
#   clustering.coefficient <- transitivity(G,type="global")
#
#   # connected components
#   cc.strong <- clusters(G, mode="strong")
#   cc.weak   <- clusters(G, mode="weak")
#   eq.pair   <- equiv.classes(G, tris)
#
#   # giant components
#   gc.strong <- sort(cc.strong$csize,decreasing=TRUE)[1]
#   gc.weak   <- sort(cc.weak$csize,decreasing=TRUE)[1]
#   gc.pair   <- max(c(0,unlist(lapply(eq.pair,length))))
#
#   sim.activated <- c()
#   if (opt$sim > 0) {
#     message("running simulations...")
#     for (j in 1:100) { #opt$sim) {
#       active <- sim.contagion(G,tris)
#       sim.activated <- c(sim.activated, length(active))
#     }
#   }
#
#   message("    net.type               = ", net.type)
#   message("    edges                  = ", edges)
#   message("    triangles              = ", triangles)
#   message("    mean degree            = ", mean.degree)
#   message("    mean pair degree       = ", mean.pair.degree)
#   message("    clustering coefficient = ", clustering.coefficient)
#   message("    gc strong              = ", gc.strong)
#   message("    gc weak                = ", gc.weak)
#   message("    gc pair                = ", gc.pair)
#   if (opt$sim > 0) {
#     message("    sim mean activated     = ", mean(sim.activated))
#     message("    sim sd activated       = ", sd(sim.activated))
#     message("    sim min activated      = ", min(sim.activated))
#     message("    sim max activated      = ", max(sim.activated))
#   }
#
#   # accumulate results
#   new.results <- data.frame(
#     type                   = net.type,
#     i                      = i,
#     n                      = n,
#     p                      = p,
#     beta                   = beta,
#     sigma                  = sigma,
#     edges                  = edges,
#     triangles              = triangles,
#     gc.strong              = gc.strong,
#     gc.weak                = gc.weak,
#     gc.pair                = gc.pair,
#     mean.degree            = mean.degree,
#     mean.pair.degree       = mean.pair.degree,
#     clustering.coefficient = clustering.coefficient
#   )
#   if (opt$sim > 0) {
#     new.results <- cbind(new.results, data.frame(
#       sim.mean = mean(sim.activated),
#       sim.sd   = sd(sim.activated),
#       sim.min  = min(sim.activated),
#       sim.max  = max(sim.activated)
#     ))
#     sim.results <- data.frame(sim.activated = sim.activated)
#     new.results <- cbind(new.results, sim.results)
#   }
#   results <- rbind(results, new.results)
#
#   # record results
#   save(results, file=file)
#   message("recorded results")
#
#   if (!opt$prange || p >= opt$pmax) { break }
#   p <- min(max(p + opt$pinc, 0), 1)
# }
# #}
#
# # record results
# save(results, file=file)
