#' @title Plot spatiotemporal habitat suitability

#' @description Function to plot out the habitat suitability, as adjusted by
#' the spatiotemporal move covariates

#' @param hab is the output from \link{create_hab}
#' @param moveCov is the output from \link{init_moveCov}
#' @param plot.file path to save the plots of the spatiotemporal habitats 
#' @param spwn_wk is a named list of the spawning week for each population

#' @param colrange is the color range to use in the image plots. Best to set it as the range of all possible images 

#' @examples None

#' @export

BENS_plot_spatiotemp_hab_justtemp <- function(hab = NULL, moveCov = NULL, plot.file = NULL, spwn_wk = NULL, plot_wk = NULL, colrange = NULL) {
  
  nrows <- nrow(hab[["hab"]][[1]]) 
  ncols <- ncol(hab[["hab"]][[1]])
  
  library(fields)
  library(lattice)
  library(RColorBrewer)
  
  #plot just the temperature gradient
  
  nt <- length(moveCov[["cov.matrix"]])
  if(!is.null(plot.file)) {
    png(filename = paste0(plot.file,'/','justtemp_spatiotemp.png'), width = 800, height = 800)
    #pdf(file=paste0(plot.file,'/','justtemp_spatiotemp.pdf'))	  
  }
  par(mfrow = c(ceiling(sqrt(length(plot_wk))), ceiling(length(plot_wk)/ceiling(sqrt(length(plot_wk))))), mar = c(1, 1, 1, 1))
  
  for(i in plot_wk) {
    
    move_cov_wk <- moveCov[["cov.matrix"]][[i]]
    
    #move_cov_wk <- move_cov_wk[,nrow(move_cov_wk):1] # Attempt 1: THIS PART ORIENTS THE IMAGE FOR PLOTTING
    #  move_cov_wk <- apply(move_cov_wk,1,rev)
    
    move_cov_wk <- t(move_cov_wk[nrow(move_cov_wk):1,]) # Attempt 2: THIS PART ORIENTS THE IMAGE FOR PLOTTING
    

    
    
    #move_cov_wk <- matrix(unlist(moveCov[["cov.matrix"]][[i]]), ncol = ncols, nrow= nrows)
    
  #  coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
 #   levelplot(move_cov_wk, col.regions = coul) # try cm.colors() or terrain.colors()  
 # plot<- levelplot(move_cov_wk, col.regions = coul) # try cm.colors() or terrain.colors()  
#  print(plot)
    # col = heat.colors(12)
    image.plot(move_cov_wk, cex.axis = 1.5, cex.main = 2, axes = F, zlim=colrange)
   # axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
  #  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
    text(0.5, 0.98, labels = paste('week', i), cex = 1)

    

    
    
  }
  dev.off()
  
  
  
  
  
  
  #plot species-specific temp preferences
  
  for(s in seq_len(length(hab[["hab"]]))) {
    
    nt <- length(moveCov[["cov.matrix"]])
    if(!is.null(plot.file)) {
      png(filename = paste0(plot.file,'/','justtemp_spatiotemp_spp_',s,'.png'), width = 800, height = 800)
    }
    par(mfrow = c(ceiling(sqrt(length(plot_wk))), ceiling(length(plot_wk)/ceiling(sqrt(length(plot_wk))))), mar = c(1, 1, 1, 1))
    
    for(i in plot_wk) {
      
      move_cov_wk <- moveCov[["cov.matrix"]][[i]]
      
      move_cov_wk_spp <- matrix(nc = ncols,
                                nr = nrows, 
                                sapply(move_cov_wk, norm_fun, 
                                       mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                       va = moveCov[["spp_tol"]][[s]][["va"]]))
      
      move_cov_wk_spp <- t(move_cov_wk_spp[nrow(move_cov_wk_spp):1,]) # Attempt 2: THIS PART ORIENTS THE IMAGE FOR PLOTTING
      
      
      if(!i %in% spwn_wk[[s]]) {
       #, col = heat.colors(12)
        image.plot(move_cov_wk_spp, cex.axis = 1.5, cex.main = 2, axes = F)
      }
      
      if(i %in% spwn_wk[[s]]) {
        col = grey(seq(1,0,l = 51))
        image.plot( move_cov_wk_spp, cex.axis = 1.5, cex.main = 1,  axes = F)
      }
    #  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
    #  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
      text(0.5, 0.98, labels = paste('week', i), cex = 1)
      
    }
    dev.off()
  }
  
  
}
