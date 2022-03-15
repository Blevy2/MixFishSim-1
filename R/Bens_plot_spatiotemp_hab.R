#' @title Plot spatiotemporal habitat suitability

#' @description Function to plot out the habitat suitability, as adjusted by
#' the spatiotemporal move covariates

#' @param hab is the output from \link{create_hab}
#' @param moveCov is the output from \link{init_moveCov}
#' @param plot.file path to save the plots of the spatiotemporal habitats 
#' @param spwn_wk is a named list of the spawning week for each population

#' @examples None

#' @export

plot_spatiotemp_hab <- function(hab = NULL, moveCov = NULL, plot.file = NULL, spwn_wk = NULL, plot_wk = NULL, plot_monthly = NULL,colrange = NULL) {

	nrows <- nrow(hab[["hab"]][[1]]) 
	ncols <- ncol(hab[["hab"]][[1]])
	
	if(!is.null(plot.file)) {
	png(filename = paste0(plot.file,'/','habitat_spatiotemp_spp_','.png'), width = 800, height = 800)
	}

for(s in seq_len(length(hab[["hab"]]))) {

	nt <- length(moveCov[["cov.matrix"]])

	par(mfrow = c(ceiling(sqrt(length(plot_wk))), ceiling(length(plot_wk)/ceiling(sqrt(length(plot_wk))))), mar = c(1, 1, 1, 1))

	for(i in plot_wk) {

	move_cov_wk <- moveCov[["cov.matrix"]][[i]]

	move_cov_wk_spp <- matrix(nc = ncols,
				 nr = nrows, 
				 sapply(move_cov_wk, norm_fun, 
                          mu = moveCov[["spp_tol"]][[s]][["mu"]], 
		  	  va = moveCov[["spp_tol"]][[s]][["va"]]))
#col = grey(seq(1,0,l = 51)),
		if(!i %in% spwn_wk[[s]]) {
		fields::image.plot(hab[["hab"]][[paste0('spp',s)]] * move_cov_wk_spp, cex.axis = 1.5, cex.main = 2,  axes = F)
		}
	# col = grey(seq(1,0,l = 51)),
		if(i %in% spwn_wk[[s]]) {
		fields::image.plot(hab[["spwn_hab"]][[paste0('spp',s)]] * move_cov_wk_spp, cex.axis = 1.5, cex.main = 1, axes = F )
		}
#	axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
#	axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
	text(0.5, 0.98, labels = paste('week', i), cex = 1)

	}
}
	dev.off()
	
	
	
	
	
	#plot a week from each month over entire simulation

	if(plot_monthly == TRUE){	
	  
	  if(!is.null(plot.file)) {
	    
	    pdf(file=paste0(plot.file,'/Monthly_covariate_plots/','monthly_habitat_spatiotemp_spp_','month_','.pdf'))
	    #png(filename = paste0(plot.file,'/Monthly_covariate_plots/','monthly_habitat_spatiotemp_spp_',s,'month_',k,'.png'), width = 800, height = 800)
	  }
	for(s in seq_len(length(hab[["hab"]]))) {
	  
  
	for(k in seq(12)){
	  
	  	  nt <- length(moveCov[["cov.matrix"]])
	
	  par(mfrow = c(5,4), mar = c(1, 1, 1, 1))
	
	  
	for(i in seq(1,nt,52)){
	  
	  month_shift <- 4*(k-1)
	  
	  move_cov_wk <- moveCov[["cov.matrix"]][[i+month_shift]]
	  
	  move_cov_wk_spp <- matrix(nc = ncols,
	                            nr = nrows, 
	                            sapply(move_cov_wk, norm_fun, 
	                                   mu = moveCov[["spp_tol"]][[s]][["mu"]], 
	                                   va = moveCov[["spp_tol"]][[s]][["va"]]))
	  #col = grey(seq(1,0,l = 51)), 
	  if(!i %in% spwn_wk[[s]]) {
	    fields::image.plot(hab[["hab"]][[paste0('spp',s)]] * move_cov_wk_spp, cex.axis = 1.5, cex.main = 2, axes = F)
	  }
	  # col = grey(seq(1,0,l = 51)),
	  if(i %in% spwn_wk[[s]]) {
	    fields::image.plot(hab[["spwn_hab"]][[paste0('spp',s)]] * move_cov_wk_spp, cex.axis = 1.5, cex.main = 1, axes = F )
	  }
#	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
#	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
	  text(0.5, 0.98, labels = paste('week', i), cex = 1)
	  
	  
	}
	  
	  
	}
	}
	}
	dev.off()
	

	
	
}
