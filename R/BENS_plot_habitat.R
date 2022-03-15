#' @title Plot the habitat distribution fields for all populations
#'
#' @description \code{plot_habitat} is a function to plot the habitat spatial
#' fields.
#'
#' @param hab is the habitat to plot, either unadjusted or adjusted for
#' spawning area. 

#' @return Silently plots the habitat fields (can be either the unadjusted
#' habitat, or including the spawning adjustment).

#' @examples
#' plot_habitat(hab = hab)

#' @export

BENS_plot_habitat <- function(hab = hab, spp.ctrl = spp.ctl) {

	n.spp <- length(hab)
	nrows <- dim(hab[[1]])[1]
	ncols <- dim(hab[[1]])[2]

	
	


	
	show("plotting inside plot_habitat")
	
	par(mfrow = c(ceiling(sqrt(n.spp)), ceiling(n.spp/ceiling(sqrt(n.spp)))), mar = c(2, 2, 2, 2))
	for(i in seq(n.spp)) {
	  par(pty="s") #this makes the plots square to compare to publication
	image(hab[[paste0("spp", i)]], cex.axis = 1.5, cex.main = 2, col = grey(seq(1,0, l = 51)), axes = F)
	axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
	axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
	text(0.1, 0.98, labels = paste("habitat spp = ", i), cex = 1)
	
	
	nu <- round( spp.ctrl[[paste0("spp.",i)]][["nu"]], digits = 1)
	var <- round( spp.ctrl[[paste0("spp.",i)]][["var"]], digits = 1)
	scale <- round( spp.ctrl[[paste0("spp.",i)]][["scale"]], digits = 1)
	Aniso <- round( spp.ctrl[[paste0("spp.",i)]][["Aniso"]], digits = 1)
	
	
	text(0.1, 0.88, labels = paste("nu = ", nu), cex = 1)
	text(0.1, 0.78, labels = paste("var = ", var), cex = 1)
	text(0.1, 0.68, labels = paste("scale = ", scale), cex = 1)
	text(0.1, 0.58, labels = paste("Aniso = ", Aniso[1],Aniso[2],Aniso[3],Aniso[4]), cex = 1)
	}
	
}


