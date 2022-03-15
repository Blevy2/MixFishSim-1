#' @title Initialise movement covariates
#'
#' @description This function creates a list of covariates, to be used 

#' @param sim_init is the output from the function \link{init_sim}.
#' @param steps is a Numeric with the number of timesteps over which the
#' covariate changes
#' @param spp_tol is a named list (each species) with a list of mean (mu) and
#' variance (va) for the normal distribution for thermal tolerance.

#' @examples None

#' @export

init_moveCov_Bens2 <- function (sim_init = NULL, steps = NULL, spp_tol = NULL) {	

	## Create list of matrices to capture spatio-temporal move covariates

	nx <- sim_init[["idx"]][["ncols"]]
	ny <- sim_init[["idx"]][["nrows"]]

	cov.matrix <- list()

	for(i in 1:steps) {

		if(i == 1) {
	temp_original <-matrix(NA,nrow=nx, ncol=ny)
	temp_original <-row(temp_original) + col(temp_original)  ## From South-West to North-East
	covariate.trend<- (temp_original-1)/10 #define cost values
	covariate.trend <- matrix(covariate.trend, nx , ny) #assign costs 
	cov.matrix[[i]] <- covariate.trend
	covariate.trend_base <- covariate.trend
  
	val <- 1
		}

		if(i > 1) {
		  
		  
		  if(i %% 52 == 0){ #reset each year but increment val each year
		    
		    print("i mod 52 is 1")
		    print(i)
		    
		   
		    
		    temp <-matrix(NA,nrow=nx, ncol=ny)
		    temp <-row(temp_original) + col(temp_original)  ## From South-West to North-East
		    
		    #new part to increase initial temp value each year
		  		   val <- val + 0.01  #is this enough?  
		  		   temp <- val*temp
		    
		    covariate.trend<- (temp-1)/10 #define cost values
		    covariate.trend <- matrix(covariate.trend, nx , ny) #assign costs 
		    cov.matrix[[i]] <- covariate.trend
		    covariate.trend_base <- covariate.trend
		    

		    
		  }
		  
      if(i %% 52 != 0){
		   #during each year keep val the same
			covariate.trend <- ifelse(row(temp_original) + col(temp_original) < ((nx + ny) / 2), #this creates a matrix where the upper half is true and the lower half is false going along the antidiagonal
						  covariate.trend_base - (10 * ((i%%52)/52)),  #52 used to be steps
						  covariate.trend_base + (10 * ((i%%52)/52)))
			cov.matrix[[i]] <- covariate.trend
                      }
		  
		  

		  
			}
}


	return(list(cov.matrix = cov.matrix, spp_tol = spp_tol))

}
