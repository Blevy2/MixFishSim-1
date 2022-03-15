#' @title Initialise survey settings
#'
#' @description \code{init_survey} is a function to mimic a
#' fisheries-independent survey to sample catches from the populations.
#'
#' @param sim_init is the general simualtion settings from \link{sim_init}
#' @param design is the survey design used, at the moment only
#' \emph{fixed_station}
#' @param n_stations is a Numeric for the number of stations to be fished each.
#' Note: If using 'fixed_station' design this will be rounded down to maintain
#' a grid shape if not divisble.  
#' @param start_day is a Numeric for the first day of the survey each year
#' @param stations_per_day is a Numeric for the number of stations surveyed per
#' day
#' @param Qs is a named Numeric Vector containing any survey catchabilities, assumed to be time invariant.
#'
#' @param strata_coords is created by init_hab and contains the corner coordinates of each strata (if rectangular)
#'
#' @param strata_num is created by init_hab. It contains the entire domain with each of the n strata labeled 1 to n 
#' 
#' @param years_cut cuts off year_shift from the start of the survey
#'
#' @return is a list consisting of the survey setting and a a matrix for
#' storing the log of catches from the survey, to be used as an input to
#' \link{run_sim}.

#' @examples
#' init_survey(design = 'fixed_station', n_stations = 50, start_day = 90, stations_per_days = 5, Qs = c("spp1" = 0.1, "spp2" = 0.2)

#' @export

BENS_init_survey <- function (sim_init = NULL, design = 'fixed_station', n_stations = 50, 
			     start_day = 90, stations_per_day = 5, Qs = NULL, strata_coords = NULL, strata_num = NULL, years_cut = 0) {

	# useful indexes
	idx       <- sim_init[["idx"]]
	n_fleets  <- idx[["nf"]]
	n_vessels <- idx[["nv"]]
	brk.idx   <- sim_init[["brk.idx"]]
	
	n_strata <- length(strata_coords)


	### Checks
	if(!start_day %in% sim_init[["brk.idx"]][["day.seq"]]) stop("We don't fish this day, choose another start_day!")

	## determine design

	if(design == 'fixed_station') {

	# Create a grid of stations
	x <- round(seq(1,idx[["ncols"]], length.out = round(sqrt(n_stations),0)),0) # sim_init[["idx"]][["n_rows"]], 1)
	y <- round(seq(1, idx[["nrows"]], length.out = round(sqrt(n_stations),0)),0) # sim_init[["idx"]][["n_cols"]], 1)
	grid <- cbind(x = x, y = rep(y, each = length(x)))

	# update no stations 
	n_stations <- nrow(grid)
	
	
	##########################
	## set up FIXED survey log matrix
	##########################
	
	#create the repeating pattern for the station days
	station_days <- rep(sim_init[["brk.idx"]][["day.seq"]][which(sim_init[["brk.idx"]][["day.seq"]] == start_day, arr.ind = T):c(which(sim_init[["brk.idx"]][["day.seq"]] == start_day, arr.ind = T) + round(n_stations / stations_per_day, 0) - 1)], each = stations_per_day)[seq_len(n_stations)]
	
	
	log.mat 	       <- matrix(NA, nrow = n_stations * sim_init[["idx"]][["ny"]], 
	                          ncol = 6 + idx[["n.spp"]])
	colnames(log.mat)      <- c("station_no", "x","y","day","tow","year",
	                            paste0("spp",seq(idx[["n.spp"]])))
	log.mat[,'station_no'] <- rep(seq_len(n_stations), sim_init[["idx"]][["ny"]])
	log.mat[,'x']          <- rep(grid[,"x"], times = sim_init[["idx"]][["ny"]])
	log.mat[,'y']          <- rep(grid[,"y"], times = sim_init[["idx"]][["ny"]])
	log.mat[,'day']        <- rep(station_days, times = sim_init[["idx"]][["ny"]])
	log.mat[,'tow']        <- rep(seq_len(n_stations), sim_init[["idx"]][["ny"]]) 
	log.mat[,'year']       <- rep(seq_len(sim_init[["idx"]][["ny"]]), each = n_stations)
	
	
	return(list(survey_settings = c("design" = design, "n_stations" =
	                                  n_stations, "days_fished" = round(n_stations / stations_per_day, 0),
	                                "Qs" = Qs), log.mat = log.mat))
	}

	
	
	
	
	if(design == "random_station") {

	  #figure out how many strata there are by finding unique values in each each row and then taking the unique values found in all rows
	  new_strat <- vector()
	  	  for(i in seq(1:length(strata_num[,1]))){
	    new_strat <- c(new_strat,unique(strata_num[i,]))
	     }
	  unique_numbers <- unique(new_strat)
	  
#setup list that 1 entry for each strata
	  strata_index_list <- vector(mode = "list", length = max(unique_numbers))


	  
	  #prepare indices for each strata 
	  index <- vector()  #empty vector
	  for(j in unique_numbers){
	    index[j]<-0

	
	  #go through entire strata and number each strata 1 to n where n is the total elements in the strata
	  strata_index <- matrix(0,nrow=idx[["nrows"]],idx[["ncols"]])
	  
	  for(i in seq(1:length(strata_num))){
	    
	    if(strata_num[i] == unique_numbers[j]){
	      strata_index[i]<-index[strata_num[i]] +1
	      index[strata_num[i]]<-index[strata_num[i]]+1
	      
	      
	      }
	      else{strata_index[i]<-0}

	    
	  }	  
	  #have created matirx with 1 to n in correct strata and 0 elsewhere
	  #store each in list
	  strata_index_list[[j]]<-strata_index
	  }
	  
	  
	  
	  
	  #go through each strata_index_list, choose correct number of random stations
	  #translate back to index for given strata
	  coords<-vector()
	  
	  #while you do keep track of strata number
	  str_num <-vector()
	  	   yrs <- vector()
	  for(j in unique_numbers){
	    print(j)
	    #index[j] is how many total stations there are in each strata
	    #currently dividing total number of samples evening among each strata
	    
	   ifelse(index[j]>=(n_stations[[j]]*(sim_init[["idx"]][["ny"]]-years_cut)),
	                     {my_sample <- sample(index[j],n_stations[[j]]*(sim_init[["idx"]][["ny"]]-years_cut),replace = FALSE)},
	                     {print("not enough sampling locations. sampling this strata with replacement")
	                     my_sample <- sample(index[j],n_stations[[j]]*(sim_init[["idx"]][["ny"]]-years_cut),replace = TRUE)})
#	print("max/min is")
#	print(max(my_sample))
#	print(min(my_sample))
#	print("length index j")
#	print(length(index[j]))
#	View(index)
	    #find index for each sample
#	    print(range(my_sample))
#	    print(j)
	   

	   nsamps <- length(my_sample)
	 #  print(nsamps)
	    for(i in seq(1:nsamps)){
	    coords <- c(coords,which(strata_index_list[[j]]==my_sample[i]))

	    #record strata number
	    str_num <-c(str_num,unique_numbers[j])
	    
	    #record year
	   
	    }
	     yrs <- c(yrs,rep(seq(years_cut+1,sim_init[["idx"]][["ny"]]), each = nsamps/(sim_init[["idx"]][["ny"]]-years_cut)))
#	    print(length(coords))   #LENGTH OF COORDS IS NOT ALWAYS THE SAME WHICH MEANS IT CANT ALWAYS FIND MY_SAMPLE VALUES IN STRATA_INDEX_LIST 
	  }
	  
	
	  #translate matrix index into x,y coordinate  
	
      row_dist <- length(strata_num[,1]) #specifies size of given strata
      col_dist <- length(strata_num[1,])

	  
	  dim.mat = c(row_dist,col_dist) #dimension of given strata
	  

	  coord <- matrix( NA,nrow=1,ncol=2)
	  
	  
	  x<-vector()
	  y<-vector()
	  
#	  View(coords)
	#  View(str_num)
	  
	  #translate each matrix position into an (x,y) index for given strata
	  for(i in coords){
	 
	    

        pos <- i	    

	      
	      coord[1,1] <- ((pos-1) %% dim.mat[1]) +1  #xcoordinate
	      coord[1,2] <- ((pos-1) %/% dim.mat[1]) +1 #y coordinate
	   #  show(pos)
	   	      
#	    coords <- pos2coord(pos=i,dim.mat = dim.mat)   #I extracted the above code from this function file 
	      
	      

	    
#tack onto existing coordinates 
	    x <- c(x,coord[1,1])
	    
	    y <- c(y,coord[1,2]) 


	  }
    
#View(x)

	##########################
	## set up RANDOM survey log matrix
	##########################
	
	#create the station days
#	station_days <- rep(sim_init[["brk.idx"]][["day.seq"]][which(sim_init[["brk.idx"]][["day.seq"]] == start_day, arr.ind = T):c(which(sim_init[["brk.idx"]][["day.seq"]] == start_day, arr.ind = T) + round(n_stations / stations_per_day, 0) - 1)], each = stations_per_day)[seq_len(n_stations)]
	
	
	log.mat 	       <- matrix(NA, nrow = length(x), 
	                          ncol = 7 + idx[["n.spp"]])
#	View(log.mat)
	
#	print(str_num)
#print(length(str_num))
	
#	print(length(log.mat[,1]))
	
	colnames(log.mat)      <- c("station_no", "x","y","strata","day","tow","year",
	                            paste0("spp",seq(idx[["n.spp"]])))
	log.mat[,'station_no'] <- coords #different (matrix number from total domain)
	log.mat[,'x']          <- x  #different than fixed station section
	log.mat[,'y']          <- y  #different than fixed station section
	log.mat[,'strata']     <- str_num #new
#	log.mat[,'day']        <- rep(station_days, times = sim_init[["idx"]][["ny"]]-years_cut)
#	log.mat[,'tow']        <- rep(seq_len(n_stations), sim_init[["idx"]][["ny"]]-years_cut) 
	log.mat[,'year']       <- yrs
	
	
	return(list(survey_settings = c("design" = design, "n_stations" =
	                                  n_stations, "days_fished" = round(n_stations / stations_per_day, 0),
	                                "Qs" = Qs), log.mat = log.mat, "cells_per_strata" = index))
	
	#check for duplicate stations
	
	if(anyDuplicated(log.mat[,2:3])!=0){"There are duplicated sampling stations"}

    
	}

}
