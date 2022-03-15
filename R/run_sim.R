#' @title Run sim

#' @description \code{run_sim} is the overarching simulation function, taking
#' all the parameterised inputs and returning the results.

#' @param sim_init is the parameterised simulation settings from
#' \code{init_sim}
#' @param pop_init is the parameterised populations from \code{init_pop}
#' @param move_cov is a parameterised movement covariate object, from
#' \code{init_moveCov}
#' @param fleets_init is the parameterised fleets from \code{init_fleets}
#' @param hab_init is the parameterised habitat maps from \code{create_hab}
#' @param save_pop_bio is a logical flag to indicate if you want to record #' true spatial population at each time step (day)
#' @param survey is the survey settings from \link{init_survey}, else NULL if no survey is due to be simulated
#' @param closure is the spatial closure settings from \link{init_closure}
#' else NULL if no closures are to be implemented

#' @return is the results...

#' @examples Not yet
#'
#' @export

run_sim <- function(MoveProb = NULL, MoveProb_spwn = NULL, sim_init = NULL, pop_init = NULL, move_cov = NULL, fleets_init = NULL, hab_init = NULL, save_pop_bio = FALSE, survey = NULL, closure = NULL,...) {
  # Overarching function for running the simulations
  
  start.time <- Sys.time() # for printing runtime
  
  #######################
  ####### Indices #######
  #######################
  
  
  # Extract these to make indexing easier 
  
  ntow         <- sim_init[["idx"]][["ntow"]] # length of t loop
  n_fleets     <- sim_init[["idx"]][["nf"]]  
  n_vess       <- sim_init[["idx"]][["nv"]]  
  n_spp        <- sim_init[["idx"]][["n.spp"]]  
  ncols        <- sim_init[["idx"]][["ncols"]]  
  nrows        <- sim_init[["idx"]][["nrows"]]  
  n_weeks        <- sim_init[["idx"]][["nw"]]  
  
  day.breaks     <- sim_init[["brk.idx"]][["day.breaks"]]  
  week.breaks    <- sim_init[["brk.idx"]][["week.breaks"]]  
  trip.breaks    <- sim_init[["brk.idx"]][["trip.breaks"]]  
  month.breaks   <- sim_init[["brk.idx"]][["month.breaks"]]  
  year.breaks    <- sim_init[["brk.idx"]][["year.breaks"]]
  
  #new to use all moveCov matrices
  week.breaks.all <-sim_init[["brk.idx"]][["week.breaks.all"]]
  
  ###################################
  ##### Temporary containers ########
  ###################################
  
  Rec  <- vector("list", n_spp) # For storing spatial recruitment, is overwritten
  for(s in paste0("spp",seq_len(n_spp))) { Rec[[s]] <- 0 }
  B    <- pop_init[["Start_pop"]]  # For storing current biomass, is overwritten
  Bm1  <- pop_init[["Start_pop"]]  # For storing last time-step biomass, is overwritten
  
  AreaClosures <- NULL
  close_count <- 0 # counter for recording closures
  
  
  # preallocate closure list
  
  if(is.null(closure)) { CalcClosures  <- FALSE }
  
  if(!is.null(closure)) {
    
    
    if(closure[["temp_dyn"]] == 'yearly') {
      closure_list <- vector("list", sim_init[["idx"]][["ny"]] - closure[["year_start"]])
    }
    
    if(closure[["temp_dyn"]] == 'monthly') {
      closure_list <- vector("list", 12 * (sim_init[["idx"]][["ny"]] - closure[["year_start"]]))
    }
    
    if(closure[["temp_dyn"]] == 'weekly') {
      closure_list <- vector("list", 52 * (sim_init[["idx"]][["ny"]] - closure[["year_start"]]))
    }
    
  }
  
  if(is.null(closure)) { closure_list <- NULL} 
  
  # ###################################
  # ###### Move probabilities #########  I MOVED THESE OUT OF HERE TO IMPROVE SPEED
  # ###################################
  print("Calculating movement probabilities")
  
  MoveProb  <- lapply(paste0("spp", seq_len(n_spp)), function(s) { move_prob_Lst(lambda = pop_init[["dem_params"]][[s]][["lambda"]], hab = hab_init[["hab"]][[s]])})
  
  
  MoveProb_spwn <- lapply(paste0("spp", seq_len(n_spp)), function(s) { move_prob_Lst(lambda = pop_init[["dem_params"]][[s]][["lambda"]], hab = hab_init[["spwn_hab"]][[s]])})
  
  #View(MoveProb)
  # print(range(MoveProb[[1]]))
  # 
  # print(range(MoveProb[[2]]))
  
  names(MoveProb)      <- paste0("spp", seq_len(n_spp))
  names(MoveProb_spwn) <- paste0("spp", seq_len(n_spp))
  
  
  
  #CALULATE INDICES OF NONZERO VALUES IN HAB TO PASS TO MOVE_POPULAITON DURING MOVEMENT
  nonzero_idx <- lapply(paste0("spp", seq_len(n_spp)), function(s) {
   
    which(hab_init[["hab"]][[s]] !=0 , arr.ind=T)
   
  })
  
  names(nonzero_idx) <- paste("spp",seq(sim_init[["idx"]][["n.spp"]]), sep ="")
  
# View(nonzero_idx)

  #  View(MoveProb)
  #   View(MoveProb_spwn)
  
  
  ## Avoid printing every tow
  print.seq <- seq(1, ntow, 100)
  
  
  ## Closure?
  if(!is.null(closure )) {
    print("You are implementing spatial closures....")
    closeArea <- TRUE
  }
  
  if(is.null(closure )) {
    print("You are NOT implementing spatial closures....")
    closeArea <- FALSE 
  }
  
  ##################
  ### loop control #
  for (t in seq_len(ntow)) { #for(t in seq(50,ntow,1)){ #THIS DEFINES THE LOOP. NTOW = n_tows_day * n_days_wk_fished * 52 * n_years
    ##################
    print(t)
    ## Loop messages
    
    ## Print when new year
    if(t == 1){
      print(paste("----------year", year.breaks[t], "-----------"))
    }
    
    if(t > 1){
      if(year.breaks[t] != year.breaks[t-1]) {
        print(paste("----------year", year.breaks[t], "-----------")) }
    }
    
    ## Print some tow info
    if(t %in% print.seq) {
      print(paste("tow ==", t, "----",round(t/ntow * 100,0), "%"))
    }
    
    ###################################
    ## Switches for various processes #
    ###################################
    
    ## first tow in a week where any of the stocks recruit
    
    #CHANGED THIS SO REC IS TRUE ON FIRST WEEK OF REC/SPAWN ONLY
    Recruit  <- ifelse(t > 1, ifelse(week.breaks[t] != week.breaks[t-1] &
                                       #IF IN FIRST SPAWN WEEK OR IN LAST SPAWN WEEK IN FIRST YEAR    
                                       (( week.breaks[t] %in% unlist(sapply(pop_init$dem_params, function(x) x[["spwn_wk"]][[1]])))
                                        | ( t %in% unlist(sapply(pop_init$dem_params, function(x) tail(x[["spwn_wk"]],n=1))))),
                                     TRUE, FALSE), FALSE) 
    
    if(t != ntow) {
      Pop_dyn  <- ifelse(day.breaks[t] != day.breaks[t+1], TRUE, FALSE) ## weekly delay diff
      Pop_move <- ifelse(week.breaks[t] != week.breaks[t+1], TRUE, FALSE) ## weekly pop movement
      Update   <- ifelse(day.breaks[t] != day.breaks[t+1], TRUE, FALSE) ## weekly pop records 
      
      ## Closure switch, when to recalculate the closed areas
      
      
      if(!is.null(closure)) {
        if(t==1 & !closeArea) {CalcClosures  <-  FALSE }
        if(t==1 & closeArea & is.null(closure[["input_coords"]])) {CalcClosures <- FALSE}
        if(t==1 & closeArea & !is.null(closure[["input_coords"]]) & closure[["year_start"]] == 1) {CalcClosures <- TRUE}
        if(t==1 & closeArea & !is.null(closure[["input_coords"]]) & closure[["year_start"]] > 1) {CalcClosures <- FALSE}
        
        if( t > 1 & closeArea ) 		{
          if(closure[["temp_dyn"]] == 'yearly') {
            CalcClosures <- ifelse(year.breaks[t] != year.breaks[t-1], TRUE, FALSE)
          }
          if(closure[["temp_dyn"]] == 'monthly') {
            CalcClosures <- ifelse(month.breaks[t] != month.breaks[t-1], TRUE, FALSE)
          }
          if(closure[["temp_dyn"]] == 'weekly') {
            CalcClosures <- ifelse(week.breaks[t] != week.breaks[t-1], TRUE, FALSE)
          }
          
        }
      }
    } #end line 142 BL
    
    #######################
    ##### Recruitment #####
    #######################
    
    # Specified weeks
    # As defined by:
    # init_pop$rec_params$rec_wk
    # Recruitment only occurs in specified spwn.hab
    # Recruitment based on pop in spawning grounds in first week of spawning,
    # but occurs throughout the spawning period
    # Different spawning periods for different pops, so we need to handle this...
    #print("Recruit?")
    #print(Recruit)
    if(Recruit) { # Check for new week
      
    #  print("Recruiting")
      
      ## Check if its a recruitment week for the population
      Rec <- lapply(paste0("spp", seq_len(n_spp)), function(s) {
        
        if(week.breaks[t] %in% pop_init[["dem_params"]][[s]][["rec_wk"]]) {
          
       #   print(t)
        #  print(s)
          #View(B[[s]])
          #print(sum(B[[s]]))
          
          if(year.breaks[t]==1){  #NEED TO ACCOUNT FOR FIRST YEAR
            
            #IN FIRST YEAR BLAST OUT ALL OF IT IN LAST REC WEEK SO THAT 
            
            ifelse( t == tail(pop_init[["dem_params"]][[s]][["rec_wk"]],n=1),
                    
                    #NEED TO ACCOUNT FOR ALPHA IN POP DYNMAICS BY SCALING UP RECRUITMENT FOR 1 WEEK
                    
                    rec <- length(pop_init[["dem_params"]][[s]][["rec_wk"]])*Recr_mat(model = pop_init[["dem_params"]][[s]][["rec_params"]][["model"]],
                                                                                      params = c("a" = as.numeric(pop_init[["dem_params"]][[s]][["rec_params"]][["a"]]),
                                                                                                 "b" = as.numeric(pop_init[["dem_params"]][[s]][["rec_params"]][["b"]])),
                                                                                      B = B[[s]], #THIS IS NEW. USE CURRENT POP IN FIRST YEAR
                                                                                      cv = as.numeric(pop_init[["dem_params"]][[s]][["rec_params"]][["cv"]])),
                    rec <- matrix(0, ncol = ncols, nrow = nrows)
            )
            
          }
          
          if(year.breaks[t]>1){
            
            last_spwn <- tail(pop_init[["dem_params"]][[s]][["rec_wk"]],n=1)
            
            rec <- Recr_mat(model = pop_init[["dem_params"]][[s]][["rec_params"]][["model"]],
                            params = c("a" = as.numeric(pop_init[["dem_params"]][[s]][["rec_params"]][["a"]]),
                                       "b" = as.numeric(pop_init[["dem_params"]][[s]][["rec_params"]][["b"]])),
                            B = pop_bios[[year.breaks[t]-1, week.breaks[last_spwn]]][[s]], #THIS IS NEW. USING PREVIOUS YEARS POPULATION VALUE AT END OF RECRUITMENT TO MAKE PULSE IN SPAWNING GROUND 
                            cv = as.numeric(pop_init[["dem_params"]][[s]][["rec_params"]][["cv"]]))
            
          }
        }
        
        if(!week.breaks[t] %in% pop_init[["dem_params"]][[s]][["rec_wk"]]) {
          
          print("THIS IS AN ERROR. SHOULD NOT HAPPEN. SEE RECRUITMENT SECTION IN RUN_SIM")
          
          stop() #writing this to force a stop
          
          
          
        }
        
        rec
        
      })
      names(Rec) <- paste0("spp", seq_len(n_spp))
      
      
    }
    
    ##########################
    #### Spatial closures ####
    ##########################
    
    ## Calculate where to place the closures
    ## Can't close areas in the first year, unless manually defined
    
    
    if(t > 1 & !is.null(closure)) {
      
      ## Dynamic closures
      if(closeArea & CalcClosures & year.breaks[t] >= closure[["year_start"]] & is.null(closure[["input_coords"]]) & is.null(closure[["year_basis"]])) {
        print("Calculating where to place closures dynamically...")
        print(paste("Based on", closure[["basis"]], "on a", closure[["temp_dyn"]], "basis using", closure[["rationale"]]))
        
        AreaClosures <- close_areas(sim_init = sim_init, closure_init = closure, commercial_logs = catches, survey_logs = survey[["log.mat"]], real_pop = pop_bios, t = t)
        
        # keep a record
        close_count <- close_count + 1
        closure_list[[close_count]] <- AreaClosures
        
      }
      
      ## Closures calculated based on fixed year
      if(closeArea & CalcClosures & year.breaks[t] >= closure[["year_start"]] & is.null(closure[["input_coords"]]) & !is.null(closure[["year_basis"]])) {
        print("Calculating where to place closures based on the input years / months / weeks")
        print(paste("Based on", closure[["basis"]], "on a", closure[["temp_dyn"]], "basis using", closure[["rationale"]]))
        
        AreaClosures <- close_areas(sim_init = sim_init, closure_init = closure, commercial_logs = catches, survey_logs = survey[["log.mat"]], real_pop = pop_bios, t = t)
        # keep a record
        close_count <- close_count + 1
        closure_list[[close_count]] <- AreaClosures
        
      }
      
    }
    
    ## Fixed closures
    if(!is.null(closure) & !is.null(closure[["input_coords"]]) & CalcClosures) {
      if(closeArea & year.breaks[t] >= closure[["year_start"]]) {
        print("Setting manually defined closures")
        print(paste("Closures are", closure[["temp_dyn"]]))
        
        if(closure[["temp_dyn"]] == 'yearly') {
          AreaClosures <- closure[["input_coords"]]
        }
        
        if(closure[["temp_dyn"]] == "monthly") {
          AreaClosures <- closure[["input_coords"]][[month.breaks[[t]]]]
        }
        
        if(closure[["temp_dyn"]] == "weekly") {
          AreaClosures <- closure[["input_coords"]][[week.breaks[[t]]]]
        }
        
      }
    }
    
    #######################
    ###### Fishing ########    HAPPENS EVERY STEP SO NO NEED FOR SWITCH
    #######################
    
    # every t
    # uses go_fish_fleet function, either
    
    ## IF WE FISH EVERY DAY THE CATCH MATRIX WILL EXTEND BEYOND ITS SIZE
    ## INSTEAD, ONLY FISH 1 DAY PER WEEK USING IF STATMENT
    
    if(t%%7 == 1){#fish on first day of week
      
      if(t==1) {
        
        catches <- lapply(seq_len(n_fleets), function(fl) { 
          
          go_fish_fleet(FUN = go_fish, 
                        sim_init = sim_init, 
                        fleets_params = fleets_init[["fleet_params"]][[fl]],
                        fleets_catches =     fleets_init[["fleet_catches"]][[fl]], 
                        sp_fleets_catches =  fleets_init[["sp_fleet_catches"]][[fl]], 
                        closed_areas = AreaClosures, pops = B, t = t)
        })
        #print("catches")
        #print(catches)
      } # end t==1 run
      
      if(t > 1) {
        
        # if its the same day 
        if(day.breaks[t] == day.breaks[t-1]) {
          catches <- lapply(seq_len(n_fleets), function(fl) {  
            
            go_fish_fleet(FUN = go_fish, 	
                          sim_init = sim_init, 
                          fleets_params = fleets_init[["fleet_params"]][[fl]],
                          fleets_catches =     catches[[fl]][["fleets_catches"]], 
                          sp_fleets_catches =  catches[[fl]][["sp_fleets_catches"]],
                          pops = B, t = t, closed_areas = AreaClosures)
          })
          
        } # end same day run
        
        # if its a new day - reset the spatial catches counter
        if(day.breaks[t] != day.breaks[t-1]) {
          
          catches <- lapply(seq_len(n_fleets), function(fl) { 
            
            go_fish_fleet(FUN = go_fish, 
                          sim_init = sim_init, 
                          fleets_params = fleets_init[["fleet_params"]][[fl]],
                          fleets_catches =     catches[[fl]][["fleets_catches"]], 
                          sp_fleets_catches =  fleets_init[["sp_fleet_catches"]][[fl]], ## These are empty
                          pops = B, t = t, closed_areas = AreaClosures)
            
          })
          
        } # end new day run
        
      } # end if t>1
    } #end if first day of week
    
    #######################
    ##### Pop dynamics ####
    #######################
    
    # every day (end day)
    # calc the spatial Fs, find_spat_f_pops()
    # Apply the delay_diff()
    # Need B-1 and B to calc B+1
    
    if(Pop_dyn) { #POP_DYN IS SET ON LINE 143
      
      #  print("Delay-difference model")
      
      ## Calculate the fishing mortalities
      # 1. Sum all fleet catches - DONE
      # 2. find spatial Fs - DONE
      # 3. reset the catch matrices - DONE (above)
      
      # Sum the catches of each population over the fleets and vessels
      #spp_catches <- sum_fleets_catches(FUN = sum_fleet_catches, fleets_log =
      #				  catches, sim_init = sim_init)
      spp_catches <- sum_fleets_catches(sim_init = sim_init, fleets_log =
                                          catches)
      
      ## Fs for all populations
      #spat_fs <- find_spat_f_pops(sim_init = sim_init, C = spp_catches, B = B, 
      #                            dem_params = pop_init[["dem_params"]])
      
      #No fishing so no spatial F. Instead make a matrix of 0s to use elsewhere
      spat_fs <- lapply(paste0("spp", seq_len(n_spp)), function(x) { matrix(data = 0, nrow = nrows, ncol = ncols)  })
      names(spat_fs)  <- paste("spp", seq_len(n_spp), sep = "")
      
      #print("spat_fs")
      #print(spat_fs)
      
      
      #spat_fs <- lapply(names(B), function(x) {
      #			  find_spat_f_naive(C = spp_catches[[x]], B = B[[x]])
      #				  })
      #names(spat_fs)  <- names(B)
      
      ## Fishing mortality rates
      #print(sapply(names(spat_fs), function(x) weighted.mean(spat_fs[[x]], B[[x]])))
      
      # Apply the delay difference model
      Bp1 <- lapply(paste0("spp", seq_len(n_spp)), function(x) {
        #print(t)
      #  print("sum of Rec is")
       # print(sum(Rec[[x]]))
        #View(B[[x]])
        
        al   <- ifelse(week.breaks[t] %in% pop_init[["dem_params"]][[x]][["rec_wk"]],
                       1/length(pop_init[["dem_params"]][[x]][["rec_wk"]]), 0)
        alm1 <- ifelse(c(week.breaks[t]-1) %in% pop_init[["dem_params"]][[x]][["rec_wk"]],
                       1/length(pop_init[["dem_params"]][[x]][["rec_wk"]]), 0)
       # print("al and alm1 are")
       # print(al)
       # print(alm1)
        
        res <- delay_diff(K = pop_init[["dem_params"]][[x]][["K"]], F = spat_fs[[x]], 
                          M = pop_init[["dem_params"]][[x]][["M"]]/365, 
                          wt = 1, wtm1 = 0.1, R = Rec[[x]], B = B[[x]],
                          Bm1 = Bm1[[x]], al = al,  alm1 = alm1)
        
      })
      names(Bp1) <- paste0("spp", seq_len(n_spp))
      
      Bm1 <- B  #record at location
      B <- Bp1
      
      ####################
      ## scientific survey
      ####################
      
      # #SKIPPING FOR NOW AND WILL SAMPLE AFTERWARDS
      # 
      # if(sim_init[["brk.idx"]][["day.breaks"]][t] %in% survey[["log.mat"]][,"day"] & !is.null(survey)) {
      # 
      # ##print("undertaking scientific survey")
      # 
      # # doy and y
      #   doy <- sim_init[["brk.idx"]][["day.breaks"]][t]
      #   y   <- sim_init[["brk.idx"]][["year.breaks"]][t]
      # 
      # 	# survey log
      # 	log.mat <- survey[["log.mat"]]
      # 
      # 	# survey locations
      # 	x_loc <- log.mat[log.mat[,"day"] == doy & log.mat[,"year"] == y, "x"]
      # 	y_loc <- log.mat[log.mat[,"day"] == doy & log.mat[,"year"] == y, "y"]
      # 
      # 	# For each set of locations
      # 	for(i in seq_len(length(x_loc))) {
      # 		
      # # For each species
      # for(s in seq_len(n_spp)) {
      # log.mat[log.mat[,"day"]==doy & log.mat[,"year"]==y,paste0("spp",s)][i]  <-  	B[[s]][x_loc[i],y_loc[i]] * as.numeric(survey[["survey_settings"]][[paste0("Qs.spp",s)]])
      # }
      # 	}
      # 
      # 	# return log.mat to the list
      # 	survey[["log.mat"]] <- log.mat
      # 
      # }
      
      
    } # end if statement
    
    #######################
    ##### Pop movement ####
    #######################
    # occurs every week, 
    # uses move_population: inputs, move_Prop (movement probabilities) and Start_Pop (current population)
    # uses hab or hab_spwn to calc the move probabilities, move_prob_Lst()
    # while in spwn_wk, use hab.spwn for the move_prob_Lst(), else use hab 
    
    
    if(Pop_move) {
      ##	print("Moving populations")
      
      ### With covariates ###
      
      ## If we've spatiotemporal movement covariate, include here:
      if(!is.null(move_cov)) {
        
        ## The temperature covariates for the week
        move_cov_wk <- move_cov[["cov.matrix"]][[week.breaks.all[t]]]  #this should now use all moveCov matrices
        
        
        B <- lapply(paste0("spp", seq_len(n_spp)), function(s) {
          
          move_cov_wk_spp <- matrix(nc = sim_init[["idx"]][["ncols"]],
                                    nr = sim_init[["idx"]][["nrows"]],
                                    sapply(move_cov_wk, norm_fun, 
                                           mu = move_cov[["spp_tol"]][[s]][["mu"]], 
                                           va = move_cov[["spp_tol"]][[s]][["va"]]))
          
          ## If in a non-spawning week or spawning week
          if(!week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
          
            #	  print("non-spawning movement")
            #IDEA TO SPEED UP USING C++
            # newPop <- move_population(moveProp = lapply(lapply(MoveProb[[s]], function(x) x * move_cov_wk_spp), function(x1) x1/sum(x1)),
            #                           StartPop = Bp1[[s]],
            #                           Nzero_row = as.vector(nonzero_idx[[s]][,1]),
            #                           Nzero_col = as.vector(nonzero_idx[[s]][,2])) 
            newPop <- move_population(moveProp = lapply(lapply(MoveProb[[s]], function(x) x * move_cov_wk_spp), function(x1) x1/sum(x1)),
                                      StartPop = Bp1[[s]]) 
          }
          
          if(week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
            #	  print("spawning movement")
            newPop <- move_population(moveProp = lapply(lapply(MoveProb_spwn[[s]], function(x) x * move_cov_wk_spp), function(x1) x1/sum(x1)),
                                      StartPop = Bp1[[s]])
          }
          
          Reduce("+", newPop)
          
        }) #end lapply()
        
        
        ## Also need to move the previous month biomass, so the f calcs match
        ## as an input to the delay diff
        Bm1 <- lapply(paste0("spp", seq_len(n_spp)), function(s) {
          
          move_cov_wk_spp <- matrix(nc = sim_init[["idx"]][["ncols"]],
                                    nr = sim_init[["idx"]][["nrows"]],
                                    sapply(move_cov_wk, norm_fun, 
                                           mu = move_cov[["spp_tol"]][[s]][["mu"]], 
                                           va = move_cov[["spp_tol"]][[s]][["va"]]))
          
          ## If in a non-spawning week or spawning week
          if(!week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
            newPop <- move_population(lapply(lapply(MoveProb[[s]], function(x) x * move_cov_wk_spp), function(x1) x1/sum(x1)),
                                      StartPop = Bm1[[s]])
          }
          
          if(week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
            newPop <- move_population(lapply(lapply(MoveProb_spwn[[s]], function(x) x * move_cov_wk_spp), function(x1) x1/sum(x1)),
                                      StartPop = Bm1[[s]])
          }
          
          Reduce("+", newPop)
          
        })
        
      }  #end  if(!is.null(move_cov))
      
      
      ### No covariates ###
      
      if(is.null(move_cov)) {
        
        B <- lapply(paste0("spp", seq_len(n_spp)), function(s) {
          
          ## If in a non-spawning week or spawning week
          if(!week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
            newPop <- move_population(moveProp = MoveProb[[s]], StartPop = Bp1[[s]]) 
          }
          
          if(week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
            newPop <- move_population(moveProp = MoveProb_spwn[[s]], StartPop = Bp1[[s]])
          }
          
          Reduce("+", newPop)
          
        })
        
        
        ## Also need to move the previous month biomass, so the f calcs match
        ## as an input to the delay diff
        Bm1 <- lapply(paste0("spp", seq_len(n_spp)), function(s) {
          
          ## If in a non-spawning week or spawning week
          if(!week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
            newPop <- move_population(moveProp = MoveProb[[s]], StartPop = Bm1[[s]])
          }
          
          if(week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
            newPop <- move_population(moveProp = MoveProb_spwn[[s]], StartPop = Bm1[[s]])
          }
          
          Reduce("+", newPop)
          
        })
        
      }
      
      names(B) <- paste0("spp", seq_len(n_spp))
      names(Bm1) <- paste0("spp", seq_len(n_spp))
      
      ## If we're saving population biomass, do so here
      if(save_pop_bio == TRUE) {
        if(!"pop_bios" %in% ls()) {
          # Create a list structure for storing pop bios
          pop_bios <- vector("list", sim_init[["idx"]][["ny"]] * sim_init[["idx"]][["nw"]]) 
          dim(pop_bios) <- c(sim_init[["idx"]][["ny"]] , sim_init[["idx"]][["nw"]])
          pop_bios[[year.breaks[t], week.breaks[t]]] <- B
        }
        
        if("pop_bios" %in% ls()) {
          pop_bios[[year.breaks[t], week.breaks[t]]] <- B
        }
        
      }
      
      if(!"pop_bios" %in% ls() & save_pop_bio == FALSE) {
        pop_bios <- NULL
      }
      
      
      
    } # end if(Pop_Move) statement
    
    
    
    ######################
    ##### Update #########
    ######################
    
    # Update weekly / annual records at pop level
    
    if(Update) {
      ##print("Recording metrics")
      
      for(s in paste0("spp", seq_len(n_spp))) {
        
        
        pop_init[["Pop_record"]][[s]][["F.mat"]][year.breaks[t], day.breaks[t]] <- weighted.mean(spat_fs[[s]], B[[s]])
        
        pop_init[["Pop_record"]][[s]][["Catch.mat"]][year.breaks[t], day.breaks[t]] <- sum(spp_catches[[s]])
        
        pop_init[["Pop_record"]][[s]][["Bio.mat"]][year.breaks[t], day.breaks[t]] <- sum(Bp1[[s]])
        
        pop_init[["Pop_record"]][[s]][["Rec.mat"]][1, year.breaks[t]] <- sum(Rec[[s]], pop_init[["Pop_record"]][[s]][["Rec.mat"]][1, year.breaks[t]], na.rm = T)
        
        
      }
      
      
    }
    
  } # end loop control on line 112
  
  
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste("time taken is :", format(time.taken, units = "auto"), sep = " "))
  
  return(list(fleets_catches = catches, pop_summary = pop_init[["Pop_record"]], pop_bios = t(pop_bios), survey = survey, closures = closure_list))
  
} # end func

