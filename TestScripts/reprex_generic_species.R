


library(MixFishSim)

source("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/R/init_sim_Bens_nofish.R")

sim <- init_sim_Bens_nofish(nrows = 100, ncols = 100, n_years = 1, n_tows_day = 1,
                            n_days_wk_fished = 1, n_fleets = 1, n_vessels = 1, n_species = 2,
                            move_freq = 1)



hab <- readRDS(file="C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/Results/hab_16strata")



#rebuild package
devtools::build()


#load rcpp exports
Rcpp::sourceCpp(file= "src/Movement.cpp")
Rcpp::sourceCpp(file= "src/RcppExports.cpp")
Rcpp::compileAttributes() #this updates RcppExports.R file, which contains function definitions

#source(file= "R/RcppExports.R")


# 
# 
# #read in new move_population file
# 
# Rcpp::sourceCpp("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/src/Movement.cpp") #my edited version 
# 
# source("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/R/RcppExports.R")
# #to source a new move_population where I edited to skips most things:
# #1: load file
# 
# #2: allow the function to call other hidden functions from mixfishsim 
# environment(move_population_Bens) <- asNamespace('MixFishSim')
# #3: replace move_population with move_population_Bens in the MixFishSim package
# assignInNamespace("move_population", move_population, ns = "MixFishSim")






#CALULATE INDICES OF NONZERO VALUES IN HAB TO PASS TO MOVE_POPULAITON DURING MOVEMENT
nonzero_idx <- lapply(paste0("spp", seq_len(sim$idx[["n.spp"]])), function(s) {
  
  which(hab[["hab"]][[s]] !=0 , arr.ind=T)
  
})

names(nonzero_idx) <- paste("spp",seq_len(sim$idx[["n.spp"]]), sep ="")



source("R/init_pop_Bens.R")



#decreasing population settings
Pop <- init_pop_Bens(sim_init = sim, Bio = c("spp1" = 4e5, "spp2" = 10e5), #these values from paper : 1e5 and 2e5
                     hab = hab[["hab"]], start_cell = c(50,50),
                     lambda = c("spp1" = 0.1, "spp2" = 0.1), #same lambda for all?
                     init_move_steps = 20,
                     rec_params = list("spp1" = c("model" = "BH", "a" = 2, "b" = 4, "cv" = 0),
                                       "spp2" = c("model" = "BH", "a" = 7, "b" = 4,"cv" = 0)), #these values from paper
                     rec_wk = list("spp1" = 15:18, "spp2" = 15:18),
                     spwn_wk = list("spp1" = 15:18, "spp2" = 15:18),
                     M = c("spp1" = 0.275, "spp2" = 0.225), 
                     K = c("spp1" = 0.3, "spp2" = 0.3),
                      nz = nonzero_idx) #all the same for now


#check if move_population inside init_pop worked
fields::image.plot(Pop$Start_pop[[1]])
fields::image.plot(Pop$Start_pop[[2]])

View(Pop$Start_pop[[1]])
View(Pop$Start_pop[[2]])

load("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/20 year moveCov matrices/Final/Final Bens Method/Constant_temp_22yr.RData")

moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- list("spp1" = list("mu" = 7.98, "va" = 3),  #8.13 IF TEMP INCREASES   7.98 if temp constant
                             "spp2" = list("mu" = 7.98, "va" = 3) )


fleets <- MixFishSim::init_fleet(sim_init = sim, VPT = list("spp1" = 0, "spp2" = 0), #VPT = value per ton
                                 Qs = list("fleet 1" = c("spp1" = 0, "spp2" = 0)   #Q = catchability
                                 ),
                                 fuelC = list("fleet1" = 3),
                                 step_params = list("fleet 1" = c("rate" = 3, "B1" = 1, "B2" = 2, "B3" = 3)
                                 ),				
                                 past_knowledge = FALSE,  #dont use past knowledge
                                 past_year_month = TRUE,
                                 past_trip = TRUE,
                                 threshold = 0.7
)


source("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/R/run_sim.R")


#to source a new go_fish where I edited to skips most things:
#1: load file
source("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/R/go_fish_Bens.R") #my edited version that skips most things
#2: allow the function to call other hidden functions from mixfishsim 
environment(go_fish_Bens) <- asNamespace('MixFishSim')
#3: replace go_fish with go_fish_Bens in the MixFishSim package
assignInNamespace("go_fish", go_fish_Bens, ns = "MixFishSim")







res<- run_sim(sim_init = sim,
              pop_init = Pop,
              move_cov = moveCov,
              fleets_init = fleets,
              hab_init = hab,
              save_pop_bio = TRUE,
              survey = NULL, 
              closure = NULL)








