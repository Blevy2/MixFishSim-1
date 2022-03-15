#' @title Plot population summary

#' @description \code{plot_pop_summary} plots the four population dynamic
#' metrics: catches, biomass, fishing mortality and recruitment.

#' It can either operate at a daily timestep or an annual timestep

#' @param results is an output from the function \link{run_sim}.
#' @param timestep is a character string determining whether the plot is
#' 'daily' or 'annual'
#' @param save is a logical whether to save the plot
#' @param save.location is a location (defaults to current directory)

#' @return is a ggplot of all the species and metrics as a faceted plot

#' examples
#' plot_pop_summary(results = res, timestep = 'daily', save = TRUE, location = '.') 
#' ## Not run

#' @export

plot_pop_summary <- function(results = res, timestep = 'daily', save = FALSE, save.location = '.') {
  
  n_spp <- length(results[["pop_summary"]]) 
  res_df <- lapply(seq_len(n_spp), function(x) {
    res_spp <- lapply(names(results[["pop_summary"]][[x]]), function(x1) {
      x1_res <- tidyr::gather(as.data.frame(t(results[["pop_summary"]][[x]][[x1]])), key = "year", factor_key = T)
      if(x1 == "Bio.mat" | x1 == "Bio.mat.sd") {	res_out <- data.frame("pop" = rep(paste("spp",x, sep = "_"), length.out = nrow(x1_res)), 
                                                                       "metric" = x1, 
                                                                       "year" = x1_res$year, 
                                                                       "day" = rep(1:358, length.out = nrow(x1_res)),#changed 362 to 358
                                                                       "julien_day" = seq_len(nrow(x1_res)),
                                                                       "data" = x1_res$value) 
      
      return(res_out)
      
      }
      if(x1 == "Rec.mat" | x1 == "Rec.mat.sd") { res_out <- data.frame("pop" = rep(paste("spp",x, sep = "_"), length.out = nrow(x1_res)), 
                                                                       "metric" = x1, 
                                                                       "year" = seq_len(nrow(x1_res)), 
                                                                       "day" = rep(1, length.out = nrow(x1_res)),
                                                                       "julien_day" = rep(1, length.out = nrow(x1_res)),
                                                                       "data" = x1_res$value) 
      
      return(res_out)
      
      }
      
    })
    return(do.call(rbind, res_spp))
  })
  results_df <- do.call(rbind, res_df)
  
  View(results_df)
  
  if(timestep == "daily") {
    require(ggplot2)
    
    results_df <- na.omit(results_df) #removing rows that contain NA
    View(results_df)
    
    print(ggplot(results_df, aes(x = julien_day, y = data, group = 2)) + geom_point() + facet_grid(pop ~ metric, scale = "free"))
    
    plot(seq(1:200),results_df[["data"]][1:200])
    plot(seq(10:30),results_df[["data"]][10:30])
    
    plot(seq(62:82),results_df[["data"]][62:82])
    
  }
  
  if(timestep == "annual") {
    require(ggplot2); require(dplyr)
    
    results_df_an1 <- results_df %>% filter(metric == "Bio.mat", day == 1) %>% 
      group_by(pop, metric, year) 
    results_df_an2 <- results_df %>% filter(metric != "Bio.mat") %>% 
      group_by(pop, metric, year) 
    
    results_df_annual <- rbind(results_df_an1, results_df_an2) 
    
    #print all
    print(ggplot(results_df_annual, aes(x = year, y = data, group = 2)) + geom_point() + geom_line() + 
            facet_grid(pop ~ metric, scale = "free") + expand_limits(y = 0))
    
    #print 3 on same scale
    print(ggplot(results_df_an2, aes(x = year, y = data, group = 2)) + geom_point() + geom_line() + 
            facet_grid(pop ~ metric, scale = "free") + expand_limits(y = 0))
    
    
    #plot just population
    print(ggplot(annual_pop_results, aes(x = year, y = data, group = 2)) + geom_point() + geom_line() + 
            facet_wrap(~pop, scales = "free_y") + expand_limits(y = 0))
    
    
    
  }
  
  if(save == TRUE) {
    ggsave(file = file.path(paste(save.location, '/Population_Summary', timestep, '.png', sep = "")))
  }
  
  
  #plotting recruitment by itself
  
  plot(res$pop_summary$spp1$Rec.mat[3:22])
  plot(res$pop_summary$spp2$Rec.mat[3:22])
  
  
  
  
  
  #plotting recruitment  SD by itself
  
  plot(res$pop_summary$spp1$Rec.mat.sd[3:22])
  plot(res$pop_summary$spp2$Rec.mat.sd[3:22])
  
  
  
  #plotting weekly  SD of population values
  
  #remove NA
  NO_NA <- vector()
  NO_NA <- res$pop_summary$spp1$Bio.mat.sd[!is.na(res$pop_summary$spp1$Bio.mat.sd)]
  
  plot(seq(length(NO_NA)),NO_NA)
  plot(res$pop_summary$spp2$Rec.mat[3:22])
  n
}

