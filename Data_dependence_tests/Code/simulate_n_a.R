
library(foreach)
library(doParallel)
no_cores <- 16
registerDoParallel(makeCluster(no_cores))
library(truncnorm)

file_names <- c("baseline_beta_right","scen1_beta_right","scen2_0.1_beta_right","scen2_0.5_beta_right","scen2_0.9_beta_right")
#"baseline_unif","baseline_beta_left","scen1_unif","scen1_beta_left",,"scen2_0.1_unif","scen2_0.1_beta_left","scen2_0.1_beta_right",
#                "scen2_0.5_unif","scen2_0.5_beta_left","scen2_0.5_beta_right","scen2_0.9_unif","scen2_0.9_beta_left","scen2_0.9_beta_right"

results <- NULL
for (a_file in file_names) {
  result_path <- paste("G:/My Drive/trip bias paper/scenarios/zone_homogenity/",a_file,".csv",sep="")
  para_info <- read.csv(result_path)
  para_info$se_data[which(is.na(para_info$se_data))] <- 0
  
  data_path <- paste("G:/My Drive/trip bias paper/scenarios/",a_file,".csv",sep="")
  data_info <- read.csv(data_path)
  
  ###completely random simulations
  # par_1_all <- rnorm(10000, para_info$para_data[1],para_std[1])
  # par_2_all <- rnorm(10000, para_info$para_data[2],para_std[2])
  # par_3_all <- rtruncnorm(10000, 0, Inf, para_info$para_data[3],para_std[3])
  # par_4_all <- rtruncnorm(10000, 0, Inf, para_info$para_data[4],para_std[4])
  # 
  # n_as <- foreach(i=1:10000, .combine="rbind", .packages="truncnorm") %dopar% {
  #   par_1 <- par_1_all[i]
  #   par_2 <- par_2_all[i]
  #   par_3 <- par_3_all[i]
  #   par_4 <- par_4_all[i]
  # 
  #   set.seed(1)
  #   noise_1 <- rnorm(obs_all_count,0,par_3)
  #   noise_2 <- rnorm(obs_all_count,0,par_4)
  # 
  #   utility_1 <- par_1 + noise_1
  #   utility_2 <- par_2 + noise_2
  # 
  #   p_i_a_1 <- 1 - 1/(1+exp(utility_1))
  #   p_i_a_2 <- 1 - 1/(1+exp(utility_2))
  #   #https://stackoverflow.com/questions/63495476/r-getting-very-small-number-instead-of-zero (searching "R small number zero")
  #   p_i_a_1[which(p_i_a_1 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_2[which(p_i_a_2 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_1[which(p_i_a_1 < 0.0000000001)] <- 0.0000000001
  #   p_i_a_2[which(p_i_a_2 < 0.0000000001)] <- 0.0000000001
  #   p_a_1 <- mean(p_i_a_1)
  #   p_a_2 <- mean(p_i_a_2)
  # 
  #   n_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)
  #   
  #   obs_0_count <- sapply(n_a - obs_all_count, function(x) max(x,0))
  #   
  #   likelihood_0 <- obs_0_count*log((1 - p_a_1)*(1 - p_a_2))
  #   likelihood_1 <- log(p_i_a_1[1:obs_1_count]*(1 - p_i_a_2[1:obs_1_count]))
  #   if (obs_1_count == 0) likelihood_1 <- 0
  #   likelihood_2 <- log((1 - p_i_a_1[(obs_1_count+1):(obs_1_count+obs_2_count)])*p_i_a_2[(obs_1_count+1):(obs_1_count+obs_2_count)])
  #   if (obs_2_count == 0) likelihood_2 <- 0
  #   likelihood_12 <- log(p_i_a_1[(obs_1_count+obs_2_count+1):obs_all_count]*p_i_a_2[(obs_1_count+obs_2_count+1):obs_all_count])
  #   if (obs_12_count == 0) likelihood_12 <- 0
  #   log_likelihood <- likelihood_0+sum(likelihood_1)+sum(likelihood_2)+sum(likelihood_12)
  # 
  #   c(n_a,log_likelihood)
  # }
  
  ###fix alpha_1 and alpha_2 and randomize error_std--------------------------------------------------------------------------------------------------------------------
  # par_1_upper <- para_info$para_data[1]+para_info$se_data[1]*1.96
  # par_1_lower <- para_info$para_data[1]-para_info$se_data[1]*1.96
  # par_2_upper <- para_info$para_data[2]+para_info$se_data[2]*1.96
  # par_2_lower <- para_info$para_data[2]-para_info$se_data[2]*1.96
  # par_3_all <- rtruncnorm(10000,0,Inf,para_info$para_data[3],para_std[3])
  # par_4_all <- rtruncnorm(10000,0,Inf,para_info$para_data[4],para_std[4])
  # 
  # n_as_upper <- foreach(i=1:10000, .combine="c", .packages="truncnorm") %dopar% {
  #   par_1 <- par_1_upper
  #   par_2 <- par_2_upper
  #   par_3 <- par_3_all[i]
  #   par_4 <- par_4_all[i]
  # 
  #   set.seed(6)
  #   noise_1 <- rnorm(obs_all_count,0,par_3)
  #   noise_2 <- rnorm(obs_all_count,0,par_4)
  # 
  #   utility_1 <- par_1 + noise_1
  #   utility_2 <- par_2 + noise_2
  # 
  #   p_i_a_1 <- 1 - 1/(1+exp(utility_1))
  #   p_i_a_2 <- 1 - 1/(1+exp(utility_2))
  #   #https://stackoverflow.com/questions/63495476/r-getting-very-small-number-instead-of-zero (searching "R small number zero")
  #   p_i_a_1[which(p_i_a_1 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_2[which(p_i_a_2 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_1[which(p_i_a_1 < 0.0000000001)] <- 0.0000000001
  #   p_i_a_2[which(p_i_a_2 < 0.0000000001)] <- 0.0000000001
  #   p_a_1 <- mean(p_i_a_1)
  #   p_a_2 <- mean(p_i_a_2)
  # 
  #   n_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)
  # 
  #   n_a
  # }
  # 
  # n_as_lower <- foreach(i=1:10000, .combine="c", .packages="truncnorm") %dopar% {
  #   par_1 <- par_1_lower
  #   par_2 <- par_2_lower
  #   par_3 <- par_3_all[i]
  #   par_4 <- par_4_all[i]
  # 
  #   set.seed(6)
  #   noise_1 <- rnorm(obs_all_count,0,par_3)
  #   noise_2 <- rnorm(obs_all_count,0,par_4)
  # 
  #   utility_1 <- par_1 + noise_1
  #   utility_2 <- par_2 + noise_2
  # 
  #   p_i_a_1 <- 1 - 1/(1+exp(utility_1))
  #   p_i_a_2 <- 1 - 1/(1+exp(utility_2))
  #   #https://stackoverflow.com/questions/63495476/r-getting-very-small-number-instead-of-zero (searching "R small number zero")
  #   p_i_a_1[which(p_i_a_1 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_2[which(p_i_a_2 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_1[which(p_i_a_1 < 0.0000000001)] <- 0.0000000001
  #   p_i_a_2[which(p_i_a_2 < 0.0000000001)] <- 0.0000000001
  #   p_a_1 <- mean(p_i_a_1)
  #   p_a_2 <- mean(p_i_a_2)
  # 
  #   n_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)
  # 
  #   n_a
  # }
  # 
  # n_as_exp <- foreach(i=1:10000, .combine="c", .packages="truncnorm") %dopar% {
  #   par_1 <- para_info$para_data[1]
  #   par_2 <- para_info$para_data[2]
  #   par_3 <- par_3_all[i]
  #   par_4 <- par_4_all[i]
  # 
  #   set.seed(6)
  #   noise_1 <- rnorm(obs_all_count,0,par_3)
  #   noise_2 <- rnorm(obs_all_count,0,par_4)
  # 
  #   utility_1 <- par_1 + noise_1
  #   utility_2 <- par_2 + noise_2
  # 
  #   p_i_a_1 <- 1 - 1/(1+exp(utility_1))
  #   p_i_a_2 <- 1 - 1/(1+exp(utility_2))
  #   #https://stackoverflow.com/questions/63495476/r-getting-very-small-number-instead-of-zero (searching "R small number zero")
  #   p_i_a_1[which(p_i_a_1 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_2[which(p_i_a_2 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_1[which(p_i_a_1 < 0.0000000001)] <- 0.0000000001
  #   p_i_a_2[which(p_i_a_2 < 0.0000000001)] <- 0.0000000001
  #   p_a_1 <- mean(p_i_a_1)
  #   p_a_2 <- mean(p_i_a_2)
  # 
  #   n_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)
  # 
  #   n_a
  # }
  
  ### using pop_density
  pop_densities_unique <- unique(data_info$popu_density)
  obs_zones_counts <- sapply(pop_densities_unique, function(x) length(which((data_info$capture_1==1 | data_info$capture_2==1) & data_info$popu_density==x)))
  
  para_interval <- matrix(NA,10,3)
  para_interval[,1] <- para_info$para_data[1:10] - 1.96*para_info$se_data[1:10]
  para_interval[,2] <- para_info$para_data[1:10]
  para_interval[,3] <- para_info$para_data[1:10] + 1.96*para_info$se_data[1:10]
  
  est <- c()
  for (i in 1:3) {
    para_set <- para_interval[,i]
    
    par_1_all <- para_set[1:5] #rnorm(10000, para_info$para_data[1],para_std[1])
    #par_2_all <- para_info$para_data[2] #rnorm(10000, para_info$para_data[2],para_std[2])
    par_3_all <- para_set[6:10] #rtruncnorm(10000, 0, Inf, para_info$para_data[3],para_std[3])
    
    utility_zones_1 <- par_1_all #+ par_2_all*pop_densities_unique
    utility_zones_2 <- par_3_all#*pop_densities_unique
    p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
    p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
    # p_zones_1[which(p_zones_1 > 0.9999999999)] <- 0.9999999999
    # p_zones_2[which(p_zones_2 > 0.9999999999)] <- 0.9999999999
    # p_zones_1[which(p_zones_1 < 0.0000000001)] <- 0.0000000001
    # p_zones_2[which(p_zones_2 < 0.0000000001)] <- 0.0000000001
    
    n_a_zones <- obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)
    est <- c(est,sum(n_a_zones))
  }
  
  # weights <- exp(n_as[,2]-max(n_as[,2]))
  # weights <- weights/sum(weights)
  # sampled_n_as <- sample(n_as[,1],10000,T,weights)
  # results <- cbind(results,sampled_n_as)
  
  #results <- cbind(results,c(mean(n_as_upper),mean(n_as_lower),mean(n_as_exp)))
  
  results <- cbind(results,est)
}

colnames(results) <- file_names
write.csv(results,"G:/My Drive/trip bias paper/scenarios/zone_homogenity/n_a_interval.csv",row.names=F)

stopImplicitCluster()


###data analysis-----------------------------------------------------------------------------------------------------------------------------
file_names <- c("baseline_unif","baseline_beta_left","baseline_beta_right","scen1_unif","scen1_beta_left","scen1_beta_right","scen2_0.1_unif","scen2_0.1_beta_left","scen2_0.1_beta_right",
                "scen2_0.5_unif","scen2_0.5_beta_left","scen2_0.5_beta_right","scen2_0.9_unif","scen2_0.9_beta_left","scen2_0.9_beta_right")

obs_counts <- NULL
for (a_file in file_names) {
  
  data_path <- paste("G:/My Drive/trip bias paper/scenarios/",a_file,".csv",sep="")
  data_info <- read.csv(data_path)
  
  obs_1 <- which(data_info$capture_1==1 & data_info$capture_2==0)
  obs_2 <- which(data_info$capture_1==0 & data_info$capture_2==1)
  obs_12 <- which(data_info$capture_1==1 & data_info$capture_2==1)
  all_obs <- c(obs_1,obs_2,obs_12)
  obs_all_count <- length(all_obs)
  
  obs_counts <- c(obs_counts, obs_all_count)
}

results_modified <- NULL
for (i in 1:15) {
  results_modified <- cbind(results_modified,results[,i][which(results[,i] < quantile(results[,i],0.95) & results[,i] > quantile(results[,i],0.05))])
}

colMeans(results)
apply(results,2,median)
confidence_interval <- sapply(1:15,function(x) 1.96*sd(results[,x])/sqrt(obs_counts[x]))
interval_95 <- apply(results,2,function(x) quantile(x,c(0.05,0.5,0.95)))

