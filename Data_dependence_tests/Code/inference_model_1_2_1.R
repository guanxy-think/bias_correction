###similar as inference_model_1_2.R in "zone_homogenity" but use population density as independent variable
## Pay attention to the converging criteria factr=1e7
#######Abandoned
library(foreach)
library(doParallel)
no_cores <- 16
registerDoParallel(makeCluster(no_cores))
library(BB)
library(numDeriv)    #calculate gradient

##Generate population density
#average population density in US: 283 per mile^2 (http://css.umich.edu/factsheets/us-cities-factsheet)

file_names <- c("scenario_low_low","scenario_low_mid","scenario_low_high","scenario_mid_low","scenario_mid_mid","scenario_mid_high","scenario_high_low")
                #"scenario_0.1_0.1","baseline_unif","baseline_beta_left","baseline_beta_right","scen1_unif","scen1_beta_left","scen1_beta_right","scen2_0.1_unif","scen2_0.1_beta_left",
                #"scen2_0.5_unif","scen2_0.5_beta_left","scen2_0.9_unif","scen2_0.9_beta_left",)

#a_file <- file_names[1]
for (a_file in file_names) {
  
  data_baseline <- read.csv(paste('G:/My Drive/trip bias paper/scenarios/joint_prob/',a_file,'.csv',sep=""))
  
  pop_densities_unique <- unique(data_baseline$popu_density)
  obs_zones_counts <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==1 | data_baseline$capture_2==1) & data_baseline$popu_density==x)))
  
  obs_1_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==1 & data_baseline$capture_2==0) & data_baseline$popu_density==x)))
  obs_2_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==0 & data_baseline$capture_2==1) & data_baseline$popu_density==x)))
  obs_12_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==1 & data_baseline$capture_2==1) & data_baseline$popu_density==x)))
  
  obs_1_zones_count <- round(obs_1_zones_count)
  obs_2_zones_count <- round(obs_2_zones_count)
  obs_12_zones_count <- round(obs_12_zones_count)
  
  likelihood_function <- function(par) {
    set.seed(1)
    
    utility_zones_1 <- par[1] + par[2]*pop_densities_unique
    utility_zones_2 <- par[3] + par[4]*pop_densities_unique
    p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
    p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
    inf_count <- length(which(p_zones_1 > 1-1e-10)) + length(which(p_zones_2 > 1-1e-10)) + length(which(p_zones_1 < 1e-10)) + length(which(p_zones_2 < 1e-10))
    p_zones_1[which(p_zones_1 > 1-1e-10)] <- 1-1e-10
    p_zones_2[which(p_zones_2 > 1-1e-10)] <- 1-1e-10
    p_zones_1[which(p_zones_1 < 1e-10)] <- 1e-10
    p_zones_2[which(p_zones_2 < 1e-10)] <- 1e-10
    
    # lambda_a <- obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)#rpois(1,lambda_a)
    # poisson_numbers <- t(sapply(1:2000, function(x) rpois(5,lambda_a)))
    poisson_upper_bound <- qpois(0.999,par[5])
    
    cat(par,file = "G:/My Drive/trip bias paper/scenarios/results/logfile_server_1.txt", append=T,fill=T)
    cat(inf_count,file = "G:/My Drive/trip bias paper/scenarios/results/logfile_server_1.txt", append=T,fill=T)
    all_likelihoods <- c()
    for (i in 1:5) {
      likelihoods <- foreach(j = obs_zones_counts[i]:poisson_upper_bound, .combine="c", .export=c("obs_zones_counts","obs_1_zones_count","obs_2_zones_count","obs_12_zones_count")) %dopar% {
        
        obs_0_zones_count <- j - obs_zones_counts[i]
        n_a_zones <- j
        
        # likelihood_0 <- obs_0_zones_count*log((1 - p_zones_1)*(1 - p_zones_2))
        # likelihood_1 <- obs_1_zones_count*log(p_zones_1*(1 - p_zones_2))
        # likelihood_2 <- obs_2_zones_count*log((1-p_zones_1)*p_zones_2)
        # likelihood_12 <- obs_12_zones_count*log(p_zones_1*p_zones_2)
        
        obs_0_zones_count <- round(obs_0_zones_count)
        n_a_zones <- round(n_a_zones)
        
        
        log_likelihood_zones <- dmultinom(c(obs_0_zones_count,obs_1_zones_count[i],obs_2_zones_count[i],obs_12_zones_count[i]),size=n_a_zones, 
                                                                  prob=c((1 - p_zones_1[i])*(1 - p_zones_2[i]),
                                                                         p_zones_1[i]*(1 - p_zones_2[i]),
                                                                         (1-p_zones_1[i])*p_zones_2[i],
                                                                         p_zones_1[i]*p_zones_2[i]), log=T)
        log_likelihood_zones + dpois(j,par[5],log=T)
      }
      
      LL_max <- max(likelihoods)
      LL <- LL_max + log(sum(exp(likelihoods-LL_max)))
      
      
      all_likelihoods <- c(all_likelihoods,LL)
    }
    
    #cat(num_inf,file = "C:/Users/guanxy/Google Drive/Harvey data/individual_correlation/clustering/distribution_fitting/logfile_server_1.txt", append=T,fill=T)
    
    log_likelihood <- sum(all_likelihoods)
    
    cat(log_likelihood,file = "G:/My Drive/trip bias paper/scenarios/results/logfile_server_1.txt", append=T,fill=T)
    return(-log_likelihood)    #########
  }
  
  
  # gibbs_samples <- matrix(NA,1000,3)    #alpha_1, alpha_2, error_1, error_2, n_a
  # colnames(gibbs_samples) <- c("alpha_1","alpha_2","n_a")
  # gibbs_samples[1,1] <- rnorm(1,0,1)
  # gibbs_samples[1,2] <- rnorm(1,0,1)
  # 
  # alpha_1 <- gibbs_samples[1,1]
  # alpha_2 <- gibbs_samples[1,2]
  # 
  # utility_1 <- alpha_1 + rnorm(obs_all_count,0,2)
  # utility_2 <- alpha_2 + rnorm(obs_all_count,0,2)
  # 
  # p_i_a_1 <- exp(utility_1)/(1+exp(utility_1))
  # p_i_a_2 <- exp(utility_2)/(1+exp(utility_2))
  # p_a_1 <- mean(p_i_a_1)
  # p_a_2 <- mean(p_i_a_2)
  # 
  # #lambda_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)    #equation 4
  # n_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)#rpois(1,lambda_a)
  # gibbs_samples[1,3] <- n_a
  # 
  # # clusterExport(cl, "likelihood_function")
  # # clusterExport(cl, "obs_all_count")
  # # clusterExport(cl, "gibbs_samples")
  # # clusterExport(cl, "obs_1_count")
  # # clusterExport(cl, "obs_2_count")
  # # clusterExport(cl, "obs_12_count")
  # 
  # for (i in 2:1000) {
  #   #clusterExport(cl, "i")
  #   
  #   alpha_1_set <- rnorm(2000,gibbs_samples[i-1,1],1)
  #   LLs <- sapply(alpha_1_set, function(x) likelihood_function(x,gibbs_samples[i-1,2]))
  #   LLs_scale <- exp(LLs - max(LLs,na.rm=T))
  #   LLs_scale <- LLs_scale/sum(LLs_scale,na.rm=T)
  #   LLs_scale[which(is.na(LLs_scale))] <- 0
  #   gibbs_samples[i,1] <- sample(alpha_1_set,1,prob=LLs_scale)
  #   
  #   alpha_2_set <- rnorm(2000,gibbs_samples[i-1,2],1)
  #   LLs <- sapply(alpha_2_set, function(x) likelihood_function(gibbs_samples[i,1],x))
  #   LLs_scale <- exp(LLs - max(LLs,na.rm=T))
  #   LLs_scale <- LLs_scale/sum(LLs_scale,na.rm=T)
  #   LLs_scale[which(is.na(LLs_scale))] <- 0
  #   gibbs_samples[i,2] <- sample(alpha_2_set,1,prob=LLs_scale)
  #   
  #   alpha_1 <- gibbs_samples[i,1]
  #   alpha_2 <- gibbs_samples[i,2]
  #   
  #   utility_1 <- alpha_1 + rnorm(obs_all_count,0,2)
  #   utility_2 <- alpha_2 + rnorm(obs_all_count,0,2)
  #   
  #   p_i_a_1 <- exp(utility_1)/(1+exp(utility_1))
  #   p_i_a_2 <- exp(utility_2)/(1+exp(utility_2))
  #   p_a_1 <- mean(p_i_a_1)
  #   p_a_2 <- mean(p_i_a_2)
  #   
  #   #lambda_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)    #equation 4
  #   n_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)#rpois(1,lambda_a)
  #   gibbs_samples[i,3] <- n_a
  #   
  #   write.csv(gibbs_samples,"G:/My Drive/trip bias paper/scenarios/results/gibbs_samples_1.csv",row.names = F)
  # }
  # 
  # stopCluster(cl)
  
  ###MLE --------------------------------------------------------------------------------------------------------------
  #-21.06326007,0.06178814,rep(-4.83395080,5)
  max_obs <- max(obs_zones_counts)
  
  estim_optim <- spg(c(-13.20567, 0.04487, 9.06718, -0.03219, max_obs + 100),likelihood_function,lower=c(rep(-Inf,4),max_obs),
               control = list(maxit = 300000, trace = TRUE, maxfeval=500000))
  # estim_optim <- optim(c(1.325397, 0.001, -1.905074, -0.001, max_obs + 100),likelihood_function, method="L-BFGS-B", lower = c(rep(-Inf,4),max_obs), upper = Inf, #hessian=T,
  #              control = list(maxit = 30000,trace = TRUE,ndeps=c(rep(0.0000000001,4),1),factr=0.45e9,REPORT=1))
  para_data <- estim_optim$par
  hessian_data <- hessian(likelihood_function,para_data,method.args=list(eps=1e-9)) #estim_optim$hessian
  se_data <- sqrt(diag(solve(hessian_data)))
  results <- data.frame(para_data,se_data)
  write.csv(results,paste("G:/My Drive/trip bias paper/scenarios/joint_prob/results_with_density/",a_file,".csv",sep=""),row.names=F)
}
