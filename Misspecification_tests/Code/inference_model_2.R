###compared to inference_model_1, poisson distribution is not randomly sampled (line 40). Instead, a fix summation bound is used. Also, there is only one zone.
###for unobserved users, their average factors will also be estimated
library(foreach)
library(doParallel)
no_cores <- 16
# clust <- makeCluster(no_cores)
registerDoParallel(makeCluster(no_cores))
library(BB)
library(numDeriv)    #calculate gradient

##Generate population density
#average population density in US: 283 per mile^2 (http://css.umich.edu/factsheets/us-cities-factsheet)

# clusterEvalQ(clust, library("doParallel"))

file_names <- c("scenario_0.1_0.1","scenario_0.1_0.5","scenario_0.1_0.9","scenario_0.5_0.1","scenario_0.5_0.5","scenario_0.5_0.9","scenario_0.9_0.1","scenario_0.9_0.5","scenario_0.9_0.9")
                #"baseline_unif","baseline_beta_left","baseline_beta_right","scen1_unif","scen1_beta_left","scen1_beta_right","scen2_0.1_unif","scen2_0.1_beta_left",
                #"scen2_0.5_unif","scen2_0.5_beta_left","scen2_0.9_unif","scen2_0.9_beta_left",)

#a_file <- file_names[1]
for (a_file in file_names) {
  
  data_baseline <- read.csv(paste('G:/My Drive/trip bias paper/scenarios/conditional_overlap/',a_file,'.csv',sep=""))
  
  avg_density <- mean(unique(data_baseline$popu_density))
  
  obs_user_1_index <- which(data_baseline$capture_1==1 & data_baseline$capture_2==0)
  obs_user_2_index <- which(data_baseline$capture_1==0 & data_baseline$capture_2==1)
  obs_user_12_index <- which(data_baseline$capture_1==1 & data_baseline$capture_2==1)
  obs_count <- length(obs_user_1_index) + length(obs_user_2_index) + length(obs_user_12_index)
  
  # clusterExport(clust, "data_baseline")
  # clusterExport(clust, "obs_user_1_index")
  # clusterExport(clust, "obs_user_2_index")
  # clusterExport(clust, "obs_user_12_index")
  # clusterExport(clust, "obs_count")
  # clusterExport(clust, "avg_density")
  
  likelihood_function <- function(par) {
    set.seed(1)
    
    utility_1 <- par[1] + par[2]*data_baseline$popu_density
    utility_2 <- par[3] + par[4]*data_baseline$popu_density
    p_user_1 <- 1 - 1/(1+exp(utility_1))
    p_user_2 <- 1 - 1/(1+exp(utility_2))

    # average_density_0 <- par[5]
    average_density_0 <- avg_density
    utility_1_0 <- par[1] + par[2]*average_density_0
    utility_2_0 <- par[3] + par[4]*average_density_0
    p_1_0 <- 1 - 1/(1+exp(utility_1_0))
    p_2_0 <- 1 - 1/(1+exp(utility_2_0))
    missing_probability <- (1-p_1_0)*(1-p_2_0)
    
    threshold_upper <- 1-1e-15
    threshold_lower <- 1e-15
    p_user_1[which(p_user_1 > threshold_upper)] <- threshold_upper
    p_user_2[which(p_user_2 > threshold_upper)] <- threshold_upper
    p_user_1[which(p_user_1 < threshold_lower)] <- threshold_lower
    p_user_2[which(p_user_2 < threshold_lower)] <- threshold_lower
    if (p_1_0 > threshold_upper) p_1_0 <- threshold_upper
    if (p_1_0 < threshold_lower) p_1_0 <- threshold_lower
    if (p_2_0 > threshold_upper) p_2_0 <- threshold_upper
    if (p_2_0 < threshold_lower) p_2_0 <- threshold_lower
    
    lambda <- par[5]
    # lambda <- obs_count/(1 - missing_probability)
    sum_upper_bound <- qpois(0.999, lambda)
    
    cat(par,file = "G:/My Drive/trip bias paper/scenarios/results/logfile_server.txt", append=T,fill=T)
    
    all_likeluhoods <- foreach(total_count = obs_count:sum_upper_bound, .combine="c", .export=c("obs_count","obs_user_1_index","obs_user_2_index","obs_user_12_index")) %dopar% {
      
      obs_0_count <- max(total_count - obs_count, 0)
      
      likelihood_1 <- p_user_1[obs_user_1_index] * (1 - p_user_2[obs_user_1_index])
      likelihood_2 <- p_user_2[obs_user_2_index] * (1 - p_user_1[obs_user_2_index])
      likelihood_12 <- p_user_1[obs_user_12_index] * p_user_2[obs_user_12_index]
      likelihood_0 <- dbinom(obs_0_count, total_count, missing_probability, log=T)
      
      LL_1 <- log(likelihood_1)
      # LL_1[which(is.infinite(LL_1))] <- -9999
      LL_2 <- log(likelihood_2)
      # LL_2[which(is.infinite(LL_2))] <- -9999
      LL_12 <- log(likelihood_12)
      # LL_12[which(is.infinite(LL_12))] <- -9999
      # if (is.infinite(likelihood_0)) likelihood_0 <- -9999999
      
      log_likelihood_all <- sum(LL_1) + sum(LL_2) + sum(LL_12) + likelihood_0 + dpois(total_count,lambda,log=T)
      
      log_likelihood_all
    }
    
    
    #cat(num_inf,file = "C:/Users/guanxy/Google Drive/Harvey data/individual_correlation/clustering/distribution_fitting/logfile_server.txt", append=T,fill=T)
    
    LL_max <- max(all_likeluhoods)
    log_likelihood <- LL_max + log(sum(exp(all_likeluhoods-LL_max)))
    
    cat(log_likelihood,file = "G:/My Drive/trip bias paper/scenarios/results/logfile_server.txt", append=T,fill=T)
    return(-log_likelihood)    #########
  }
  
  # ###Gibbs sampling -------------------------------------------------------------------------------------------------------------------------------------------
  # clusterExport(clust, "likelihood_function")
  # 
  # gibbs_samples <- matrix(NA,1000,5)    #alpha_1, beta_1, alpha_2, beta_2, density_0, lambda
  # colnames(gibbs_samples) <- c("alpha_1", "beta_1", "alpha_2", "beta_2", "density_0")
  # gibbs_samples[1,1] <- -2#rnorm(1,-2,1)
  # gibbs_samples[1,2] <- 0.02#rnorm(1,0,1)
  # gibbs_samples[1,3] <- -2#rnorm(1,-2,1)
  # gibbs_samples[1,4] <- 0.01#rnorm(1,0,1)
  # # gibbs_samples[1,5] <- 300#rgamma(1,shape=10,scale=30)
  # gibbs_samples[1,5] <- obs_count+1000#rpois(1,obs_count+1000)
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
  #   for (j in 1:5) {
  #     if (j < 5) {
  #       par_set <- rnorm(200,gibbs_samples[i-1,j],1)
  #     # } else if (j == 5) {
  #     #   par_set <- rgamma(200,shape=10,scale=gibbs_samples[i-1,j]/10)
  #     } else {
  #       par_set <- rpois(200,gibbs_samples[i-1,j])
  #     }
  #     
  #     pars <- gibbs_samples[i-1,]
  #     if (j > 1) pars[1:(j-1)] <- gibbs_samples[i,1:(j-1)]
  #     all_pars <- matrix(pars,200,5,byrow=T)
  #     all_pars[,j] <- par_set
  #     
  #     clusterExport(clust, "all_pars")
  #     LLs <- parSapply(clust, 1:200, function(x) likelihood_function(all_pars[x,]))
  #     LLs_scale <- exp(LLs - max(LLs,na.rm=T))
  #     LLs_scale <- LLs_scale/sum(LLs_scale,na.rm=T)
  #     LLs_scale[which(is.na(LLs_scale))] <- 0
  #     gibbs_samples[i,j] <- sample(par_set,1,prob=LLs_scale)
  #   }
  # 
  #   write.csv(gibbs_samples,paste("G:/My Drive/trip bias paper/scenarios/conditional_overlap/results/gibbs_",a_file,".csv",sep=""),row.names=F)
  # }
  # 
  # # stopCluster(clust)
  
  ###MLE --------------------------------------------------------------------------------------------------------------
  # #-21.06326007,0.06178814,rep(-4.83395080,5)
  estim <- spg(c(-2.001485,0.01633865,-1.997893,0.001054262,obs_count+5000),likelihood_function,lower=c(rep(-Inf,4),obs_count),control = list(maxit = 300000, trace = TRUE, maxfeval=500000))
  # estim <- optim(c(-2.001485,0.01633865,-1.997893,0.001054262,300,11223),likelihood_function, method="L-BFGS-B", lower = c(rep(-Inf,4),0,obs_count), upper = Inf, hessian=T,
  #              control = list(maxit = 30000,trace = TRUE,ndeps=c(rep(0.0000000001,4),1,10),factr=0.45e9,REPORT=1))
  para_data <- estim$par
  hessian_data <- hessian(likelihood_function,para_data,method.args=list(eps=1e-9))
  se_data <- sqrt(diag(solve(hessian_data)))
  results <- data.frame(para_data,se_data)
  write.csv(results,paste("G:/My Drive/trip bias paper/scenarios/conditional_overlap/results/",a_file,".csv",sep=""),row.names=F)
}

stopImplicitCluster()
