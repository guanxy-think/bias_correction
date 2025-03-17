library(foreach)
library(parallel)
no_cores <- 16
cl <- makeCluster(no_cores)
#registerDoParallel(cl)
library(BB)
library(numDeriv)    #calculate gradient

##Generate population density
#average population density in US: 283 per mile^2 (http://css.umich.edu/factsheets/us-cities-factsheet)

file_names <- c("scen1_beta_right","baseline_beta_right")
                #"baseline_unif","baseline_beta_left","scen1_unif","scen1_beta_left",,"scen2_0.1_unif","scen2_0.1_beta_left","scen2_0.1_beta_right",
                #"scen2_0.5_unif","scen2_0.5_beta_left","scen2_0.5_beta_right","scen2_0.9_unif","scen2_0.9_beta_left","scen2_0.9_beta_right")

for (a_file in file_names) {
  
  data_baseline <- read.csv(paste('G:/My Drive/trip bias paper/scenarios/',a_file,'.csv',sep=""))
  
  obs_1 <- which(data_baseline$capture_1==1 & data_baseline$capture_2==0)
  obs_2 <- which(data_baseline$capture_1==0 & data_baseline$capture_2==1)
  obs_12 <- which(data_baseline$capture_1==1 & data_baseline$capture_2==1)
  all_obs <- c(obs_1,obs_2,obs_12)
  obs_1_count <- length(obs_1)
  obs_2_count <- length(obs_2)
  obs_12_count <- length(obs_12)
  obs_all_count <- length(all_obs)
  
  pop_densities_unique <- unique(data_baseline$popu_density)
  obs_zones_counts <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==1 | data_baseline$capture_2==1) & data_baseline$popu_density==x)))
  
  likelihood_function <- function(par) {
    set.seed(1)
    noise_1 <- rnorm(obs_all_count,0,2)
    noise_2 <- rnorm(obs_all_count,0,2)
    
    utility_1 <- par[1] + par[2]*data_baseline$popu_density[all_obs] + noise_1
    utility_2 <- par[3] + noise_2# + par[4]*data_baseline$popu_density[all_obs] 
    
    p_i_a_1 <- 1 - 1/(1+exp(utility_1))#exp(utility_1)/(1+exp(utility_1))
    p_i_a_2 <- 1 - 1/(1+exp(utility_2))#exp(utility_2)/(1+exp(utility_2))
    #https://stackoverflow.com/questions/63495476/r-getting-very-small-number-instead-of-zero (searching "R small number zero")
    p_i_a_1[which(p_i_a_1 > 0.9999999999)] <- 0.9999999999
    p_i_a_2[which(p_i_a_2 > 0.9999999999)] <- 0.9999999999
    p_i_a_1[which(p_i_a_1 < 0.0000000001)] <- 0.0000000001
    p_i_a_2[which(p_i_a_2 < 0.0000000001)] <- 0.0000000001
    
    utility_zones_1 <- par[1] + par[2]*pop_densities_unique
    utility_zones_2 <- par[3]#*pop_densities_unique
    p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
    p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
    p_zones_1[which(p_zones_1 > 0.9999999999)] <- 0.9999999999
    p_zones_2[which(p_zones_2 > 0.9999999999)] <- 0.9999999999
    p_zones_1[which(p_zones_1 < 0.0000000001)] <- 0.0000000001
    p_zones_2[which(p_zones_2 < 0.0000000001)] <- 0.0000000001
    
    #lambda_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)    #equation 4
    n_a_zones <- obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)#rpois(1,lambda_a)
    
    obs_0_zones_count <- sapply(n_a_zones - obs_zones_counts, function(x) max(x,0))
    
    likelihood_0 <- obs_0_zones_count*log((1 - p_zones_1)*(1 - p_zones_2))
    likelihood_1 <- log(p_i_a_1[1:obs_1_count]*(1 - p_i_a_2[1:obs_1_count]))
    if (obs_1_count == 0) likelihood_1 <- 0
    likelihood_2 <- log((1 - p_i_a_1[(obs_1_count+1):(obs_1_count+obs_2_count)])*p_i_a_2[(obs_1_count+1):(obs_1_count+obs_2_count)])
    if (obs_2_count == 0) likelihood_2 <- 0
    likelihood_12 <- log(p_i_a_1[(obs_1_count+obs_2_count+1):obs_all_count]*p_i_a_2[(obs_1_count+obs_2_count+1):obs_all_count])
    if (obs_12_count == 0) likelihood_12 <- 0
    log_likelihood <- sum(likelihood_0)+sum(likelihood_1)+sum(likelihood_2)+sum(likelihood_12)
    
    #cat(par,file = "G:/My Drive/trip bias paper/scenarios/results/logfile_server.txt", append=T,fill=T)
    #cat(num_inf,file = "C:/Users/guanxy/Google Drive/Harvey data/individual_correlation/clustering/distribution_fitting/logfile_server.txt", append=T,fill=T)
    #cat(log_likelihood,file = "G:/My Drive/trip bias paper/scenarios/results/logfile_server.txt", append=T,fill=T)
    
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
  estim <- spg(c(-18.25310899,0.09060964,-5.84809884),likelihood_function,lower=c(-Inf,-Inf,-Inf),
               control = list(maxit = 100000, trace = TRUE, maxfeval=500000))
  # estim_optim <- optim(c(0.70344097,  0.99653960,  0.01855751, -0.99000000),likelihood_function, method="L-BFGS-B", lower = c(-Inf,-Inf,-Inf,-Inf), upper = Inf, hessian=T,
  #              control = list(maxit = 30000,trace = TRUE,ndeps=rep(0.0000000001,4),factr=0.45e9,REPORT=1))
  para_data <- estim$par
  hessian_data <- hessian(likelihood_function,para_data,method.args=list(eps=1e-9))
  se_data <- sqrt(diag(solve(hessian_data)))
  results <- data.frame(para_data,se_data)
  write.csv(results,paste("G:/My Drive/trip bias paper/scenarios/results/pop_density/",a_file,".csv",sep=""),row.names=F)
}
