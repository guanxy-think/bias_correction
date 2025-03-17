###similar as inference_model_1_2_8_indiv in "zone_homogenity".
###But using conditional likelihood estimation, i.e. alternating between betas (determining capture probabilities) and lambda. 

## Pay attention to the converging criteria factr=1e7
## Use the parscale parameter wisely
library(foreach)
library(doParallel)
no_cores <- 16
registerDoParallel(makeCluster(no_cores))
library(BB)
library(numDeriv)    #calculate gradient
library(dfoptim)    #https://stackoverflow.com/questions/11387330/errors-when-attempting-constrained-optimisation-using-optim

##Generate population density
#average population density in US: 283 per mile^2 (http://css.umich.edu/factsheets/us-cities-factsheet)

file_names <- c("SR_SU")
                #"SR_LU","SU_LU","LU09_LU"
                #"scenario_0.1_0.1","baseline_unif","baseline_beta_left","baseline_beta_right","scen1_unif","scen1_beta_left","scen1_beta_right","scen2_0.1_unif","scen2_0.1_beta_left",
                #"scen2_0.5_unif","scen2_0.5_beta_left","scen2_0.9_unif","scen2_0.9_beta_left",)

#a_file <- file_names[1]
for (a_file in file_names) {
  
  data_baseline <- read.csv(paste('G:/My Drive/trip bias paper/scenarios/representativeness/more_zones/',a_file,'_regen_with_income_indiv.csv',sep=""))
  
  pop_densities_unique <- unique(data_baseline$trip_density_new)
  obs_zones_counts <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1_new==1 | data_baseline$capture_2_new==1) & data_baseline$trip_density_new==x)))
  
  ###For 4-category model
  obs_1_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1_new==1 & data_baseline$capture_2_new==0) & data_baseline$trip_density_new==x)))
  obs_2_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1_new==0 & data_baseline$capture_2_new==1) & data_baseline$trip_density_new==x)))
  obs_12_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1_new==1 & data_baseline$capture_2_new==1) & data_baseline$trip_density_new==x)))
  
  ###For N-mixture model
  # obs_1_zones_count <- sapply(pop_densities_unique, function(x) length(which(data_baseline$capture_1==1 & data_baseline$density_trips==x)))
  # obs_2_zones_count <- sapply(pop_densities_unique, function(x) length(which(data_baseline$capture_2==1 & data_baseline$density_trips==x)))
  
  obs_1_zones_count <- round(obs_1_zones_count)
  obs_2_zones_count <- round(obs_2_zones_count)
  obs_12_zones_count <- round(obs_12_zones_count)
  
  ###Zone-level income
  income_data <- read.csv('G:/My Drive/trip bias paper/scenarios/representativeness/more_zones/zone_incomes_regen_with_income_indiv.csv')
  ##mean or median?#######################
  income_zone <- sapply(pop_densities_unique, function(x) median(income_data$people_income[which(income_data$people_density==x)]))
  
  ground_truth <- sapply(pop_densities_unique, function(x) length(which(data_baseline$trip_density_new==x)))
  fit_lm <- lm(log(ground_truth) ~ pop_densities_unique +income_zone)
  fit_1_lm <- glm(data_baseline$capture_1_new~data_baseline$trip_density+data_baseline$trip_income,family=binomial) #glm(data_baseline$capture_1_new~data_baseline$trip_density+data_baseline$trip_income,family=binomial)
  fit_2_lm <- glm(data_baseline$capture_2_new~data_baseline$trip_density+data_baseline$trip_income,family=binomial)
  
  index_1 <- which(data_baseline$capture_1_new==1 & data_baseline$capture_2_new==0)
  index_12 <- which(data_baseline$capture_1_new==1 & data_baseline$capture_2_new==1)
  index_2 <- which(data_baseline$capture_1_new==0 & data_baseline$capture_2_new==1)
  index_0 <- which(data_baseline$capture_1_new==0 & data_baseline$capture_2_new==0)   #unknown to modeler
  
  likelihood_function_1 <- function(par, par_known_1) {
    set.seed(1)
    
    utility_trips_1 <- par[1] + par[2]*data_baseline$trip_density_new + par[3]*data_baseline$trip_income_new
    utility_trips_2 <- par[4] + par[5]*data_baseline$trip_density_new + par[6]*data_baseline$trip_income_new
    p_trips_1 <- 1 - 1/(1+exp(utility_trips_1))
    p_trips_2 <- 1 - 1/(1+exp(utility_trips_2))
    inf_count_trips <- length(which(p_trips_1 > 1-1e-50)) + length(which(p_trips_2 > 1-1e-50)) + length(which(p_trips_1 < 1e-50)) + length(which(p_trips_2 < 1e-50))
    p_trips_1[which(p_trips_1 >= 1-1e-50)] <- 1-1e-50
    p_trips_2[which(p_trips_2 >= 1-1e-50)] <- 1-1e-50
    p_trips_1[which(p_trips_1 <= 1e-50)] <- 1e-50
    p_trips_2[which(p_trips_2 <= 1e-50)] <- 1e-50
    
    LL_1 <- sum(log(p_trips_1[index_1]) + log(1-p_trips_2[index_1]))
    LL_12 <- sum(log(p_trips_1[index_12]) + log(p_trips_2[index_12]))
    LL_2 <- sum(log(1-p_trips_1[index_2]) + log(p_trips_2[index_2]))
    LL_0 <- sum(log(1-p_trips_1[index_0]) + log(1-p_trips_2[index_0]))
    
    log_likelihood_obs <- LL_1 + LL_12 + LL_2
    
    utility_zones_1 <- par[1] + par[2]*pop_densities_unique + par[3]*income_zone
    utility_zones_2 <- par[4] + par[5]*pop_densities_unique + par[6]*income_zone
    p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
    p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
    inf_count <- length(which(p_zones_1 > 1-1e-50)) + length(which(p_zones_2 > 1-1e-50)) + length(which(p_zones_1 < 1e-50)) + length(which(p_zones_2 < 1e-50))
    p_zones_1[which(p_zones_1 > 1-1e-50)] <- 1-1e-50
    p_zones_2[which(p_zones_2 > 1-1e-50)] <- 1-1e-50
    p_zones_1[which(p_zones_1 < 1e-50)] <- 1e-50
    p_zones_2[which(p_zones_2 < 1e-50)] <- 1e-50

    lambda_a <- exp(par_known_1[1] + par_known_1[2]*pop_densities_unique + par_known_1[3]*income_zone)#obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)#rpois(1,lambda_a)
    poisson_upper_bound <- qpois(0.99999999,lambda_a)#rep(5000,5)#        ##################################

    zone_trips <- obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)

    cat(par,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    cat(zone_trips,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    cat(lambda_a,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    cat(inf_count,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    cat(inf_count_trips,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)

    if (max(poisson_upper_bound) > 30000) return(max(poisson_upper_bound))  ##############################
    if (max(lambda_a < obs_zones_counts) == 1) return(30000)              ################################

    all_likelihoods <- c()
    for (i in 1:10) {
      likelihoods <- foreach(j = obs_zones_counts[i]:poisson_upper_bound[i], .combine="c", .export=c("obs_zones_counts","obs_1_zones_count","obs_2_zones_count","obs_12_zones_count")) %dopar% {

        obs_0_zones_count <- j - obs_zones_counts[i]
        n_a_zones <- j

        likelihood_0 <- obs_0_zones_count*(log(1 - p_zones_1[i]) + log(1 - p_zones_2[i])) # dbinom(obs_0_zones_count,n_a_zones,(1 - p_zones_1[i])*(1 - p_zones_2[i]),log=T)#   # binom
        # likelihood_1 <- obs_1_zones_count[i]*log(p_zones_1[i]*(1 - p_zones_2[i]))
        # likelihood_2 <- obs_2_zones_count[i]*log((1-p_zones_1[i])*p_zones_2[i])
        # likelihood_12 <- obs_12_zones_count[i]*log(p_zones_1[i]*p_zones_2[i])

        log_likelihood_zones <- likelihood_0

        # log_likelihood_zones <- dmultinom(c(obs_0_zones_count,obs_1_zones_count[i],obs_2_zones_count[i],obs_12_zones_count[i]),size=n_a_zones,
        #                                                           prob=c((1 - p_zones_1[i])*(1 - p_zones_2[i]),
        #                                                                  p_zones_1[i]*(1 - p_zones_2[i]),
        #                                                                  (1-p_zones_1[i])*p_zones_2[i],
        #                                                                  p_zones_1[i]*p_zones_2[i]), log=F)
        ### For N-mixture model
        # log_likelihood_zones_1 <- dbinom(obs_1_zones_count[i], n_a_zones, p_zones_1[i])
        # log_likelihood_zones_2 <- dbinom(obs_2_zones_count[i], n_a_zones, p_zones_2[i])
        # log_likelihood_zones <- log_likelihood_zones_1*log_likelihood_zones_2

        log_likelihood_zones + dpois(j,lambda_a[i], log=T)
      }

      LL_max <- max(likelihoods)
      LL <- LL_max + log(sum(exp(likelihoods-LL_max)))

      #likelihood_one_zone <- sum(likelihoods)

      all_likelihoods <- c(all_likelihoods, LL)
    }
    
    log_likelihood_output <- sum(all_likelihoods) + LL_1 + LL_2 + LL_12 #+ LL_0 #
    
    cat(log_likelihood_output,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    return(-log_likelihood_output)    #########
  }
  
  likelihood_function_2 <- function(par, par_known_2) {
    set.seed(1)
    
    utility_trips_1 <- par_known_2[1] + par_known_2[2]*data_baseline$trip_density_new + par_known_2[3]*data_baseline$trip_income_new
    utility_trips_2 <- par_known_2[4] + par_known_2[5]*data_baseline$trip_density_new + par_known_2[6]*data_baseline$trip_income_new
    p_trips_1 <- 1 - 1/(1+exp(utility_trips_1))
    p_trips_2 <- 1 - 1/(1+exp(utility_trips_2))
    inf_count_trips <- length(which(p_trips_1 > 1-1e-50)) + length(which(p_trips_2 > 1-1e-50)) + length(which(p_trips_1 < 1e-50)) + length(which(p_trips_2 < 1e-50))
    p_trips_1[which(p_trips_1 > 1-1e-50)] <- 1-1e-50
    p_trips_2[which(p_trips_2 > 1-1e-50)] <- 1-1e-50
    p_trips_1[which(p_trips_1 < 1e-50)] <- 1e-50
    p_trips_2[which(p_trips_2 < 1e-50)] <- 1e-50
    
    LL_1 <- sum(log(p_trips_1[index_1]) + log(1-p_trips_2[index_1]))
    LL_12 <- sum(log(p_trips_1[index_12]) + log(p_trips_2[index_12]))
    LL_2 <- sum(log(1-p_trips_1[index_2]) + log(p_trips_2[index_2]))
    LL_0 <- sum(log(1-p_trips_1[index_0]) + log(1-p_trips_2[index_0]))
    
    log_likelihood_obs <- LL_1 + LL_12 + LL_2
    
    utility_zones_1 <- par_known_2[1] + par_known_2[2]*pop_densities_unique + par_known_2[3]*income_zone
    utility_zones_2 <- par_known_2[4] + par_known_2[5]*pop_densities_unique + par_known_2[6]*income_zone
    p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
    p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
    inf_count <- length(which(p_zones_1 > 1-1e-50)) + length(which(p_zones_2 > 1-1e-50)) + length(which(p_zones_1 < 1e-50)) + length(which(p_zones_2 < 1e-50))
    p_zones_1[which(p_zones_1 > 1-1e-50)] <- 1-1e-50
    p_zones_2[which(p_zones_2 > 1-1e-50)] <- 1-1e-50
    p_zones_1[which(p_zones_1 < 1e-50)] <- 1e-50
    p_zones_2[which(p_zones_2 < 1e-50)] <- 1e-50
    
    lambda_a <- exp(par[1] + par[2]*pop_densities_unique + par[3]*income_zone)#obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)#rpois(1,lambda_a)
    poisson_upper_bound <- qpois(0.99999999,lambda_a)#rep(5000,5)#        ##################################
    
    zone_trips <- obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)
    
    cat(par,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    cat(zone_trips,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    cat(lambda_a,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    cat(inf_count,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    cat(inf_count_trips,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    
    if (max(poisson_upper_bound) > 30000) return(max(poisson_upper_bound))  ##############################
    if (max(lambda_a < obs_zones_counts) == 1) return(30000)              ################################
    
    all_likelihoods <- c()
    for (i in 1:10) {
      likelihoods <- foreach(j = obs_zones_counts[i]:poisson_upper_bound[i], .combine="c", .export=c("obs_zones_counts","obs_1_zones_count","obs_2_zones_count","obs_12_zones_count")) %dopar% {
        
        obs_0_zones_count <- j - obs_zones_counts[i]
        n_a_zones <- j
        
        likelihood_0 <- obs_0_zones_count*(log(1 - p_zones_1[i]) + log(1 - p_zones_2[i])) # dbinom(obs_0_zones_count,n_a_zones,(1 - p_zones_1[i])*(1 - p_zones_2[i]),log=T)#   # binom
        # likelihood_1 <- obs_1_zones_count[i]*log(p_zones_1[i]*(1 - p_zones_2[i]))
        # likelihood_2 <- obs_2_zones_count[i]*log((1-p_zones_1[i])*p_zones_2[i])
        # likelihood_12 <- obs_12_zones_count[i]*log(p_zones_1[i]*p_zones_2[i])
        
        log_likelihood_zones <- likelihood_0
        
        # log_likelihood_zones <- dmultinom(c(obs_0_zones_count,obs_1_zones_count[i],obs_2_zones_count[i],obs_12_zones_count[i]),size=n_a_zones,
        #                                                           prob=c((1 - p_zones_1[i])*(1 - p_zones_2[i]),
        #                                                                  p_zones_1[i]*(1 - p_zones_2[i]),
        #                                                                  (1-p_zones_1[i])*p_zones_2[i],
        #                                                                  p_zones_1[i]*p_zones_2[i]), log=F)
        ### For N-mixture model
        # log_likelihood_zones_1 <- dbinom(obs_1_zones_count[i], n_a_zones, p_zones_1[i])
        # log_likelihood_zones_2 <- dbinom(obs_2_zones_count[i], n_a_zones, p_zones_2[i])
        # log_likelihood_zones <- log_likelihood_zones_1*log_likelihood_zones_2
        
        log_likelihood_zones + dpois(j,lambda_a[i], log=T)
      }
      
      LL_max <- max(likelihoods)
      LL <- LL_max + log(sum(exp(likelihoods-LL_max)))
      
      #likelihood_one_zone <- sum(likelihoods)
      
      all_likelihoods <- c(all_likelihoods, LL)
    }
    
    log_likelihood_output <- sum(all_likelihoods) + LL_1 + LL_2 + LL_12 #+ LL_0 #
    
    cat(log_likelihood_output,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    return(-log_likelihood_output)    #########
  }
  
  
  # gibbs_samples <- matrix(NA,1000,6)    #alpha_1, alpha_2, error_1, error_2, n_a
  # #colnames(gibbs_samples) <- c("alpha_1","alpha_2","n_a")
  # gibbs_samples[1,1] <- rnorm(1,-4.588,0.2244)
  # gibbs_samples[1,2] <- rnorm(1,-6.156e-07,1.693e-05)
  # gibbs_samples[1,3] <- rnorm(1,-3.836,0.1084)
  # gibbs_samples[1,4] <- rnorm(1,9.547e-05,7.383e-06)
  # gibbs_samples[1,5] <- rnorm(1,7.022,0.05540)
  # gibbs_samples[1,6] <- rnorm(1,9.260e-05,4.879e-06)
  # 
  # sds <- c(0.2244,1.693e-05,0.1084,7.383e-06,0.05540,4.879e-06)
  # 
  # for (i in 2:1000) {
  #   #clusterExport(cl, "i")
  #   
  #   for (j in 1:6) {
  #     para <- matrix(NA,25,6)
  #     para[,1:j] <- matrix(gibbs_samples[i,1:j],nrow=25,ncol=j,byrow=T)
  #     para[,j:6] <- matrix(gibbs_samples[i-1,j:6],nrow=25,ncol=7-j,byrow=T)
  #     para[,j] <- rnorm(25,gibbs_samples[i-1,j],sds[j])
  #     lhs <- sapply(1:25, function(x) likelihood_function(para[x,]))
  #     lhs_scale <- lhs/sum(lhs)
  #     gibbs_samples[i,j] <- sample(para[,j],1,prob=lhs_scale)
  #   }
  # 
  #   write.csv(gibbs_samples,"G:/My Drive/trip bias paper/scenarios/representativeness/inference_results/gibbs_samples_1_N_mixture.csv",row.names = F)
  # }

  # stopCluster(cl)
  
  ###MLE --------------------------------------------------------------------------------------------------------------
  #-21.06326007,0.06178814,rep(-4.83395080,5)
  max_obs <- max(obs_zones_counts)
  
  para_est <- matrix(NA, 100, 9)
  para_est[1,] <- c(fit_1_lm$coefficients, fit_2_lm$coefficients, fit_lm$coefficients)
  for (i in 2:100) {
    estim_optim_1 <- optim(para_est[i-1,1:6], likelihood_function_1, par_known_1=para_est[i-1,7:9], method="L-BFGS-B", lower=c(rep(-Inf,6)),upper = c(rep(Inf,6)),
                       control = list(maxit = 300000, trace = TRUE, ndeps=rep(0.0000001,6),factr=1e5,REPORT=1))
    para_est[i,1:6] <- estim_optim_1$par
    
    estim_optim_2 <- optim(para_est[i-1,7:9], likelihood_function_2, par_known_2=para_est[i,1:6], method="L-BFGS-B", lower=c(rep(-Inf,3)),upper = c(rep(Inf,3)),
                         control = list(maxit = 300000, trace = TRUE, ndeps=rep(0.0000001,3),factr=1e5,REPORT=1))
    para_est[i,7:9] <- estim_optim_2$par
    
    write.csv(para_est, paste("G:/My Drive/trip bias paper/scenarios/representativeness/more_zones/inference_results/",a_file,"_indivModel_indivGen_conditLL_1.csv",sep=""),row.names=F)
  }
  
  estim_optim <- spg(c(fit_1_lm$coefficients, fit_2_lm$coefficients, fit_lm$coefficients),likelihood_function,lower=c(rep(-Inf,9)),upper = c(rep(Inf,9)),
               control = list(maxit = 300000, trace = TRUE, maxfeval=500000))    #, eps=0.0000001
  estim_optim <- optim(para_data,likelihood_function, method="L-BFGS-B", lower = rep(-Inf,9), upper = rep(Inf,9), #hessian=T,
                       control = list(maxit = 30000,trace = TRUE,ndeps=rep(0.0000001,9),factr=1e5,REPORT=1))
  # estim_potim <- optim(c(0.01,-0.000000001,0.00001,0.00001, obs_zones_counts + 100),likelihood_function, method="L-BFGS-B", lower = c(rep(-Inf,4),obs_zones_counts), upper = c(rep(Inf,4),rep(10000,5)), #hessian=T,
  #              control = list(maxit = 30000,trace = TRUE,parscale=c(10,0.01,10,0.01,1000,1000,1000,1000,1000),ndeps=c(rep(0.000000001,9)),factr=1e5,REPORT=1))
  # estim_optim <- hjkb(c(-10,0.01,10,-0.01, max_obs + 100),likelihood_function, lower = c(rep(-Inf,4),max_obs), upper = c(rep(Inf,5)),
  #                      control = list(tol=1e-8,maxfeval=100000,info=T))
  # estim_optim <- nmkb(c(-10,0.01,10,-0.01, obs_zones_counts + 100),likelihood_function, lower = c(rep(-Inf,4),obs_zones_counts), upper = c(rep(Inf,4),rep(5000,5)),
  #                     control = list(tol=1e-8,maxfeval=100000,trace=T))
  para_data <- estim_optim$par
  hessian_data <- hessian(likelihood_function,para_data,method.args=list(eps=1e-9,d=1e-4)) #estim_optim$hessian
  se_data <- sqrt(diag(solve(hessian_data)))
  results <- data.frame(para_data,se_data)
  write.csv(results,paste("G:/My Drive/trip bias paper/scenarios/representativeness/more_zones/inference_results/",a_file,"_indivModel_indivGen.csv",sep=""),row.names=F)
}


### Diagnosis of parametrer estimates --------------------------------------------------------------------------------------------------------
landscape_matrix <- matrix(NA, 2001, 9)
for (i in 1:9) {
  para <- para_data[i]
  para_copy <- para_data
  candidates <- seq(para-1, para+1, 0.001)
  for (j in 1:2001) {
    para_replace <- candidates[j]
    para_copy[i] <- para_replace
    ll <- likelihood_function(para_copy)
    landscape_matrix[j,i] <- ll
  }
}
write.csv(landscape_matrix,'G:/My Drive/trip bias paper/scenarios/representativeness/more_zones/indivModelDiagnosis.csv', row.names = F)
plot(landscape_matrix[900:1100,6])
