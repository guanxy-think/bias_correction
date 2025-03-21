###similar as inference_model_1_2_4.R in "zone_homogenity", but use binomial distributions.

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
  
  data_baseline <- read.csv(paste('G:/My Drive/trip bias paper/scenarios/representativeness/',a_file,'.csv',sep=""))
  
  pop_densities_unique <- unique(data_baseline$density_trips)
  obs_zones_counts <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==1 | data_baseline$capture_2==1) & data_baseline$density_trips==x)))
  
  # obs_1_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==1 & data_baseline$capture_2==0) & data_baseline$density_trips==x)))
  # obs_2_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==0 & data_baseline$capture_2==1) & data_baseline$density_trips==x)))
  # obs_12_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==1 & data_baseline$capture_2==1) & data_baseline$density_trips==x)))
  
  ###For N-mixture model
  obs_1_zones_count <- sapply(pop_densities_unique, function(x) length(which(data_baseline$capture_1==1 & data_baseline$density_trips==x)))
  obs_2_zones_count <- sapply(pop_densities_unique, function(x) length(which(data_baseline$capture_2==1 & data_baseline$density_trips==x)))
  
  obs_1_zones_count <- round(obs_1_zones_count)
  obs_2_zones_count <- round(obs_2_zones_count)
  #obs_12_zones_count <- round(obs_12_zones_count)
  
  ground_truth <- sapply(pop_densities_unique, function(x) length(which(data_baseline$density_trips==x)))
  fit_lm <- lm(log(ground_truth) ~ pop_densities_unique)
  fit_1_lm <- glm(data_baseline$capture_1~data_baseline$density_trips,family=binomial)
  fit_2_lm <- glm(data_baseline$capture_2~data_baseline$density_trips,family=binomial)
  
  likelihood_function <- function(par) {
    set.seed(1)
    
    #par[c(1,3,5)] <- par[c(1,3,5)]*10000
    
    utility_zones_1 <- par[1] + par[2]*pop_densities_unique
    utility_zones_2 <- par[3] + par[4]*pop_densities_unique
    p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
    p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
    inf_count <- length(which(p_zones_1 > 1-1e-10)) + length(which(p_zones_2 > 1-1e-10)) + length(which(p_zones_1 < 1e-10)) + length(which(p_zones_2 < 1e-10))
    p_zones_1[which(p_zones_1 > 1-1e-10)] <- 1-1e-10
    p_zones_2[which(p_zones_2 > 1-1e-10)] <- 1-1e-10
    p_zones_1[which(p_zones_1 < 1e-10)] <- 1e-10
    p_zones_2[which(p_zones_2 < 1e-10)] <- 1e-10
    
    lambda_a <- exp(par[5] + par[6]*pop_densities_unique)#obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)#rpois(1,lambda_a)
    # poisson_numbers <- t(sapply(1:2000, function(x) rpois(5,lambda_a)))
    poisson_upper_bound <- qpois(0.999999,lambda_a)#rep(5000,5)#

    cat(par,file = "C:/Users/guanxy/Documents/logfile_server_4.txt", append=T,fill=T)
    cat(lambda_a,file = "C:/Users/guanxy/Documents/logfile_server_4.txt", append=T,fill=T)
    cat(inf_count,file = "C:/Users/guanxy/Documents/logfile_server_4.txt", append=T,fill=T)
    
    if (max(poisson_upper_bound) > 100000) return(max(poisson_upper_bound))

    all_likelihoods <- c()
    for (i in 1:5) {
      likelihoods <- foreach(j = obs_zones_counts[i]:poisson_upper_bound[i], .combine="c", .export=c("obs_zones_counts","obs_1_zones_count","obs_2_zones_count")) %dopar% {
        
        obs_0_zones_count <- j - obs_zones_counts[i]
        n_a_zones <- j
        
        # likelihood_0 <- obs_0_zones_count*log((1 - p_zones_1)*(1 - p_zones_2))
        # likelihood_1 <- obs_1_zones_count*log(p_zones_1*(1 - p_zones_2))
        # likelihood_2 <- obs_2_zones_count*log((1-p_zones_1)*p_zones_2)
        # likelihood_12 <- obs_12_zones_count*log(p_zones_1*p_zones_2)
        
        obs_0_zones_count <- round(obs_0_zones_count)
        n_a_zones <- round(n_a_zones)
        
        
        # log_likelihood_zones <- dmultinom(c(obs_0_zones_count,obs_1_zones_count[i],obs_2_zones_count[i],obs_12_zones_count[i]),size=n_a_zones,
        #                                                           prob=c((1 - p_zones_1[i])*(1 - p_zones_2[i]),
        #                                                                  p_zones_1[i]*(1 - p_zones_2[i]),
        #                                                                  (1-p_zones_1[i])*p_zones_2[i],
        #                                                                  p_zones_1[i]*p_zones_2[i]), log=F)
        ### For N-mixture model
        log_likelihood_zones_1 <- dbinom(obs_1_zones_count[i], n_a_zones, p_zones_1[i])
        log_likelihood_zones_2 <- dbinom(obs_2_zones_count[i], n_a_zones, p_zones_2[i])
        log_likelihood_zones <- log_likelihood_zones_1*log_likelihood_zones_2
        
        log_likelihood_zones * dpois(j,lambda_a[i])
      }
      
      # LL_max <- max(likelihoods)
      likelihood_one_zone <- sum(likelihoods)
      
      
      all_likelihoods <- c(all_likelihoods,likelihood_one_zone)
    }
    
    #cat(num_inf,file = "C:/Users/guanxy/Documents/logfile_server_2.txt", append=T,fill=T)
    
    likelihood_output <- prod(all_likelihoods)
    
    #if (is.infinite(log_likelihood)) log_likelihood <- -100000
    
    cat(likelihood_output,file = "C:/Users/guanxy/Documents/logfile_server_4.txt", append=T,fill=T)
    return(likelihood_output)    #########
  }
  
  
  gibbs_samples <- matrix(NA,1000,6)    #alpha_1, alpha_2, error_1, error_2, n_a
  #colnames(gibbs_samples) <- c("alpha_1","alpha_2","n_a")
  gibbs_samples[1,1] <- rnorm(1,-4.588,0.2244)
  gibbs_samples[1,2] <- rnorm(1,-6.156e-07,1.693e-05)
  gibbs_samples[1,3] <- rnorm(1,-3.836,0.1084)
  gibbs_samples[1,4] <- rnorm(1,9.547e-05,7.383e-06)
  gibbs_samples[1,5] <- rnorm(1,7.022,0.05540)
  gibbs_samples[1,6] <- rnorm(1,9.260e-05,4.879e-06)
  
  sds <- c(0.2244,1.693e-05,0.1084,7.383e-06,0.05540,4.879e-06)

  for (i in 2:1000) {
    #clusterExport(cl, "i")
    
    for (j in 1:6) {
      para <- matrix(NA,25,6)
      para[,1:j] <- matrix(gibbs_samples[i,1:j],nrow=25,ncol=j,byrow=T)
      para[,j:6] <- matrix(gibbs_samples[i-1,j:6],nrow=25,ncol=7-j,byrow=T)
      para[,j] <- rnorm(25,gibbs_samples[i-1,j],sds[j])
      lhs <- sapply(1:25, function(x) likelihood_function(para[x,]))
      lhs_scale <- lhs/sum(lhs)
      gibbs_samples[i,j] <- sample(para[,j],1,prob=lhs_scale)
    }

    write.csv(gibbs_samples,"G:/My Drive/trip bias paper/scenarios/representativeness/inference_results/gibbs_samples_1_N_mixture.csv",row.names = F)
  }

  # stopCluster(cl)
  
  ###MLE --------------------------------------------------------------------------------------------------------------
  #-21.06326007,0.06178814,rep(-4.83395080,5)
  max_obs <- max(obs_zones_counts)
  
  estim_optim <- spg(c(-4.587858, 8.392916e-05, -3.835956, 0.0001987276, 7.021753, 1e-05),likelihood_function,lower=c(rep(-Inf,5),0.0000001),upper = c(rep(Inf,5),0.01),
               control = list(maxit = 300000, trace = TRUE, maxfeval=500000))    #, eps=0.0000001
  # estim_optim <- optim(c(-4.587858,-6.155694e-07, -3.835954,9.546615e-05, 7, 0.0001),likelihood_function, method="L-BFGS-B", lower = c(rep(-Inf,5),-0.01), upper = c(rep(Inf,5),0.01), #hessian=T,
  #                      control = list(maxit = 30000,trace = TRUE,ndeps=c(0.0000001,0.0000001,0.0000001,0.0000001,0.0000001,0.0000001),factr=1e5,REPORT=1))
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
  write.csv(results,paste("G:/My Drive/trip bias paper/scenarios/representativeness/inference_results/",a_file,"_multinom.csv",sep=""),row.names=F)
}
