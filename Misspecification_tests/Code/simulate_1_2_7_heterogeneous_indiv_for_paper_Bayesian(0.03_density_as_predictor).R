###For paper writing purpose.
###Similar to "inference_model_1_2_7_heterogeneous_indiv.R"

## Pay attention to the converging criteria factr=1e7
## Use the parscale parameter wisely
library(foreach)
library(doParallel)
no_cores <- 16
#registerDoParallel(makeCluster(no_cores))
cl <- makeCluster(no_cores)
library(BB)
library(numDeriv)    #calculate gradient
library(dfoptim)    #https://stackoverflow.com/questions/11387330/errors-when-attempting-constrained-optimisation-using-optim

##Generate population density
#average population density in US: 283 per mile^2 (http://css.umich.edu/factsheets/us-cities-factsheet)

result <- read.csv("G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/scenario2(-0.03)_density_negNumDensityCor_results_MAP_0MeanPrior_noConst.csv")
par <- result$para_data

plot.ts(result$V9)

par <- colMeans(result[500:1500,])
#apply(result[1000:2000,],2,median)
all_par <- matrix(NA,2000,9)
for (i in 1:2000) {
  all_par[i,] <- colMeans(result[i,])
}

file_names <- c("scenario2(-0.03)_negNumDensityCor.csv")
                #"SR_LU","SU_LU","LU09_LU"
                #"scenario_0.1_0.1","baseline_unif","baseline_beta_left","baseline_beta_right","scen1_unif","scen1_beta_left","scen1_beta_right","scen2_0.1_unif","scen2_0.1_beta_left",
                #"scen2_0.5_unif","scen2_0.5_beta_left","scen2_0.9_unif","scen2_0.9_beta_left",)

#a_file <- file_names[1]
for (a_file in file_names) {
  
  data_baseline <- read.csv(paste('G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/', a_file, sep=""))
  
  pop_densities_unique <- sort(unique(data_baseline$trip_density))
  obs_zones_counts <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==1 | data_baseline$capture_2==1) & data_baseline$trip_density==x)))
  
  ###For 4-category model
  obs_1_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==1 & data_baseline$capture_2==0) & data_baseline$trip_density==x)))
  obs_2_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==0 & data_baseline$capture_2==1) & data_baseline$trip_density==x)))
  obs_12_zones_count <- sapply(pop_densities_unique, function(x) length(which((data_baseline$capture_1==1 & data_baseline$capture_2==1) & data_baseline$trip_density==x)))
  
  ###For N-mixture model
  obs_1_zones_count_N_mix <- sapply(pop_densities_unique, function(x) length(which(data_baseline$capture_1==1 & data_baseline$trip_density==x)))
  obs_2_zones_count_N_mix <- sapply(pop_densities_unique, function(x) length(which(data_baseline$capture_2==1 & data_baseline$trip_density==x)))
  
  obs_1_zones_count <- round(obs_1_zones_count)
  obs_2_zones_count <- round(obs_2_zones_count)
  obs_12_zones_count <- round(obs_12_zones_count)
  
  ###Zone-level income
  income_zone <- sapply(pop_densities_unique, function(x) median(unique(data_baseline$trip_income[which(data_baseline$trip_density==x)])))
  
  ground_truth <- sapply(pop_densities_unique, function(x) length(which(data_baseline$trip_density==x)))
  obs_index <- which(data_baseline$capture_1==1 | data_baseline$capture_2==1)
  
  pop_densities_unique <- pop_densities_unique/sd(pop_densities_unique)
  income_zone <- income_zone/sd(income_zone)
  
  fit_lm <- lm(log(ground_truth) ~ pop_densities_unique +income_zone)
  fit_1_lm <- glm(data_baseline$capture_1 ~ data_baseline$trip_density + data_baseline$trip_income,family=binomial) #glm(data_baseline$capture_1_new~data_baseline$trip_density+data_baseline$trip_income,family=binomial)
  fit_2_lm <- glm(data_baseline$capture_2 ~ data_baseline$trip_density + data_baseline$trip_income,family=binomial)
  
  obs_1_zones_count_copy <- obs_1_zones_count
  obs_2_zones_count_copy <- obs_2_zones_count
  obs_12_zones_count_copy <- obs_12_zones_count
  obs_1_zones_count_copy[which(obs_1_zones_count_copy == 0)] <- 1
  obs_2_zones_count_copy[which(obs_2_zones_count_copy == 0)] <- 1
  obs_12_zones_count_copy[which(obs_12_zones_count_copy == 0)] <- 1
  # Proof: solve for N in the system of equations
  # p1*p2 = a/N
  # p1(1-p2) = b/N
  # p2*(1-p1) = c/N
  prior <- (obs_12_zones_count_copy + obs_1_zones_count_copy)*(obs_12_zones_count_copy + obs_2_zones_count_copy)/obs_12_zones_count_copy
  prior <- obs_zones_counts*2  ######### 3, 2
  prior[prior == 0] <- 2
  
  fit_lm_obs <- lm(log(prior) ~ pop_densities_unique +income_zone)
  fit_1_lm_obs <- lm(obs_1_zones_count_N_mix/obs_zones_counts ~ pop_densities_unique + income_zone) #glm(data_baseline$capture_1_new~data_baseline$trip_density+data_baseline$trip_income,family=binomial)
  fit_2_lm_obs <- lm(obs_2_zones_count_N_mix/obs_zones_counts ~ pop_densities_unique + income_zone)
  
  std_err <- sqrt(10) * c(summary(fit_1_lm_obs)$coefficients[,2], summary(fit_2_lm_obs)$coefficients[,2], summary(fit_lm_obs)$coefficients[,2])
  avg <- c(fit_1_lm_obs$coefficients,fit_2_lm_obs$coefficients,fit_lm_obs$coefficients)
  
  prior_1 <- obs_zones_counts
  prior_1[prior_1 == 0] <- 1
  fit_lm_obs_1 <- lm(log(prior_1) ~ pop_densities_unique +income_zone)
  
  std_err_prior <- sqrt(10)*c(summary(fit_1_lm_obs)$coefficients[,2], summary(fit_2_lm_obs)$coefficients[,2], summary(fit_lm_obs_1)$coefficients[,2])
  avg_prior <- c(fit_1_lm_obs$coefficients,fit_2_lm_obs$coefficients,fit_lm_obs_1$coefficients)
  
  likelihood_function <- function(par) {
    #set.seed(1)
    
    utility_zones_1 <- par[1] + par[2]*pop_densities_unique + par[3]*income_zone
    utility_zones_2 <- par[4] + par[5]*pop_densities_unique + par[6]*income_zone
    p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
    p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
    # inf_count <- length(which(p_zones_1 > 1-1e-10)) + length(which(p_zones_2 > 1-1e-10)) + length(which(p_zones_1 < 1e-10)) + length(which(p_zones_2 < 1e-10))
    # p_zones_1[which(p_zones_1 > 1-1e-10)] <- 1-1e-10
    # p_zones_2[which(p_zones_2 > 1-1e-10)] <- 1-1e-10
    # p_zones_1[which(p_zones_1 < 1e-10)] <- 1e-10
    # p_zones_2[which(p_zones_2 < 1e-10)] <- 1e-10
    
    lambda_a <- exp(par[7] + par[8]*pop_densities_unique + par[9]*income_zone)  #obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)#rpois(1,lambda_a)
    #lambda_a <- obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)
    # poisson_numbers <- t(sapply(1:2000, function(x) rpois(5,lambda_a)))
    poisson_upper_bound <- qpois(0.9999999,lambda_a)#rep(5000,5)#
    
    #cat(par,file = "C:/Users/guanxy/Documents/logfile_paper_0.10_density_lambda10_MAP.txt", append=T,fill=T)
    #cat(lambda_a,file = "C:/Users/guanxy/Documents/logfile_paper_0.10_density_lambda10_MAP.txt", append=T,fill=T)
    # cat(inf_count,file = "C:/Users/guanxy/Documents/logfile_server_9.txt", append=T,fill=T)
    
    if (max(poisson_upper_bound) > 30000) return(Inf)#max(poisson_upper_bound))    #######
    if (max(lambda_a < obs_zones_counts) == 1) return(Inf)#30000)                    #######
    
    all_likelihoods <- c()
    for (i in 1:10) {
      likelihoods <- c()
      #likelihoods <- foreach(j = obs_zones_counts[i]:poisson_upper_bound[i], .combine="c", .export=c("obs_zones_counts","obs_1_zones_count","obs_2_zones_count","obs_12_zones_count")) %dopar% {
      for (j in obs_zones_counts[i]:poisson_upper_bound[i]) {
        obs_0_zones_count <- j - obs_zones_counts[i]
        n_a_zones <- j
        
        # likelihood_0 <- obs_0_zones_count*log((1 - p_zones_1)*(1 - p_zones_2))
        # likelihood_1 <- obs_1_zones_count*log(p_zones_1*(1 - p_zones_2))
        # likelihood_2 <- obs_2_zones_count*log((1-p_zones_1)*p_zones_2)
        # likelihood_12 <- obs_12_zones_count*log(p_zones_1*p_zones_2)
        
        obs_0_zones_count <- round(obs_0_zones_count)
        n_a_zones <- round(n_a_zones)
        
        ### For multinomial model
        log_likelihood_zones <- dmultinom(c(obs_0_zones_count,obs_1_zones_count[i],obs_2_zones_count[i],obs_12_zones_count[i]),size=n_a_zones,
                                          prob=c((1 - p_zones_1[i])*(1 - p_zones_2[i]),
                                                 p_zones_1[i]*(1 - p_zones_2[i]),
                                                 (1-p_zones_1[i])*p_zones_2[i],
                                                 p_zones_1[i]*p_zones_2[i]), log=F)
        ### For N-mixture model
        # log_likelihood_zones_1 <- dbinom(obs_1_zones_count[i], n_a_zones, p_zones_1[i])
        # log_likelihood_zones_2 <- dbinom(obs_2_zones_count[i], n_a_zones, p_zones_2[i])
        # log_likelihood_zones <- log_likelihood_zones_1*log_likelihood_zones_2
        
        likelihoods <- c(likelihoods, log_likelihood_zones * dpois(j,lambda_a[i]))
      }
      #}
      
      # LL_max <- max(likelihoods)
      # LL <- LL_max + log(sum(exp(likelihoods-LL_max)))
      
      likelihood_one_zone <- sum(likelihoods)
      
      all_likelihoods <- c(all_likelihoods, likelihood_one_zone)#LL) #
    }
    
    log_likelihood_output <- sum(log(all_likelihoods)) # sum(all_likelihoods) #
    
    #if (is.infinite(log_likelihood)) log_likelihood <- -100000
    
    #cat(log_likelihood_output,file = "C:/Users/guanxy/Documents/logfile_paper_0.10_density_lambda10_MAP.txt", append=T,fill=T)
    
    ridged_LL <- -log_likelihood_output - sum(dnorm(par[c(2:3,5:6)],0,std_err_prior[c(2:3,5:6)],log=T))#+ 10*sum(par[c(2:3,5:6)]^2)    #########
    
    return(ridged_LL)    #########
  }
  
  LLs <- c()
  for (i in 1:2000) {
    par_temp <- all_par[i,]
    L <- -likelihood_function(par_temp)
    LLs <- c(LLs, L)
  }
  
  par <- all_par[which(LLs == max(LLs)),]
  lambda_a <- exp(par[7] + par[8]*pop_densities_unique + par[9]*income_zone)
  lambda_a/ground_truth
  
  plot.ts(LLs, ylab="Log-likelihood",xlab="Iterations")
  write.csv(LLs, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/LLs/0.03_density.csv")
  
  #lambda_a[1:2] <- lambda_a[1:2]/10
  plot(obs_zones_counts*100/ground_truth, lambda_a*100/ground_truth, xlab="Percentage of observed trips over ground truth", ylab="Percentage of inferred trips over ground truth")
  abline(h=100,col="red",lty=2)
  output <- cbind(obs_zones_counts*100/ground_truth, lambda_a*100/ground_truth)
  write.csv(output, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/LLs/percentages_0.03_density.csv",row.names=F)
  
  max_index <- which(LLs == max(LLs))
  colMeans(result[max_index:(max_index+99),])
  apply(result[max_index:(max_index+99),], 2, sd)
  
  truth <- c(fit_1_lm$coefficients, fit_2_lm$coefficients, fit_lm$coefficients)
  
  
  
  stopCluster(cl)
  
  ###MLE --------------------------------------------------------------------------------------------------------------
  #-21.06326007,0.06178814,rep(-4.83395080,5)
  max_obs <- max(obs_zones_counts)
  
  estim_optim <- spg(c(fit_1_lm$coefficients, fit_2_lm$coefficients, fit_lm$coefficients),likelihood_function,lower=c(rep(-Inf,9)),upper = c(rep(Inf,9)),
               control = list(maxit = 300000, trace = TRUE, maxfeval=500000))    #, eps=0.0000001
  estim_optim <- optim(c(fit_1_lm$coefficients, fit_2_lm$coefficients, fit_lm$coefficients),likelihood_function, method="L-BFGS-B", lower = rep(-Inf,9), upper = rep(Inf,9), #hessian=T,
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
  write.csv(results,paste("G:/My Drive/trip bias paper/scenarios/representativeness/more_zones/inference_results/",a_file,"_multinom_no_income_indiv.csv",sep=""),row.names=F)
}
