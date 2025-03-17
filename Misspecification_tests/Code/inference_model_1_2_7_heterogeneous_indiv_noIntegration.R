###similar as inference_model_1_2_6.R in "zone_homogenity", but with individual level income data (used in data generation but not in inference model).
###multinomial likelihood with the three observed categories replace by individual-level likelihood. The missing category is still zone level.
##compare zone likelihood vs individual likelihood

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
  fit_1_lm <- glm(data_baseline$capture_1_new~data_baseline$trip_density + data_baseline$trip_income,family=binomial) #glm(data_baseline$capture_1_new~data_baseline$trip_density+data_baseline$trip_income,family=binomial)
  fit_2_lm <- glm(data_baseline$capture_2_new~data_baseline$trip_density + data_baseline$trip_income,family=binomial)
  
  likelihood_function <- function(par) {
    #set.seed(1)
  
    utility_trips_1 <- par[1] + par[2]*data_baseline$trip_density_new + par[3]*data_baseline$trip_income_new
    utility_trips_2 <- par[4] + par[5]*data_baseline$trip_density_new + par[6]*data_baseline$trip_income_new
    p_trips_1 <- 1 - 1/(1+exp(utility_trips_1))
    p_trips_2 <- 1 - 1/(1+exp(utility_trips_2))
    inf_count_trip <- length(which(p_trips_1 > 1-1e-10)) + length(which(p_trips_2 > 1-1e-10)) + length(which(p_trips_1 < 1e-10)) + length(which(p_trips_2 < 1e-10))
    p_trips_1[which(p_trips_1 > 1-1e-10)] <- 1-1e-10
    p_trips_2[which(p_trips_2 > 1-1e-10)] <- 1-1e-10
    p_trips_1[which(p_trips_1 < 1e-10)] <- 1e-10
    p_trips_2[which(p_trips_2 < 1e-10)] <- 1e-10
    
    LL_0_for_each_zone <- sapply(pop_densities_unique, function(x) sum(log(1-p_trips_1[which(data_baseline$capture_1_new==0 & data_baseline$capture_2_new==0 & data_baseline$trip_density_new==x)]) + log(1-p_trips_2[which(data_baseline$capture_1_new==0 & data_baseline$capture_2_new==0 & data_baseline$trip_density_new==x)])))
    LL_1_for_each_zone <- sapply(pop_densities_unique, function(x) sum(log(p_trips_1[which(data_baseline$capture_1_new==1 & data_baseline$capture_2_new==0 & data_baseline$trip_density_new==x)]) + log(1-p_trips_2[which(data_baseline$capture_1_new==1 & data_baseline$capture_2_new==0 & data_baseline$trip_density_new==x)])))
    LL_12_for_each_zone <- sapply(pop_densities_unique, function(x) sum(log(p_trips_1[which(data_baseline$capture_1_new==1 & data_baseline$capture_2_new==1 & data_baseline$trip_density_new==x)]) + log(p_trips_2[which(data_baseline$capture_1_new==1 & data_baseline$capture_2_new==1 & data_baseline$trip_density_new==x)])))
    LL_2_for_each_zone <- sapply(pop_densities_unique, function(x) sum(log(1-p_trips_1[which(data_baseline$capture_1_new==0 & data_baseline$capture_2_new==1 & data_baseline$trip_density_new==x)]) + log(p_trips_2[which(data_baseline$capture_1_new==0 & data_baseline$capture_2_new==1 & data_baseline$trip_density_new==x)])))
  
    utility_zones_1 <- par[1] + par[2]*pop_densities_unique + par[3]*income_zone
    utility_zones_2 <- par[4] + par[5]*pop_densities_unique + par[6]*income_zone
    p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
    p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
    inf_count <- length(which(p_zones_1 > 1-1e-10)) + length(which(p_zones_2 > 1-1e-10)) + length(which(p_zones_1 < 1e-10)) + length(which(p_zones_2 < 1e-10))
    p_zones_1[which(p_zones_1 > 1-1e-10)] <- 1-1e-10
    p_zones_2[which(p_zones_2 > 1-1e-10)] <- 1-1e-10
    p_zones_1[which(p_zones_1 < 1e-10)] <- 1e-10
    p_zones_2[which(p_zones_2 < 1e-10)] <- 1e-10
    
    lambda_a <- exp(par[7] + par[8]*pop_densities_unique + par[9]*income_zone)  #obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2) ##rpois(1,lambda_a)
    # poisson_numbers <- t(sapply(1:2000, function(x) rpois(5,lambda_a)))
    # poisson_upper_bound <- qpois(0.9999999,lambda_a)#rep(5000,5)#

    cat(par,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    cat(lambda_a,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    cat(inf_count+inf_count_trip,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    
    # if (max(poisson_upper_bound) > 30000) return(-max(poisson_upper_bound))
    # if (max(lambda_a < obs_zones_counts) == 1) return(-30000)
    
    obs_0_zones_count <- lambda_a - obs_zones_counts
    LL_0_infer <- obs_0_zones_count*(log(1 - p_zones_1) + log(1 - p_zones_2))
    combination_term_log <- lfactorial(lambda_a) - lfactorial(obs_0_zones_count) - lfactorial(obs_zones_counts)#lfactorial(obs_1_zones_count[i]) - lfactorial(obs_2_zones_count[i]) - lfactorial(obs_12_zones_count[i])
    
    LL_1 <- obs_1_zones_count*(log(p_zones_1) + log(1 - p_zones_2))
    LL_2 <- obs_2_zones_count*(log(1-p_zones_1) + log(p_zones_2))
    LL_12 <- obs_12_zones_count*(log(p_zones_1) + log(p_zones_2))
    
    log_likelihood_output <- sum(combination_term_log) + sum(LL_0_infer) + sum(LL_1) + sum(LL_2) + sum(LL_12)
    
    #if (is.infinite(log_likelihood)) log_likelihood <- -100000
    
    cat(log_likelihood_output,file = "C:/Users/guanxy/Documents/logfile_server_11.txt", append=T,fill=T)
    return(-log_likelihood_output)    #########
  }
  
  # likelihood_helper <- function(optim_para, other_paras, index) {
  #   para_current <- other_paras
  #   para_current[index] <- optim_para
  #   return(-likelihood_function(para_current))
  # }
  # 
  # set.seed(1)
  # gibbs_samples <- matrix(NA,1000,9)    #alpha_1, alpha_2, error_1, error_2, n_a
  # #colnames(gibbs_samples) <- c("alpha_1","alpha_2","n_a")
  # # gibbs_samples[1,1] <- rnorm(1,0,1) #rnorm(1,-4.652783*0.9,0.01)
  # # gibbs_samples[1,2] <- rnorm(1,0,0.01) #rnorm(1,0.003998*0.9,0.01)
  # # gibbs_samples[1,3] <- rnorm(1,0,0.01) #rnorm(1,-0.002403*0.9,0.01)
  # # gibbs_samples[1,4] <- rnorm(1,0,1) #rnorm(1,-4.967978*0.9,0.01)
  # # gibbs_samples[1,5] <- rnorm(1,0,0.01) #rnorm(1,0.062323*0.9,0.01)
  # # gibbs_samples[1,6] <- rnorm(1,0,0.01) #rnorm(1,0.015512*0.9,0.01)
  # # gibbs_samples[1,7] <- rnorm(1,0,1) #rnorm(1,5.85363*0.9,0.01)
  # # gibbs_samples[1,8] <- rnorm(1,0,0.01) #rnorm(1,0.17056*0.9,0.01)
  # # gibbs_samples[1,9] <- rnorm(1,0,0.01) #rnorm(1,-0.03260*0.9,0.01)
  # gibbs_samples[1,] <- c(-0.537773467019924,0.0912289941366288,0.0810362747703075,1.6846733630322,0.0926876386125616,0.0811878770532278,0.576821613322893,0.0967758079457001,0.108581534003104)
  # 
  # sds <- c(5,0.01,0.01,5,0.1,0.1,5,1,0.1)
  # 
  # for (i in 2:1000) {
  #   #clusterExport(cl, "i")
  # 
  #   for (j in 1:9) {
  #     # para <- matrix(NA,25,9)
  #     # para[,1:j] <- matrix(gibbs_samples[i,1:j],nrow=25,ncol=j,byrow=T)
  #     # para[,j:9] <- matrix(gibbs_samples[i-1,j:9],nrow=25,ncol=10-j,byrow=T)
  #     # para[,j] <- rnorm(25,gibbs_samples[i-1,j],sds[j])
  #     # lhs <- sapply(1:25, function(x) likelihood_function(para[x,]))
  #     # lhs_max <- max(lhs,na.rm=T)
  #     # lhs_down <- exp(lhs - lhs_max)
  #     # lhs_scale <- lhs_down/sum(lhs_down)
  #     # #gibbs_samples[i,j] <- sample(para[,j],1,prob=lhs_scale)
  #     # gibbs_samples[i,j] <- sum(para[,j]*lhs_scale)
  #     
  #     ### reject sampling: http://ericfrazerlock.com/Rejection_Sampling.pdf
  #     para <- rep(NA,9)
  #     para[1:j] <- gibbs_samples[i,1:j]
  #     para[j:9] <- gibbs_samples[i-1,j:9]
  #     mle <- spg(para[j], likelihood_helper, other_paras=para, index=j, control = list(maxit = 300000, trace = TRUE, maxfeval=500000))
  #     max_likelihood <- -mle$value  # M value
  #     
  #     theta <- rnorm(1, gibbs_samples[i-1,j], sds[j])
  #     U <- runif(1,0,1)
  #     para[j] <- theta
  #     LL_temp <- likelihood_function(para)
  #     if (is.na(LL_temp) | is.nan(LL_temp)) LL_temp <- -99999
  #     while (max_likelihood + log(U) >= LL_temp) {
  #       theta <- rnorm(1, gibbs_samples[i-1,j], sds[j])
  #       U <- runif(1,0,1)
  #       para[j] <- theta
  #       LL_temp <- likelihood_function(para)
  #       if (is.na(LL_temp) | is.nan(LL_temp)) LL_temp <- -99999
  #     }
  #     gibbs_samples[i,j] <- theta
  #   }
  # 
  #   write.csv(gibbs_samples,"G:/My Drive/trip bias paper/scenarios/representativeness/inference_results/gibbs_samples_multinomial_individual_reject_sampling.csv",row.names = F)
  # }

  # stopCluster(cl)
  
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
# -3.639081 -0.01698305 -0.01290838 -0.4577075 -0.02631083 -0.06024199 1.112949 0.23773 0.06021872
# -4.542667 -0.37567 -0.02971032 -5.165259 -0.4617249 0.1091541 5.858954 0.6522405 -0.09730277