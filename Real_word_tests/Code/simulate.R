###Similar to "inference_model_1_2_7_heterogeneous_indiv.R"

## Pay attention to the converging criteria factr=1e7
## Use the parscale parameter wisely
## when calculating initial para values, use obs_zones_counts+100 instead of obs_zones_counts
library(foreach)
library(doParallel)
no_cores <- 20
#registerDoParallel(makeCluster(no_cores))
cl <- makeCluster(no_cores)
registerDoParallel(cl)
library(BB)
library(numDeriv)    #calculate gradient
library(dfoptim)    #https://stackoverflow.com/questions/11387330/errors-when-attempting-constrained-optimisation-using-optim

##Generate population density
#average population density in US: 283 per mile^2 (http://css.umich.edu/factsheets/us-cities-factsheet)

result <- read.csv("C:/Users/USXG713181/OneDrive/Google drive/trip bias paper/scenarios/real_world/real_world_results_reduced.csv")
result <- as.matrix(result)
estimates <- c()
likelihoods <- c()
for (i in 1:nrow(result)) {
  par <- result[i,]
  lambda_a <- sum(exp(par[15] + par[16]*income_zone +  + par[17]*pop_densities_unique + par[18]*population_unique_norm + par[19]*renter_HH_norm + par[20]*owner_HH_norm + par[21]*empoyment_norm))
  #ll <- likelihood_function(par)
  estimates <- c(estimates, lambda_a)
  #likelihoods <- c(likelihoods, ll)
}

plot.ts(estimates)
plot.ts(result[,9])

# 3000:4000
# 500:749
par_final <- colMeans(result[2000:2500,])
par_std <- apply(result[2000:2500,], 2, sd)
t_value <- par_final/par_std/sqrt(249)
estimates_final <- exp(par_final[15] + par_final[16]*income_zone +  + par_final[17]*pop_densities_unique + par_final[18]*population_unique_norm + par_final[19]*renter_HH_norm + par_final[20]*owner_HH_norm + par_final[21]*empoyment_norm)
#pop_densities_unique <- sort(unique(data_baseline$pop_density))
#neighborhood_name <- sapply(pop_densities_unique, function(x) data_baseline$L_HOOD[which(data_baseline$pop_density==x)][1])
output_data <- cbind(obs_1_zones_count, obs_2_zones_count, obs_12_zones_count, estimates_final, income_zone, pop_densities_unique, population_unique)

write.csv(output_data, "C:/Users/USXG713181/OneDrive/Google drive/trip bias paper/scenarios/real_world/output_1.csv", row.names = F)

file_names <- c("trip_data_final.csv")
                #"SR_LU","SU_LU","LU09_LU"
                #"scenario_0.1_0.1","baseline_unif","baseline_beta_left","baseline_beta_right","scen1_unif","scen1_beta_left","scen1_beta_right","scen2_0.1_unif","scen2_0.1_beta_left",
                #"scen2_0.5_unif","scen2_0.5_beta_left","scen2_0.9_unif","scen2_0.9_beta_left",)
## Date range: Feb, Mar, Apr, Oct, Nov (150 days)

a_file <- file_names[1]
#for (a_file in file_names) {
  
  data_baseline <- read.csv(paste('Z:/projects/Caltrans/Bias correction/processing/', a_file, sep=""))
  
  # data_baseline$trip_density <- (data_baseline$trip_density - mean(unique(data_baseline$trip_density)))/sd(unique(data_baseline$trip_density))
  # data_baseline$trip_income <- (data_baseline$trip_income - mean(unique(data_baseline$trip_income)))/sd(unique(data_baseline$trip_income))
  
  GEOID_unique <- unique(data_baseline$GEOID_CBG)
  
  pop_densities_unique <- sapply(GEOID_unique, function(x) mean(data_baseline$pop_density[which(data_baseline$GEOID_CBG==x)]))
  #pop_densities_unique <- pop_densities_unique[2]   ##test onky one zone
  
  population_unique <- sapply(GEOID_unique, function(x) mean(data_baseline$pop[which(data_baseline$GEOID_CBG==x)]))
  adult_pop <- sapply(GEOID_unique, function(x) mean(data_baseline$adult_pop[which(data_baseline$GEOID_CBG==x)]))
  
  obs_zones_counts <- sapply(GEOID_unique, function(x) length(which((data_baseline$gps==1 | data_baseline$cell==1) & data_baseline$GEOID_CBG==x)))
  
  ###For 4-category model
  obs_1_zones_count <- sapply(GEOID_unique, function(x) length(which((data_baseline$gps==1 & data_baseline$cell==0) & data_baseline$GEOID_CBG==x)))
  obs_2_zones_count <- sapply(GEOID_unique, function(x) length(which((data_baseline$gps==0 & data_baseline$cell==1) & data_baseline$GEOID_CBG==x)))
  obs_12_zones_count <- sapply(GEOID_unique, function(x) length(which((data_baseline$gps==1 & data_baseline$cell==1) & data_baseline$GEOID_CBG==x)))
  
  ###For N-mixture model
  obs_1_zones_count_N_mix <- sapply(GEOID_unique, function(x) length(which(data_baseline$gps==1 & data_baseline$GEOID_CBG==x)))
  obs_2_zones_count_N_mix <- sapply(GEOID_unique, function(x) length(which(data_baseline$cell==1 & data_baseline$GEOID_CBG==x)))
  
  ### 5 months of data
  obs_1_zones_count <- round(obs_1_zones_count)
  obs_2_zones_count <- round(obs_2_zones_count)
  obs_12_zones_count <- round(obs_12_zones_count)
  
  obs_zones_counts <- obs_1_zones_count + obs_2_zones_count + obs_12_zones_count
  
  ###Zone-level income
  income_zone <- sapply(GEOID_unique, function(x) median(unique(data_baseline$income[which(data_baseline$GEOID_CBG==x)])))
  renter_HH <- sapply(GEOID_unique, function(x) median(unique(data_baseline$renter_HH[which(data_baseline$GEOID_CBG==x)])))
  owner_HH <- sapply(GEOID_unique, function(x) median(unique(data_baseline$owner_HH[which(data_baseline$GEOID_CBG==x)])))
  empoyment <- sapply(GEOID_unique, function(x) median(unique(data_baseline$employment[which(data_baseline$GEOID_CBG==x)])))
  
  ###Ground truth not available in real-world case.
  #ground_truth <- sapply(pop_densities_unique, function(x) length(which(data_baseline$pop_density==x)))
  #obs_index <- which(data_baseline$capture_1==1 | data_baseline$capture_2==1)
  
  pop_densities_unique <- (pop_densities_unique - mean(pop_densities_unique))/sd(pop_densities_unique)
  income_zone <- (income_zone - mean(income_zone))/sd(income_zone)
  population_unique_norm <- (population_unique - mean(population_unique))/sd(population_unique)
  renter_HH_norm <- (renter_HH - mean(renter_HH))/sd(renter_HH)
  owner_HH_norm <- (owner_HH - mean(owner_HH))/sd(owner_HH)
  empoyment_norm <- (empoyment - mean(empoyment))/sd(empoyment)
  
  #fit_lm <- lm(log(ground_truth) ~ pop_densities_unique +income_zone)
  #fit_1_lm <- glm(data_baseline$capture_1 ~ data_baseline$trip_density + data_baseline$trip_income,family=binomial) #glm(data_baseline$capture_1_new~data_baseline$trip_density+data_baseline$trip_income,family=binomial)
  #fit_2_lm <- glm(data_baseline$capture_2 ~ data_baseline$trip_density + data_baseline$trip_income,family=binomial)
  
  #par_ground_truth <- c(fit_1_lm$coefficients,fit_2_lm$coefficients,fit_lm$coefficients)
  #par_ground_truth[7:9] <- c(2.06,-0.07614198,0)
  
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
  prior <- population_unique  #(obs_12_zones_count_copy + obs_1_zones_count_copy)*(obs_12_zones_count_copy + obs_2_zones_count_copy)/obs_12_zones_count_copy
  # prior <- obs_zones_counts*2  ######### 3, 2
  # prior[prior == 0] <- 2
  
  p_1 <- sapply(obs_1_zones_count_N_mix/prior, function(x) min(x, 0.999999))
  p_2 <- sapply(obs_2_zones_count_N_mix/prior, function(x) min(x, 0.999999))
  p_1[which(p_1 == 0)] <- 1e-10
  p_2[which(p_2 == 0)] <- 1e-10
  logit_1 <- log(p_1/(1 - p_1))
  logit_2 <- log(p_2/(1 - p_2))
  fit_lm_obs <- lm(log(prior) ~ income_zone+pop_densities_unique+population_unique_norm+renter_HH_norm+owner_HH_norm+empoyment_norm)
  fit_1_lm_obs <- lm(logit_1 ~ income_zone+pop_densities_unique+population_unique_norm+renter_HH_norm+owner_HH_norm+empoyment_norm) #glm(data_baseline$capture_1_new~data_baseline$trip_density+data_baseline$trip_income,family=binomial)
  fit_2_lm_obs <- lm(logit_2 ~ income_zone+pop_densities_unique+population_unique_norm+renter_HH_norm+owner_HH_norm+empoyment_norm)
  
  std_err <- sqrt(12)*c(summary(fit_1_lm_obs)$coefficients[,2], summary(fit_2_lm_obs)$coefficients[,2], summary(fit_lm_obs)$coefficients[,2])
  avg <- c(fit_1_lm_obs$coefficients,fit_2_lm_obs$coefficients,fit_lm_obs$coefficients)
  
  # prior_1 <- obs_zones_counts
  # prior_1[prior_1 == 0] <- 1
  # fit_lm_obs_1 <- lm(log(prior_1) ~ pop_densities_unique +income_zone)
  
  # std_err_prior <- sqrt(10)*c(summary(fit_1_lm_obs)$coefficients[,2], summary(fit_2_lm_obs)$coefficients[,2], summary(fit_lm_obs_1)$coefficients[,2])
  # avg_prior <- c(fit_1_lm_obs$coefficients,fit_2_lm_obs$coefficients,fit_lm_obs_1$coefficients)
  
  # std_err_prior <- std_err
  avg_prior <- avg
  std_err_prior <- std_err/100 #rep(0.001, 9)  
  #std_err <- rep(1, 9)
  
  likelihood_function <- function(par) {
    #set.seed(1)
    
    # utility_trips_1 <- par[1] + par[2]*data_baseline$trip_density + par[3]*data_baseline$trip_income
    # utility_trips_2 <- par[4] + par[5]*data_baseline$trip_density + par[6]*data_baseline$trip_income
    # p_trips_1 <- 1 - 1/(1+exp(utility_trips_1))
    # p_trips_2 <- 1 - 1/(1+exp(utility_trips_2))
    # 
    # LL_1_for_each_zone <- sapply(pop_densities_unique, function(x) sum(log(p_trips_1[which(data_baseline$capture_1==1 & data_baseline$capture_2==0 & data_baseline$trip_density==x)]) + log(1-p_trips_2[which(data_baseline$capture_1==1 & data_baseline$capture_2==0 & data_baseline$trip_density==x)])))
    # LL_12_for_each_zone <- sapply(pop_densities_unique, function(x) sum(log(p_trips_1[which(data_baseline$capture_1==1 & data_baseline$capture_2==1 & data_baseline$trip_density==x)]) + log(p_trips_2[which(data_baseline$capture_1==1 & data_baseline$capture_2==1 & data_baseline$trip_density==x)])))
    # LL_2_for_each_zone <- sapply(pop_densities_unique, function(x) sum(log(1-p_trips_1[which(data_baseline$capture_1==0 & data_baseline$capture_2==1 & data_baseline$trip_density==x)]) + log(p_trips_2[which(data_baseline$capture_1==0 & data_baseline$capture_2==1 & data_baseline$trip_density==x)])))
    
  
    utility_zones_1 <- par[1] + par[2]*income_zone + par[3]*pop_densities_unique
    p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
    utility_zones_2_given1 <- par[4] + par[5]*income_zone + par[6]*pop_densities_unique + par[10]*p_zones_1
    utility_zones_2_givenNot1 <- par[4] + par[5]*income_zone + par[6]*pop_densities_unique - par[10]*(1-p_zones_1)
    p_zones_2_given1 <- 1 - 1/(1+exp(utility_zones_2_given1))
    p_zones_2_givenNot1 <- 1 - 1/(1+exp(utility_zones_2_givenNot1))
    # inf_count <- length(which(p_zones_1 > 1-1e-10)) + length(which(p_zones_2 > 1-1e-10)) + length(which(p_zones_1 < 1e-10)) + length(which(p_zones_2 < 1e-10))
    # p_zones_1[which(p_zones_1 > 1-1e-10)] <- 1-1e-10
    # p_zones_2[which(p_zones_2 > 1-1e-10)] <- 1-1e-10
    # p_zones_1[which(p_zones_1 < 1e-10)] <- 1e-10
    # p_zones_2[which(p_zones_2 < 1e-10)] <- 1e-10
    p_zones_1and2 <- p_zones_1*p_zones_2_given1
    p_zones_1not2 <- p_zones_1*(1-p_zones_2_given1)
    p_zones_2not1 <- (1-p_zones_1)*p_zones_2_givenNot1
    p_zones_not1Not2 <- (1-p_zones_1)*(1-p_zones_2_givenNot1)
    
    lambda_a <- exp(par[7] + par[8]*income_zone + par[9]*pop_densities_unique)  #obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)#rpois(1,lambda_a)
    # poisson_numbers <- t(sapply(1:2000, function(x) rpois(5,lambda_a)))
    poisson_upper_bound <- qpois(0.99999999,lambda_a)#rep(5000,5)#

    # cat(par,file = "C:/Users/guanxy/Documents/new_logfile_0.10_density.txt", append=T,fill=T)
    # cat(lambda_a,file = "C:/Users/guanxy/Documents/new_logfile_0.10_density.txt", append=T,fill=T)
    # cat(inf_count,file = "C:/Users/guanxy/Documents/logfile_server_9.txt", append=T,fill=T)
    
    if (max(poisson_upper_bound) > 250000) return(Inf)#max(poisson_upper_bound))    #######
    if (max(lambda_a < obs_zones_counts) == 1) return(Inf)#30000)                    #######

    #all_likelihoods <- c()
    all_likelihoods <- foreach(i=1:20, .combine="c", .export=c("obs_zones_counts","obs_1_zones_count","obs_2_zones_count","obs_12_zones_count")) %dopar% {
    #for (i in 1:20) {     ##############
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
                                                                  prob=c(p_zones_not1Not2[i],
                                                                         p_zones_1not2[i],
                                                                         p_zones_2not1[i],
                                                                         p_zones_1and2[i]), log=F)
        
        ### For conditional likelihood
        # log_likelihood_zones <- dbinom(obs_0_zones_count,size=n_a_zones,(1 - p_zones_1[i])*(1 - p_zones_2[i]), log=F)
        
        ### For N-mixture model
        # log_likelihood_zones_1 <- dbinom(obs_1_zones_count[i], n_a_zones, p_zones_1[i])
        # log_likelihood_zones_2 <- dbinom(obs_2_zones_count[i], n_a_zones, p_zones_2[i])
        # log_likelihood_zones <- log_likelihood_zones_1*log_likelihood_zones_2
        
        ### For individual-level model
        # combination_term_log <- lfactorial(n_a_zones) - lfactorial(obs_0_zones_count) - lfactorial(obs_1_zones_count[i]) - lfactorial(obs_2_zones_count[i]) - lfactorial(obs_12_zones_count[i])
        # LL_0 <- obs_0_zones_count*(log(1 - p_zones_1[i]) + log(1 - p_zones_2[i]))
        # LL_1 <- LL_1_for_each_zone[i] 
        # LL_2 <- LL_2_for_each_zone[i]
        # LL_12 <- LL_12_for_each_zone[i]
        # log_likelihood_zones <- combination_term_log + LL_0 + LL_1 + LL_2 + LL_12
        # log_likelihood_zones <- exp(log_likelihood_zones)
        
        #one_likelihood <- log_likelihood_zones * dpois(j,lambda_a[i])
        #one_likelihood
        likelihoods <- c(likelihoods, log_likelihood_zones * dpois(j,lambda_a[i]))
      }
      #}
      
      # LL_max <- max(likelihoods)
      # LL <- LL_max + log(sum(exp(likelihoods-LL_max)))
      
      likelihood_one_zone <- sum(likelihoods)
      
      #all_likelihoods <- c(all_likelihoods, likelihood_one_zone)#LL) #
      likelihood_one_zone
    }
    
    log_likelihood_output <- sum(log(all_likelihoods)) # sum(all_likelihoods) #
    
    #if (is.infinite(log_likelihood)) log_likelihood <- -100000
    
    # cat(log_likelihood_output,file = "C:/Users/guanxy/Documents/new_logfile_0.10_density.txt", append=T,fill=T)
    
    ridged_LL <- -log_likelihood_output # - sum(dnorm(par,avg,std_err_prior,log=T))#+ 10*sum(par[c(2:3,5:6)]^2)    #########
    
    return(ridged_LL)    #########
  }
  
  ### M-H algorithm, based on capture=-recapture paper
  set.seed(6)
  gibbs_samples <- matrix(NA,4000,10)
  gibbs_samples[1,] <- c(avg,0)
  
  std_err_prior <- c(std_err, 1)
  #std_err_prior[2:3] <- 0.01
  #std_err_prior[5:6] <- 0.01
  #std_err_prior[8:9] <- 0.01

  for (m in 2:4000) {
    for (n in 1:10) {
      proposal <- rnorm(1,gibbs_samples[m-1,n],std_err_prior[n])
      transit_ratio <- dnorm(gibbs_samples[m-1,n],proposal,std_err_prior[n])/dnorm(proposal,gibbs_samples[m-1,n],std_err_prior[n])
      para <- rep(NA, 10)
      para[1:n] <- gibbs_samples[m,1:n]
      para[n:10] <- gibbs_samples[m-1,n:10]
      para[n] <- proposal
      if (n == 1) {
        para_old <- gibbs_samples[m-1,]
      } else {
        para_old <- gibbs_samples[m-1,]
        para_old[1:(n-1)] <- gibbs_samples[m,1:(n-1)]
      }
      
      pre_likelihood <- likelihood_function(para_old)
      new_likelihood <- likelihood_function(para)
      likelihood_ratio <- exp(pre_likelihood - new_likelihood)
      print(c(pre_likelihood, new_likelihood))
      accept_ratio <- likelihood_ratio *  transit_ratio
      rand <- runif(1)
      gibbs_samples[m,n] <- ifelse(accept_ratio >= rand, proposal, gibbs_samples[m-1,n])
    }
    write.csv(gibbs_samples,"C:/Users/USXG713181/OneDrive/Google drive/trip bias paper/scenarios/real_world/real_world_results.csv",row.names = F)
  }

  
  
  # ### Gibbs sampling
  # clusterExport(cl, c("likelihood_function","pop_densities_unique","income_zone","obs_zones_counts","obs_1_zones_count","obs_2_zones_count","obs_12_zones_count","avg_prior","std_err_prior",
  #                     "data_baseline"))
  # 
  # #gibbs_samples_existing <- read.csv("G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/model_results/gibbs_samples_multinomial_2000_density_as_predictor_correlatedPredictors_parameterization.csv") 
  # set.seed(6)
  # 
  # gibbs_samples <- matrix(NA,3000,7)    #alpha_1, alpha_2, error_1, error_2, n_a
  # gibbs_samples[1,1:2] <- fit_1_lm_obs$coefficients
  # gibbs_samples[1,3:4] <- fit_2_lm_obs$coefficients
  # gibbs_samples[1,5:6] <- fit_lm_obs$coefficients
  # gibbs_samples[1,7] <- 0    # prior assumption: dataset independence
  # gibbs_samples[1,][which(is.na(gibbs_samples[1,]))] <- 0
  # # gibbs_samples[1:2000,] <- as.matrix(gibbs_samples_existing[1:2000,])
  # 
  # sds <- c(std_err,1) #abs(c(summary(fit_1_lm)$coefficients[,2],summary(fit_1_lm)$coefficients[,2],summary(fit_lm)$coefficients[,2]))
  # 
  # for (m in 2:3000) {
  #   #clusterExport(cl, "i")
  # 
  #   for (n in 1:7) {
  #     para <- matrix(NA,100,7)
  #     para[,1:n] <- matrix(gibbs_samples[m,1:n],nrow=100,ncol=n,byrow=T)
  #     para[,n:7] <- matrix(gibbs_samples[m-1,n:7],nrow=100,ncol=8-n,byrow=T)
  #     para[,n] <- rnorm(100,gibbs_samples[m-1,n],sds[n])
  #     clusterExport(cl, "para")
  #     lhs <- parSapply(cl, 1:100, function(x) likelihood_function(para[x,]))
  #     lhs <- -lhs
  #     lhs_max <- max(lhs[!is.infinite(lhs) & !is.na(lhs)])
  #     lhs <- exp(lhs - lhs_max)
  #     lhs[is.infinite(lhs)] <- 0
  #     lhs[is.na(lhs)] <- 0
  #     lhs_scale <- lhs/sum(lhs)
  #     if(sum(is.na(lhs_scale)) == 100) lhs_scale <- rep(0.01, 100)
  #     gibbs_samples[m,n] <- sample(para[,n],1,prob=lhs_scale)
  #   }
  # 
  #   write.csv(gibbs_samples,"C:/Users/USXG713181/OneDrive/Google drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/new_version/dataset_dependence_test/scenario3(-0.08)_density_negNumDensityCor_results_MAP.csv",row.names = F)
  # }
  
  
  # ### Filtering
  # coefs <- c(fit_1_lm$coefficients,fit_2_lm$coefficients,fit_lm$coefficients)
  # 
  # particles <- matrix(NA,1000,9)
  # for (l in 1:9) {
  #   particles[,l] <- rnorm(1000,coefs[l],abs(coefs[l]))
  # }
  # 
  # 
  # clusterExport(cl, "particles")
  # lhs <- parSapply(cl, 1:1000, function(x) likelihood_function(particles[x,]))
  # lhs <- -lhs
  # lhs_max <- max(lhs[!is.infinite(lhs)])
  # lhs <- exp(lhs - lhs_max)
  # lhs[is.infinite(lhs)] <- 0
  # lhs_scale <- lhs/sum(lhs)
  # sampled_index <- sample(1:1000, 1000, prob=lhs_scale, replace=T)

  stopCluster(cl)
  
  # ###MLE --------------------------------------------------------------------------------------------------------------
  # #-21.06326007,0.06178814,rep(-4.83395080,5)
  # max_obs <- max(obs_zones_counts)
  # 
  # initial_result <- read.csv("G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/new_version/scenario2(-0.08)_density_negNumDensityCor_results_MAP.csv")
  # iniital_values <- colMeans(initial_result[500:1000,])
  # 
  # estim_optim <- spg(par_ground_truth,likelihood_function,lower=c(rep(-Inf,9)),upper = c(rep(Inf,9)),
  #              control = list(maxit = 300000, trace = TRUE, maxfeval=500000))    #, eps=0.0000001
  # estim_optim <- optim(iniital_values,likelihood_function, method="L-BFGS-B", lower = rep(-Inf,9), upper = rep(Inf,9), #hessian=T,
  #                      control = list(maxit = 30000,trace = TRUE,ndeps=rep(0.0000001,9),factr=1e5,REPORT=1))
  # # estim_potim <- optim(c(0.01,-0.000000001,0.00001,0.00001, obs_zones_counts + 100),likelihood_function, method="L-BFGS-B", lower = c(rep(-Inf,4),obs_zones_counts), upper = c(rep(Inf,4),rep(10000,5)), #hessian=T,
  # #              control = list(maxit = 30000,trace = TRUE,parscale=c(10,0.01,10,0.01,1000,1000,1000,1000,1000),ndeps=c(rep(0.000000001,9)),factr=1e5,REPORT=1))
  # # estim_optim <- hjkb(c(-10,0.01,10,-0.01, max_obs + 100),likelihood_function, lower = c(rep(-Inf,4),max_obs), upper = c(rep(Inf,5)),
  # #                      control = list(tol=1e-8,maxfeval=100000,info=T))
  # # estim_optim <- nmkb(c(-10,0.01,10,-0.01, obs_zones_counts + 100),likelihood_function, lower = c(rep(-Inf,4),obs_zones_counts), upper = c(rep(Inf,4),rep(5000,5)),
  # #                     control = list(tol=1e-8,maxfeval=100000,trace=T))
  # para_data <- estim_optim$par
  # hessian_data <- hessian(likelihood_function,para_data,method.args=list(eps=1e-9,d=1e-4)) #estim_optim$hessian
  # se_data <- sqrt(abs(diag(solve(hessian_data))))
  # results <- data.frame(para_data,se_data)
  # write.csv(results,"G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/regularization/scenario2(-0.03)_density_MAP.csv",row.names=F)
#}
