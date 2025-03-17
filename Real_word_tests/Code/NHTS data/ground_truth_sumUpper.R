trips <- read.csv("C:/Users/USXG713181/OneDrive/Google drive/trip bias paper/data/csv/trippub.csv")
trips <- trips[,c("HOUSEID","PERSONID","TDTRPNUM","WTTRDFIN")]

persons <- read.csv("C:/Users/USXG713181/OneDrive/Google drive/trip bias paper/data/csv/perpub.csv")
persons <- persons[,c("HOUSEID","PERSONID","R_AGE")]

cbg_file <- read.csv("C:/Users/USXG713181/OneDrive/Google drive/trip bias paper/data/csv/tripctbg.csv")
cbg_file <- cbg_file[,c("HOUSEID","PERSONID","TDTRPNUM", "ORIG_ST", "ORIG_CNTY", "ORIG_CT")]

census_data <- read.csv("C:/Users/USXG713181/OneDrive/Google drive/trip bias paper/data/seattle.txt")

merged_data <- merge(trips, persons, by=c("HOUSEID","PERSONID"), all.x=T)
merged_data <- merge(merged_data, cbg_file, by=c("HOUSEID","PERSONID","TDTRPNUM"), all.x=T)
merged_data$ORIG_ST <- as.integer(merged_data$ORIG_ST)
merged_data$ORIG_CNTY <- as.integer(merged_data$ORIG_CNTY)
merged_data$ORIG_CT <- as.integer(merged_data$ORIG_CT)

merged_data_geo <- merge(merged_data, census_data, by.x=c("ORIG_ST","ORIG_CNTY","ORIG_CT"), by.y=c("STATEFP","COUNTYFP","TRACTCE"), all.y=T)
merged_data_geo$density <- merged_data_geo$B01001e1/merged_data_geo$area

merged_data_geo <- merged_data_geo[,c("ORIG_ST","ORIG_CNTY","ORIG_CT","HOUSEID","PERSONID","TDTRPNUM","R_AGE","density","B01002e1","WTTRDFIN")]
colnames(merged_data_geo) <- c("ORIG_ST","ORIG_CNTY","ORIG_CT","HOUSEID","PERSONID","TDTRPNUM","R_AGE","density","median_age_CT","WTTRDFIN")

merged_data_geo$WTTRDFIN <- merged_data_geo$WTTRDFIN/365

##calculate capture probability
mu <- median(unique(merged_data_geo$density))/10000
delta_1 <- -(max(merged_data_geo$density)/10000-mu)/log(1/9)
delta_2 <- -(min(merged_data_geo$density)/10000-mu)/log(1/9)

alpha_1 <- -mu/delta_1
beta_1 <- 1/delta_1
alpha_2 <- -mu/delta_2
beta_2 <- 1/delta_2

mu_1 <- quantile(unique(merged_data_geo$density),1)/10000
mu_2 <- quantile(unique(merged_data_geo$density),0)/10000
delta_1 <- -(min(merged_data_geo$density)/10000-mu_1)/log(99)
delta_2 <- -(max(merged_data_geo$density)/10000-mu_2)/log(99)

alpha_1 <- -mu_1/delta_1
beta_1 <- 1/delta_1
alpha_2 <- -mu_2/delta_2
beta_2 <- 1/delta_2

mu_1 <- quantile(unique(merged_data_geo$density),0.9)/10000
mu_2 <- quantile(unique(merged_data_geo$density),0.1)/10000
delta_1 <- -(min(merged_data_geo$density)/10000-mu_1)/log(99)
delta_2 <- -(max(merged_data_geo$density)/10000-mu_2)/log(99)

alpha_1 <- -mu_1/delta_1
beta_1 <- 1/delta_1
alpha_2 <- -mu_2/delta_2
beta_2 <- 1/delta_2

#merged_data_geo$p_1 <- 0.01
#merged_data_geo$p_2 <- 0.1
#merged_data_geo$p_2 <- merged_data_geo$p_2 * merged_data_geo$density / max(merged_data_geo$density)
merged_data_geo$p_1 <- 1/(1+exp(-(alpha_1+beta_1*merged_data_geo$density/10000)))
merged_data_geo$p_2 <- 1/(1+exp(-(alpha_2+beta_2*merged_data_geo$density/10000)))

set.seed(3)

merged_data_geo$capture_1_2 <- sapply(1:nrow(merged_data_geo), function(x) length(which(merged_data_geo$p_1[x] > runif(merged_data_geo$WTTRDFIN[x]) & merged_data_geo$p_2[x] > runif(merged_data_geo$WTTRDFIN[x]))))
merged_data_geo$capture_2 <- sapply(1:nrow(merged_data_geo), function(x) length(which(merged_data_geo$p_1[x] < runif(merged_data_geo$WTTRDFIN[x]) & merged_data_geo$p_2[x] > runif(merged_data_geo$WTTRDFIN[x]))))
merged_data_geo$capture_1 <- sapply(1:nrow(merged_data_geo), function(x) length(which(merged_data_geo$p_1[x] > runif(merged_data_geo$WTTRDFIN[x]) & merged_data_geo$p_2[x] < runif(merged_data_geo$WTTRDFIN[x]))))
merged_data_geo$capture_n1_n2 <- merged_data_geo$WTTRDFIN - merged_data_geo$capture_1 - merged_data_geo$capture_2 - merged_data_geo$capture_1_2

merged_data_geo$zone_id <- paste(merged_data_geo$ORIG_ST,merged_data_geo$ORIG_CNTY,merged_data_geo$ORIG_CT, sep="")

write.csv(merged_data_geo, "D:/OneDrive/Google drive/trip bias paper/data/setting_parameters/real_world_data_more_missing_2.csv", row.names=F)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#inference model
library(foreach)
library(doParallel)
no_cores <- 5
registerDoParallel(makeCluster(no_cores))
library(BB)
library(numDeriv)    #calculate gradient
library(extraDistr)

#merged_data_geo <- read.csv("C:/Users/Xiangyang/OneDrive/Google drive/trip bias paper/data/real_world_data.csv")
merged_data_geo <- read.csv("D:/OneDrive/Google drive/trip bias paper/data/setting_parameters/real_world_data_more_missing_2.csv")
merged_data_geo$density <- merged_data_geo$density/10000

zones <- unique(merged_data_geo$zone_id)[c(2,5,6,11,14)]    #select 3 zones

pop_densities_unique <- sapply(zones, function(x) mean(merged_data_geo$density[which(merged_data_geo$zone_id==x)]))

obs_zones_counts <- sapply(zones, function(x) sum(merged_data_geo$capture_1[which(merged_data_geo$zone_id==x)]) + sum(merged_data_geo$capture_2[which(merged_data_geo$zone_id==x)]) + sum(merged_data_geo$capture_1_2[which(merged_data_geo$zone_id==x)]))

obs_1_zones_count <- sapply(zones, function(x) sum(merged_data_geo$capture_1[which(merged_data_geo$zone_id==x)]))
obs_2_zones_count <- sapply(zones, function(x) sum(merged_data_geo$capture_2[which(merged_data_geo$zone_id==x)]))
obs_12_zones_count <- sapply(zones, function(x) sum(merged_data_geo$capture_1_2[which(merged_data_geo$zone_id==x)]))

ground_truth <- sapply(zones, function(x) sum(merged_data_geo$WTTRDFIN[merged_data_geo$zone_id==x]))
(sum(ground_truth)-sum(obs_zones_counts))/sum(ground_truth)

obs_1_zones_count <- round(obs_1_zones_count/(24))
obs_2_zones_count <- round(obs_2_zones_count/(24))
obs_12_zones_count <- round(obs_12_zones_count/(24))
obs_zones_counts <- obs_1_zones_count + obs_2_zones_count + obs_12_zones_count
ground_truth <- ground_truth/(24)

p_1 <- sapply(zones, function(x) mean(merged_data_geo$p_1[which(merged_data_geo$zone_id==x)]))
p_2 <- sapply(zones, function(x) mean(merged_data_geo$p_2[which(merged_data_geo$zone_id==x)]))

p_1_coef <- lm(p_1~pop_densities_unique)$coefficients
p_2_coef <- lm(p_2~pop_densities_unique)$coefficients

set.seed(1)
likelihood_function <- function(par) {
  #set.seed(1)
  utility_zones_1 <- par[1] + par[2]*pop_densities_unique#par[1:5]#
  utility_zones_2 <- par[3] + par[4]*pop_densities_unique#par[6:10]#
  p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
  p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
  #p_zones_1[which(p_zones_1 > 0.9999999999)] <- 0.9999999999
  #p_zones_2[which(p_zones_2 > 0.9999999999)] <- 0.9999999999
  #p_zones_1[which(p_zones_1 < 0.0000000001)] <- 0.0000000001
  #p_zones_2[which(p_zones_2 < 0.0000000001)] <- 0.0000000001
  
  lambda <- par[5:9]#par[5] + par[6]*pop_densities_unique#
  poisson_upper_bound <- qpois(0.999999,lambda)
  
  #cat(par,file = "C:/Users/Xiangyang/OneDrive/Google drive/trip bias paper/logfile_16zones_paraP.txt", append=T,fill=T)
  cat(par,file = "D:/OneDrive/Google drive/trip bias paper/logfile_5zones_noParaN_paraP_down.txt", append=T,fill=T)
  #cat(lambda,file = "D:/OneDrive/Google drive/trip bias paper/logfile_16zones.txt", append=T,fill=T)
  all_likelihoods <- c()
  for (i in 1:5) {
    #sample_total <- rpois(1000, lambda[i])
    #cat(i,file = "D:/OneDrive/Google drive/trip bias paper/logfile_server.txt", append=T,fill=T)
    #likelihoods <- foreach(j = sample_total, .combine="c", .export=c("obs_zones_counts","obs_1_zones_count","obs_2_zones_count","obs_12_zones_count")) %dopar% {
    likelihoods <- foreach(j = obs_zones_counts[i]:poisson_upper_bound[i], .combine="c", .export=c("obs_zones_counts","obs_1_zones_count","obs_2_zones_count","obs_12_zones_count")) %dopar% {
    #for (j in obs_zones_counts[i]:poisson_upper_bound[i]) {
      obs_0_zones_count <- j - obs_zones_counts[i]
      n_a_zones <- j
      
      obs_0_zones_count <- round(obs_0_zones_count)
      n_a_zones <- round(n_a_zones)
      
      log_likelihood_zones <- dmultinom(c(obs_0_zones_count,obs_1_zones_count[i],obs_2_zones_count[i],obs_12_zones_count[i]),size=n_a_zones, 
                                        prob=c((1 - p_zones_1[i])*(1 - p_zones_2[i]),
                                               p_zones_1[i]*(1 - p_zones_2[i]),
                                               (1-p_zones_1[i])*p_zones_2[i],
                                               p_zones_1[i]*p_zones_2[i]), log=T)
      
      log_likelihood_zones + dpois(j,lambda[i],log=T)
    }
    
    LL_max <- max(likelihoods)
    LL <- LL_max + log(sum(exp(likelihoods-LL_max)))
    
    all_likelihoods <- c(all_likelihoods,LL)
  }
  
  log_likelihood <- sum(all_likelihoods)
  
  #cat(log_likelihood,file = "C:/Users/Xiangyang/OneDrive/Google drive/trip bias paper/logfile_16zones_paraP.txt", append=T,fill=T)
  cat(log_likelihood,file = "C:/Users/USXG713181/OneDrive/Google drive/trip bias paper/logfile_5zones_noParaN_paraP_down.txt", append=T,fill=T)
  return(-log_likelihood)
}

###5/20: manually set the parameter values associated with the capture probabilities, using meaningful values from the literature.
###5/20: try initial values close to the ground truth and see if the MLE can recover the ground truth.
init <- obs_zones_counts
#init <- lm(log(obs_zones_counts*10)~pop_densities_unique)$coefficients
logit_1 <- merged_data_geo$p_1/(1-merged_data_geo$p_1)
summary(lm(log(logit_1)~merged_data_geo$density/1000))
logit_2 <- merged_data_geo$p_2/(1-merged_data_geo$p_2)
summary(lm(log(logit_2)~merged_data_geo$density/1000))

estim_optim <- optim(c(rep(0,4), init),likelihood_function, method="L-BFGS-B", lower = c(rep(-Inf,4),obs_zones_counts), upper = Inf, hessian=T,
                     control = list(maxit = 30000,trace = TRUE,factr=0.45e9,REPORT=1))
estim_optim <- optim(c(rep(0,10), init),likelihood_function, method="L-BFGS-B", lower = c(rep(-Inf,10),obs_zones_counts), upper = Inf, hessian=T,
                     control = list(maxit = 30000,trace = TRUE,factr=0.45e9,REPORT=1,parscale=c(rep(0.01,10),obs_zones_counts)))

estim_optim <- optim(c(rep(0,4), init),likelihood_function, method="L-BFGS-B", lower = c(rep(-Inf,6)), upper = Inf, hessian=T,
                     control = list(maxit = 30000,trace = TRUE,ndeps=c(rep(0.00000001,6)),factr=0.45e9,REPORT=1))

estim_optim <- optim(c(rep(0,4)),likelihood_function, method="L-BFGS-B", lower = c(rep(-Inf,4)), upper = Inf, hessian=T,
                     control = list(maxit = 30000,trace = TRUE,ndeps=c(rep(0.00000001,4)),factr=0.45e9,REPORT=1))
#estim_optim <- spg(c(0, 0, 0, 0, obs_zones_counts*3),likelihood_function,lower = c(rep(-Inf,4),obs_zones_counts), upper = Inf, control = list(maxit = 300000, trace = TRUE, maxfeval=500000))
par <- estim_optim$par#[1:4]
se_data <- sqrt(abs(diag(solve(estim_optim$hessian))))
utility_zones_1 <- par[1] + par[2]*pop_densities_unique#par[1:3]#
utility_zones_2 <- par[3] + par[4]*pop_densities_unique#par[4:6]#
p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
est <- obs_zones_counts/(p_zones_1+p_zones_2-p_zones_1*p_zones_2)
write.csv(data.frame(par,se_data), "D:/OneDrive/Google drive/trip bias paper/data/results/parameter_est_5zones(no paraN, paraP)_moreMissing2.csv",row.names = F)
write.csv(data.frame(ground_truth,est,obs_zones_counts), "D:/OneDrive/Google drive/trip bias paper/data/results/estimation_result_5zones(no paraN, paraP)_moreMissing2.csv",row.names = F)

### Bayesian
gibbs_samples <- matrix(NA,1000,9)    #alpha_1, alpha_2, error_1, error_2, n_a
gibbs_samples[1,1] <- 0.001
gibbs_samples[1,2] <- 0.001
gibbs_samples[1,3] <- 0.001
gibbs_samples[1,4] <- 0.001
gibbs_samples[1,5:9] <- 1   # increment over observed number of trips

for (i in 2:1000) {
  for (j in 1:9) {
    para <- matrix(NA,50,9)
    para[,1:j] <- matrix(gibbs_samples[i,1:j],nrow=50,ncol=j,byrow=T)
    para[,j:9] <- matrix(gibbs_samples[i-1,j:9],nrow=50,ncol=10-j,byrow=T)
    if (j < 5) {
      para[,j] <- runif(50,gibbs_samples[i-1,j]*0.95,gibbs_samples[i-1,j]*1.05)
    } else {
      para[,j] <- rtpois(50,gibbs_samples[i-1,j],a=0,b=ground_truth[j-4]*1.5)
    }
    
    para_model <- para
    para_model[,5:9] <- para_model[,5:9] + matrix(obs_zones_counts,50,5,byrow=T)
    
    lhs <- sapply(1:50, function(x) likelihood_function(para_model[x,]))
    lhs <- -lhs
    lhs_max <- max(lhs[!is.infinite(lhs) & !is.na(lhs)])
    lhs <- exp(lhs - lhs_max)
    lhs[is.infinite(lhs)] <- 0
    lhs[is.na(lhs)] <- 0
    lhs_scale <- lhs/sum(lhs)
    gibbs_samples[i,j] <- sample(para[,j],1,prob=lhs_scale)
  }
  
  write.csv(gibbs_samples,"D:/OneDrive/Google drive/trip bias paper/data/results/gibbs_samples_5zone.csv",row.names = F)
}

plot(gibbs_samples[,7], type="l",xlab="Iterations",ylab="Missing trips in zone 3", cex.lab=1.2)
likelihoods <- c()
for (i in 1:1000) {
  para <- gibbs_samples[i,]
  para[5:7] <- para[5:7] + obs_zones_counts
  l <- likelihood_function(para)
  likelihoods <- c(likelihoods, l)
}
plot(-likelihoods, type="l",xlab="Iterations",ylab="Log-Likelihood", cex.lab=1.2)
para <- c(-8.74186504742123e-10, -4.38673819702018e-05, 3.40599683646934e-10, 1.78473601271882e-05, 15.4313090415356, 49.8235029835649, 46.5671765132911)
likelihood_function(para)
para_truth <- c(1.000000e-02,-7.953265e-19,-3.204938e-17,1.794948e-02,ground_truth)
likelihood_function(para_truth)
