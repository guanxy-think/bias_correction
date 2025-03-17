trips <- read.csv("D:/OneDrive/Google drive/trip bias paper/data/csv/trippub.csv")
trips <- trips[,c("HOUSEID","PERSONID","TDTRPNUM","WTTRDFIN")]

persons <- read.csv("D:/OneDrive/Google drive/trip bias paper/data/csv/perpub.csv")
persons <- persons[,c("HOUSEID","PERSONID","R_AGE")]

cbg_file <- read.csv("D:/OneDrive/Google drive/trip bias paper/data/csv/tripctbg.csv")
cbg_file <- cbg_file[,c("HOUSEID","PERSONID","TDTRPNUM", "ORIG_ST", "ORIG_CNTY", "ORIG_CT")]

census_data <- read.csv("D:/OneDrive/Google drive/trip bias paper/data/seattle.txt")

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

merged_data_geo$p_1 <- 0.01
merged_data_geo$p_2 <- 0.1
merged_data_geo$p_2 <- merged_data_geo$p_2 * merged_data_geo$density / max(merged_data_geo$density)

set.seed(3)

merged_data_geo$capture_1_2 <- sapply(1:nrow(merged_data_geo), function(x) length(which(merged_data_geo$p_1[x] > runif(merged_data_geo$WTTRDFIN[x]) & merged_data_geo$p_2[x] > runif(merged_data_geo$WTTRDFIN[x]))))
merged_data_geo$capture_2 <- sapply(1:nrow(merged_data_geo), function(x) length(which(merged_data_geo$p_1[x] < runif(merged_data_geo$WTTRDFIN[x]) & merged_data_geo$p_2[x] > runif(merged_data_geo$WTTRDFIN[x]))))
merged_data_geo$capture_1 <- sapply(1:nrow(merged_data_geo), function(x) length(which(merged_data_geo$p_1[x] > runif(merged_data_geo$WTTRDFIN[x]) & merged_data_geo$p_2[x] < runif(merged_data_geo$WTTRDFIN[x]))))
merged_data_geo$capture_n1_n2 <- merged_data_geo$WTTRDFIN - merged_data_geo$capture_1 - merged_data_geo$capture_2 - merged_data_geo$capture_1_2

merged_data_geo$zone_id <- paste(merged_data_geo$ORIG_ST,merged_data_geo$ORIG_CNTY,merged_data_geo$ORIG_CT, sep="")

write.csv(merged_data_geo, "D:/OneDrive/Google drive/trip bias paper/data/real_world_data.csv", row.names=F)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#inference model
library(foreach)
library(doParallel)
no_cores <- 5
registerDoParallel(makeCluster(no_cores))
library(BB)
library(numDeriv)    #calculate gradient

merged_data_geo <- read.csv("D:/OneDrive/Google drive/trip bias paper/data/real_world_data.csv")

zones <- unique(merged_data_geo$zone_id)

pop_densities_unique <- sapply(zones, function(x) mean(merged_data_geo$density[which(merged_data_geo$zone_id==x)]))

obs_zones_counts <- sapply(zones, function(x) sum(merged_data_geo$capture_1[which(merged_data_geo$zone_id==x)]) + sum(merged_data_geo$capture_2[which(merged_data_geo$zone_id==x)]) + sum(merged_data_geo$capture_1_2[which(merged_data_geo$zone_id==x)]))

obs_1_zones_count <- sapply(zones, function(x) sum(merged_data_geo$capture_1[which(merged_data_geo$zone_id==x)]))
obs_2_zones_count <- sapply(zones, function(x) sum(merged_data_geo$capture_2[which(merged_data_geo$zone_id==x)]))
obs_12_zones_count <- sapply(zones, function(x) sum(merged_data_geo$capture_1_2[which(merged_data_geo$zone_id==x)]))

ground_truth <- sapply(zones, function(x) sum(merged_data_geo$WTTRDFIN[merged_data_geo$zone_id==x]))

likelihood_function <- function(par) {
  set.seed(1)
  utility_zones_1 <- par[1] + par[2]*pop_densities_unique
  utility_zones_2 <- par[3] + par[4]*pop_densities_unique
  p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
  p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
  p_zones_1[which(p_zones_1 > 0.9999999999)] <- 0.9999999999
  p_zones_2[which(p_zones_2 > 0.9999999999)] <- 0.9999999999
  p_zones_1[which(p_zones_1 < 0.0000000001)] <- 0.0000000001
  p_zones_2[which(p_zones_2 < 0.0000000001)] <- 0.0000000001
  
  lambda <- par[5:20]
  #poisson_upper_bound <- qpois(0.999999,lambda)
  
  cat(par,file = "D:/OneDrive/Google drive/trip bias paper/logfile_server1.txt", append=T,fill=T)
  cat(lambda,file = "D:/OneDrive/Google drive/trip bias paper/logfile_server1.txt", append=T,fill=T)
  all_likelihoods <- c()
  for (i in 1:16) {
    sample_total <- rpois(1000, lambda[i])
    #cat(i,file = "D:/OneDrive/Google drive/trip bias paper/logfile_server.txt", append=T,fill=T)
    likelihoods <- foreach(j = sample_total, .combine="c", .export=c("obs_zones_counts","obs_1_zones_count","obs_2_zones_count","obs_12_zones_count")) %dopar% {
      if (j < obs_zones_counts[i]) {
        log_likelihood_zones <- -Inf
      } else {
        obs_0_zones_count <- j - obs_zones_counts[i]
        n_a_zones <- j
        
        obs_0_zones_count <- round(obs_0_zones_count)
        n_a_zones <- round(n_a_zones)
        
        log_likelihood_zones <- dmultinom(c(obs_0_zones_count,obs_1_zones_count[i],obs_2_zones_count[i],obs_12_zones_count[i]),size=n_a_zones, 
                                          prob=c((1 - p_zones_1[i])*(1 - p_zones_2[i]),
                                                 p_zones_1[i]*(1 - p_zones_2[i]),
                                                 (1-p_zones_1[i])*p_zones_2[i],
                                                 p_zones_1[i]*p_zones_2[i]), log=T)
      }
      
      log_likelihood_zones
    }
    
    LL_max <- max(likelihoods)
    LL <- log(1000) + LL_max + log(mean(exp(likelihoods-LL_max)))
    
    all_likelihoods <- c(all_likelihoods,LL)
  }
  
  log_likelihood <- sum(all_likelihoods)
  
  cat(log_likelihood,file = "D:/OneDrive/Google drive/trip bias paper/logfile_server1.txt", append=T,fill=T)
  return(-log_likelihood)
}

lambda <- mean(obs_zones_counts) * 10
estim_optim <- optim(c(0, 0, 0, 0, obs_zones_counts*10),likelihood_function, method="L-BFGS-B", lower = c(rep(-Inf,4),obs_zones_counts), upper = Inf, hessian=T,
                     control = list(maxit = 30000,trace = TRUE,ndeps=c(rep(0.000001,4),rep(1,16)),factr=0.45e9,REPORT=1))
