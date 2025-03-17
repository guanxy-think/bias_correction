###same as simulate_n_a.R
###simulation for inference_model_1_2_2.R
library(foreach)
library(doParallel)
no_cores <- 16
registerDoParallel(makeCluster(no_cores))
library(truncnorm)

file_names <- c("scenario_low_low","scenario_low_mid","scenario_low_high","scenario_mid_low","scenario_mid_mid","scenario_high_low")
#"baseline_unif","baseline_beta_left","scen1_unif","scen1_beta_left",,"scen2_0.1_unif","scen2_0.1_beta_left","scen2_0.1_beta_right",
#                "scen2_0.5_unif","scen2_0.5_beta_left","scen2_0.5_beta_right","scen2_0.9_unif","scen2_0.9_beta_left","scen2_0.9_beta_right"

a_file <- file_names[1]
results <- NULL
results_zones <- NULL
obs_counts_zone <- NULL
truth_zone <- NULL
for (a_file in file_names) {
  result_path <- paste("G:/My Drive/trip bias paper/scenarios/joint_prob/results_with_density_1/",a_file,".csv",sep="")
  para_info <- read.csv(result_path)
  para_info$se_data[which(is.na(para_info$se_data))] <- 0
  
  data_path <- paste('G:/My Drive/trip bias paper/scenarios/joint_prob/',a_file,".csv",sep="")
  data_info <- read.csv(data_path)
  
  ###completely random simulations
  # par_1_all <- rnorm(10000, para_info$para_data[1],para_std[1])
  # par_2_all <- rnorm(10000, para_info$para_data[2],para_std[2])
  # par_3_all <- rtruncnorm(10000, 0, Inf, para_info$para_data[3],para_std[3])
  # par_4_all <- rtruncnorm(10000, 0, Inf, para_info$para_data[4],para_std[4])
  # 
  # n_as <- foreach(i=1:10000, .combine="rbind", .packages="truncnorm") %dopar% {
  #   par_1 <- par_1_all[i]
  #   par_2 <- par_2_all[i]
  #   par_3 <- par_3_all[i]
  #   par_4 <- par_4_all[i]
  # 
  #   set.seed(1)
  #   noise_1 <- rnorm(obs_all_count,0,par_3)
  #   noise_2 <- rnorm(obs_all_count,0,par_4)
  # 
  #   utility_1 <- par_1 + noise_1
  #   utility_2 <- par_2 + noise_2
  # 
  #   p_i_a_1 <- 1 - 1/(1+exp(utility_1))
  #   p_i_a_2 <- 1 - 1/(1+exp(utility_2))
  #   #https://stackoverflow.com/questions/63495476/r-getting-very-small-number-instead-of-zero (searching "R small number zero")
  #   p_i_a_1[which(p_i_a_1 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_2[which(p_i_a_2 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_1[which(p_i_a_1 < 0.0000000001)] <- 0.0000000001
  #   p_i_a_2[which(p_i_a_2 < 0.0000000001)] <- 0.0000000001
  #   p_a_1 <- mean(p_i_a_1)
  #   p_a_2 <- mean(p_i_a_2)
  # 
  #   n_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)
  #   
  #   obs_0_count <- sapply(n_a - obs_all_count, function(x) max(x,0))
  #   
  #   likelihood_0 <- obs_0_count*log((1 - p_a_1)*(1 - p_a_2))
  #   likelihood_1 <- log(p_i_a_1[1:obs_1_count]*(1 - p_i_a_2[1:obs_1_count]))
  #   if (obs_1_count == 0) likelihood_1 <- 0
  #   likelihood_2 <- log((1 - p_i_a_1[(obs_1_count+1):(obs_1_count+obs_2_count)])*p_i_a_2[(obs_1_count+1):(obs_1_count+obs_2_count)])
  #   if (obs_2_count == 0) likelihood_2 <- 0
  #   likelihood_12 <- log(p_i_a_1[(obs_1_count+obs_2_count+1):obs_all_count]*p_i_a_2[(obs_1_count+obs_2_count+1):obs_all_count])
  #   if (obs_12_count == 0) likelihood_12 <- 0
  #   log_likelihood <- likelihood_0+sum(likelihood_1)+sum(likelihood_2)+sum(likelihood_12)
  # 
  #   c(n_a,log_likelihood)
  # }
  
  ###fix alpha_1 and alpha_2 and randomize error_std--------------------------------------------------------------------------------------------------------------------
  # par_1_upper <- para_info$para_data[1]+para_info$se_data[1]*1.96
  # par_1_lower <- para_info$para_data[1]-para_info$se_data[1]*1.96
  # par_2_upper <- para_info$para_data[2]+para_info$se_data[2]*1.96
  # par_2_lower <- para_info$para_data[2]-para_info$se_data[2]*1.96
  # par_3_all <- rtruncnorm(10000,0,Inf,para_info$para_data[3],para_std[3])
  # par_4_all <- rtruncnorm(10000,0,Inf,para_info$para_data[4],para_std[4])
  # 
  # n_as_upper <- foreach(i=1:10000, .combine="c", .packages="truncnorm") %dopar% {
  #   par_1 <- par_1_upper
  #   par_2 <- par_2_upper
  #   par_3 <- par_3_all[i]
  #   par_4 <- par_4_all[i]
  # 
  #   set.seed(6)
  #   noise_1 <- rnorm(obs_all_count,0,par_3)
  #   noise_2 <- rnorm(obs_all_count,0,par_4)
  # 
  #   utility_1 <- par_1 + noise_1
  #   utility_2 <- par_2 + noise_2
  # 
  #   p_i_a_1 <- 1 - 1/(1+exp(utility_1))
  #   p_i_a_2 <- 1 - 1/(1+exp(utility_2))
  #   #https://stackoverflow.com/questions/63495476/r-getting-very-small-number-instead-of-zero (searching "R small number zero")
  #   p_i_a_1[which(p_i_a_1 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_2[which(p_i_a_2 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_1[which(p_i_a_1 < 0.0000000001)] <- 0.0000000001
  #   p_i_a_2[which(p_i_a_2 < 0.0000000001)] <- 0.0000000001
  #   p_a_1 <- mean(p_i_a_1)
  #   p_a_2 <- mean(p_i_a_2)
  # 
  #   n_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)
  # 
  #   n_a
  # }
  # 
  # n_as_lower <- foreach(i=1:10000, .combine="c", .packages="truncnorm") %dopar% {
  #   par_1 <- par_1_lower
  #   par_2 <- par_2_lower
  #   par_3 <- par_3_all[i]
  #   par_4 <- par_4_all[i]
  # 
  #   set.seed(6)
  #   noise_1 <- rnorm(obs_all_count,0,par_3)
  #   noise_2 <- rnorm(obs_all_count,0,par_4)
  # 
  #   utility_1 <- par_1 + noise_1
  #   utility_2 <- par_2 + noise_2
  # 
  #   p_i_a_1 <- 1 - 1/(1+exp(utility_1))
  #   p_i_a_2 <- 1 - 1/(1+exp(utility_2))
  #   #https://stackoverflow.com/questions/63495476/r-getting-very-small-number-instead-of-zero (searching "R small number zero")
  #   p_i_a_1[which(p_i_a_1 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_2[which(p_i_a_2 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_1[which(p_i_a_1 < 0.0000000001)] <- 0.0000000001
  #   p_i_a_2[which(p_i_a_2 < 0.0000000001)] <- 0.0000000001
  #   p_a_1 <- mean(p_i_a_1)
  #   p_a_2 <- mean(p_i_a_2)
  # 
  #   n_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)
  # 
  #   n_a
  # }
  # 
  # n_as_exp <- foreach(i=1:10000, .combine="c", .packages="truncnorm") %dopar% {
  #   par_1 <- para_info$para_data[1]
  #   par_2 <- para_info$para_data[2]
  #   par_3 <- par_3_all[i]
  #   par_4 <- par_4_all[i]
  # 
  #   set.seed(6)
  #   noise_1 <- rnorm(obs_all_count,0,par_3)
  #   noise_2 <- rnorm(obs_all_count,0,par_4)
  # 
  #   utility_1 <- par_1 + noise_1
  #   utility_2 <- par_2 + noise_2
  # 
  #   p_i_a_1 <- 1 - 1/(1+exp(utility_1))
  #   p_i_a_2 <- 1 - 1/(1+exp(utility_2))
  #   #https://stackoverflow.com/questions/63495476/r-getting-very-small-number-instead-of-zero (searching "R small number zero")
  #   p_i_a_1[which(p_i_a_1 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_2[which(p_i_a_2 > 0.9999999999)] <- 0.9999999999
  #   p_i_a_1[which(p_i_a_1 < 0.0000000001)] <- 0.0000000001
  #   p_i_a_2[which(p_i_a_2 < 0.0000000001)] <- 0.0000000001
  #   p_a_1 <- mean(p_i_a_1)
  #   p_a_2 <- mean(p_i_a_2)
  # 
  #   n_a <- obs_all_count/(p_a_1 + p_a_2 - p_a_1*p_a_2)
  # 
  #   n_a
  # }
  
  ### using pop_density
  pop_densities_unique <- unique(data_info$popu_density)
  obs_zones_counts <- sapply(pop_densities_unique, function(x) length(which((data_info$capture_1==1 | data_info$capture_2==1) & data_info$popu_density==x)))
  truth_zone_counts <- sapply(pop_densities_unique, function(x) length(which(data_info$popu_density==x)))
  
  obs_counts_zone <- rbind(obs_counts_zone, obs_zones_counts)
  truth_zone <- rbind(truth_zone, truth_zone_counts)
  
  para_interval <- matrix(NA,9,3)
  para_interval[,1] <- para_info$para_data[1:9] - 1.96*para_info$se_data[1:9]
  para_interval[,2] <- para_info$para_data[1:9]
  para_interval[,3] <- para_info$para_data[1:9] + 1.96*para_info$se_data[1:9]
  
  est <- c()
  for (i in 1:3) {
    para_set <- para_interval[,i]
    
    par_1_all <- para_set[1] + para_set[2]*pop_densities_unique #rnorm(10000, para_info$para_data[1],para_std[1])
    #par_2_all <- para_info$para_data[2] #rnorm(10000, para_info$para_data[2],para_std[2])
    par_3_all <- para_set[3] + para_set[4]*pop_densities_unique #rtruncnorm(10000, 0, Inf, para_info$para_data[3],para_std[3])
    
    utility_zones_1 <- par_1_all #+ par_2_all*pop_densities_unique
    utility_zones_2 <- par_3_all#*pop_densities_unique
    p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
    p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
    p_zones_1[which(p_zones_1 > 1-1e-10)] <- 1-1e-10
    p_zones_2[which(p_zones_2 > 1-1e-10)] <- 1-1e-10
    p_zones_1[which(p_zones_1 < 1e-10)] <- 1e-10
    p_zones_2[which(p_zones_2 < 1e-10)] <- 1e-10
    
    n_a_zones <- obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)
    est <- c(est,sum(n_a_zones))
    
    if (i == 2) results_zones <- rbind(results_zones, n_a_zones)
  }
  
  # weights <- exp(n_as[,2]-max(n_as[,2]))
  # weights <- weights/sum(weights)
  # sampled_n_as <- sample(n_as[,1],10000,T,weights)
  # results <- cbind(results,sampled_n_as)
  
  #results <- cbind(results,c(mean(n_as_upper),mean(n_as_lower),mean(n_as_exp)))
  
  results <- cbind(results,est)
}

colnames(results) <- file_names
write.csv(results,"G:/My Drive/trip bias paper/scenarios/zone_homogenity/n_a_interval.csv",row.names=F)

stopImplicitCluster()

recover_by_zone <- results_zones - obs_counts_zone
par(mar=c(2,2,0.5,0.5))
barplot(recover_by_zone[6,],names.arg=round(recover_by_zone[6,]),cex.names=2)

###data analysis-----------------------------------------------------------------------------------------------------------------------------
obs_counts <- NULL
obs_overlap_count <- NULL
obs_2_all <- NULL
obs_1_all <- NULL
pop_densities <- NULL
zone_trips <- list()
pop_densities_unique <- c(296, 304, 255, 307, 280)
for (a_file in file_names) {
  
  data_path <- paste("G:/My Drive/trip bias paper/scenarios/joint_prob/",a_file,".csv",sep="")
  data_info <- read.csv(data_path)
  
  obs_1 <- which(data_info$capture_1==1 & data_info$capture_2==0)
  obs_2 <- which(data_info$capture_1==0 & data_info$capture_2==1)
  obs_12 <- which(data_info$capture_1==1 & data_info$capture_2==1)
  all_obs <- c(obs_1,obs_2,obs_12)
  obs_all_count <- length(all_obs)
  
  obs_counts <- c(obs_counts, obs_all_count)
  obs_overlap_count <- c(obs_overlap_count, length(obs_12))
  obs_2_all <- c(obs_2_all,length(which(data_info$capture_2==1)))
  obs_1_all <- c(obs_1_all,length(which(data_info$capture_1==1)))
  
  obs_den_1 <- data_info$popu_density[which(data_info$capture_1 == 1)]
  obs_den_2 <- data_info$popu_density[which(data_info$capture_2 == 1)]
  missing_den <-  data_info$popu_density[which(data_info$capture_2 == 0 & data_info$capture_1 == 0)]
  den_all <- data_info$popu_density
  t_1 <- wilcox.test(obs_den_1,den_all)$p.value
  t_2 <- wilcox.test(obs_den_2,den_all)$p.value
  t_missing <- wilcox.test(missing_den,den_all)$p.value
  pop_densities <- rbind(pop_densities,c(mean(obs_den_1),mean(obs_den_2),mean(missing_den),mean(den_all),t_1,t_2,t_missing))
  
  #pop_densities_unique <- unique(data_info$popu_density)
  obs_zones_counts_1 <- sapply(pop_densities_unique, function(x) length(which(data_info$capture_1==1 & data_info$popu_density==x)))
  obs_zones_counts_2 <- sapply(pop_densities_unique, function(x) length(which(data_info$capture_2==1 & data_info$popu_density==x)))
  obs_zones_counts_missing <- sapply(pop_densities_unique, function(x) length(which(data_info$capture_2==0 & data_info$capture_1==0 & data_info$popu_density==x)))
  truth_zone_counts <- sapply(pop_densities_unique, function(x) length(which(data_info$popu_density==x)))
  zone_trips[[a_file]] <- cbind(obs_zones_counts_1,obs_zones_counts_2,obs_zones_counts_missing,truth_zone_counts)
}

par(mar=c(2,2,0.5,0.5))
a_file <- "scenario_high_low"
barplot(zone_trips[[a_file]][,4],names.arg=zone_trips[[a_file]][,4],cex.names=2)
cor.test(zone_trips[[a_file]][,1],zone_trips[[a_file]][,3])
cor.test(zone_trips[[a_file]][,2],zone_trips[[a_file]][,4])

results <- read.csv("G:/My Drive/trip bias paper/scenarios/zone_homogenity/n_a_interval.csv")

results[1,3] <- 72394

# results_modified <- NULL
# for (i in 1:15) {
#   results_modified <- cbind(results_modified,results[,i][which(results[,i] < quantile(results[,i],0.95) & results[,i] > quantile(results[,i],0.05))])
# }

# colMeans(results)
# apply(results,2,median)
# confidence_interval <- sapply(1:15,function(x) 1.96*sd(results[,x])/sqrt(obs_counts[x]))
# interval_95 <- apply(results,2,function(x) quantile(x,c(0.05,0.5,0.95)))

par(mar=c(4,9,1,1))
plot(results[2,1:6],1:6,pch=19,cex=1.3,ylab="",yaxt="n",xlab="",xlim=c(0,75000),ylim=c(0.5,6.5))
for (i in 1:6) {
  par(new=T)
  segments(results[1,i],i,results[3,i],i)
}
par(new=T)
plot(obs_counts[1:6],1:6,pch=0,cex=1.3,ylab="",yaxt="n",xlab="",xlim=c(0,75000),ylim=c(0.5,6.5))
par(new=T)
plot(obs_overlap_count[1:6], 1:6, pch=8,cex=1.3,ylab="",yaxt="n",xlab="",xlim=c(0,75000),ylim=c(0.5,6.5))
abline(v=16078,col="red",lty=2)
mtext("Total number of trips",1, line=2.2,cex=1.3)
# axis(2,at=1:15,labels=c("Baseline, uniform","Baseline,left-skewed","Baseline,right-skewed",
#                         "Scenario 1, uniform","Scenario 1,left-skewed","Scenario 1,right-skewed",
#                         "Scenario 2, uniform\noverlapping=0.1","Scenario 2,left-skewed\noverlapping=0.1","Scenario 2,right-skewed\noverlapping=0.1",
#                         "Scenario 2, uniform\noverlapping=0.5","Scenario 2,left-skewed\noverlapping=0.5","Scenario 2,right-skewed\noverlapping=0.5",
#                         "Scenario 2, uniform\noverlapping=0.9","Scenario 2,left-skewed\noverlapping=0.9","Scenario 2,right-skewed\noverlapping=0.9"),las=1)
# legend(6000,15.4,pch=c(19,NA,NA),lty=c(NA,1,2),col=c("black","black","red"),legend=c("Estimated number of trips","95% Confidence interval","Ground truth"))
axis(2,at=1:6,labels=c(expression(atop("P(1,2)" %in% "[0, 0.2]", "P(-1,-2)" %in% "[0, 0.2]")),
                       expression(atop("P(1,2)" %in% "[0, 0.2]", "P(-1,-2)" %in% "[0, 1]")),
                       expression(atop("P(1,2)" %in% "[0, 0.2]", "P(-1,-2)" %in% "[0.8, 1]")),
                       expression(atop("P(1,2)" %in% "[0, 1]", "P(-1,-2)" %in% "[0, 0.2]")),
                       expression(atop("P(1,2)" %in% "[0, 1]", "P(-1,-2)" %in% "[0, 1]")),
                       expression(atop("P(1,2)" %in% "[0.8, 1]", "P(-1,-2)" %in% "[0, 0.2]"))),las=1,cex.axis=1.1)
# axis(2,at=1:9,labels=c("Pr(2|1) = 0.1, Pr(2|-1) = 0.1","Pr(2|1) = 0.1, Pr(2|-1) = 0.5","Pr(2|1) = 0.1, Pr(2|-1) = 0.9",
#                        "Pr(2|1) = 0.5, Pr(2|-1) = 0.1","Pr(2|1) = 0.5, Pr(2|-1) = 0.5","Pr(2|1) = 0.5, Pr(2|-1) = 0.9",
#                        "Pr(2|1) = 0.9, Pr(2|-1) = 0.1","Pr(2|1) = 0.9, Pr(2|-1) = 0.5","Pr(2|1) = 0.9, Pr(2|-1) = 0.9"),las=1,cex.axis=1.1)
legend(34000,6.6,pch=c(19,0,8,NA),lty=c(NA,NA,NA,2),cex=0.9,col=c("black","black","black","red"),legend=c("Estimated number of trips","Number of observed trips","Number of overlapping trips","Ground truth"))

##plot for the relationships
missing_numbers <- 16078-obs_counts
overlapping_numbers <- obs_overlap_count
fit <- lm(results[2,] ~ overlapping_numbers+missing_numbers)
summary(fit)
cor.test(overlapping_numbers,results[2,])

plot(16078-obs_counts, obs_overlap_count, pch=19, xlab="Number of missing trips", ylab="Number of overlapping trips", cex.lab=1.3)

missing_percentages <- (16078-obs_counts)/16078
overlapping_percentages <- obs_overlap_count/16078
biases <- (16078-results[2,])/16078

cor.test(missing_percentages[2:9],abs(biases[2:9]))
cor.test(overlapping_percentages[2:9],abs(biases[2:9]))
plot(missing_percentages,abs(biases), pch=19, xlab="Number of missing trips", ylab="Number of overlapping trips", cex.lab=1.3)
plot(overlapping_percentages,abs(biases),pch=19,xlab="Number of overlapping trips", ylab="Estimated number of trips",cex.lab=1.3)
summary(lm(abs(biases)~missing_percentages))

par(mfrow=c(1,2))
par(mar=c(4.5,4.5,1,0.5))
plot(missing_percentages[2:9],abs(biases[2:9]),pch=19,xlab="Proportion of missing trips", ylab="Bias",cex.lab=1.3,xlim=c(0,0.38),ylim=c(0,0.038))
par(new=T)
plot(missing_percentages[1],abs(biases[1]),pch=19,xlab="", ylab="",cex.lab=1.3,xlim=c(0,0.38),ylim=c(0,0.038),col="red")
#abline(h=16078,col="red",lty=2)
plot(overlapping_percentages[2:9],abs(biases[2:9]),pch=19,xlab="Proportion of overlapping trips", ylab="",cex.lab=1.3,xlim=c(0.05,0.56),ylim=c(0,0.038))
par(new=T)
plot(overlapping_percentages[1],abs(biases[1]),pch=19,xlab="", ylab="",cex.lab=1.3,xlim=c(0.05,0.56),ylim=c(0,0.038),col="red")
#abline(h=16078,col="red",lty=2)


###Percent of missing trips recovered in each zone ---------------------------------------------------------------------------------------------------------------------
par(mar=c(4.5,4.5,1,0.5))
missing_trip_before <- truth_zone - obs_counts_zone
missing_trip_after <- truth_zone - results_zones
recovery_percentage <- (missing_trip_before - missing_trip_after)/missing_trip_before
plot(truth_zone, results_zones)
hist(results_zones, main="", breaks = seq(0, 4500, 300),ylim=c(0,15), xlab="Number of trips in a zone")
abline(v=3216,col="red",lty=2)
legend(0, 14, lty=2, col="red", legend="Ground truth")
length(which(abs((results_zones - truth_zone)/truth_zone) < 0.1 ))


###Regression on bias
truth_para <- NULL
est_para <- NULL
err_para <- NULL
t_value_para <- NULL
t_value_0 <- NULL
for (a_file in file_names) {
  result_path <- paste("G:/My Drive/trip bias paper/scenarios/joint_prob/results_with_density_1/",a_file,".csv",sep="")
  para_info <- read.csv(result_path)
  para_info$se_data[which(is.na(para_info$se_data))] <- 0
  
  data_path <- paste('G:/My Drive/trip bias paper/scenarios/joint_prob/',a_file,".csv",sep="")
  data_info <- read.csv(data_path)
  
  #data_info <- data_info[which(data_info$capture_1 == 1 | data_info$capture_2 == 1),]
  
  logit_1 <- log(data_info$p_1/(1 - data_info$p_1))
  fit_1 <- lm(logit_1 ~ data_info$popu_density)
  logit_2 <- log(data_info$p_2/(1 - data_info$p_2))
  fit_2 <- lm(logit_2 ~ data_info$popu_density)
  p_missing <- (1-data_info$p_1)*(1-data_info$p_2)
  logit_missing <- log(p_missing/(1-p_missing))
  fit_missing <- lm(p_missing ~ data_info$popu_density)
  sum1 <- summary(fit_1)
  sum2 <- summary(fit_2)
  summary(fit_missing)
  t_value_para <- cbind(t_value_para, c(sum1$coefficients[2,3], sum2$coefficients[2,3]))
  
  coef <- c(fit_1$coefficients[2], fit_2$coefficients[2])
  truth_para <- cbind(truth_para, coef)
  
  est_para <- cbind(est_para, c(para_info$para_data[2],para_info$para_data[4]))
  err_para <- cbind(err_para, c(para_info$se_data[2],para_info$se_data[4]))
  
  
}
(est_para - 0)/err_para
