###same as simulate_n_a.R
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
for (a_file in file_names) {
  result_path <- paste("G:/My Drive/trip bias paper/scenarios/joint_prob/results/",a_file,".csv",sep="")
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
  
  para_interval <- matrix(NA,10,3)
  para_interval[,1] <- para_info$para_data[1:10] - 1.96*para_info$se_data[1:10]
  para_interval[,2] <- para_info$para_data[1:10]
  para_interval[,3] <- para_info$para_data[1:10] + 1.96*para_info$se_data[1:10]
  
  est <- c()
  for (i in 1:3) {
    para_set <- para_interval[,i]
    
    par_1_all <- para_set[1:5] #rnorm(10000, para_info$para_data[1],para_std[1])
    #par_2_all <- para_info$para_data[2] #rnorm(10000, para_info$para_data[2],para_std[2])
    par_3_all <- para_set[6:10] #rtruncnorm(10000, 0, Inf, para_info$para_data[3],para_std[3])
    
    utility_zones_1 <- par_1_all #+ par_2_all*pop_densities_unique
    utility_zones_2 <- par_3_all#*pop_densities_unique
    p_zones_1 <- 1 - 1/(1+exp(utility_zones_1))
    p_zones_2 <- 1 - 1/(1+exp(utility_zones_2))
    # p_zones_1[which(p_zones_1 > 0.9999999999)] <- 0.9999999999
    # p_zones_2[which(p_zones_2 > 0.9999999999)] <- 0.9999999999
    # p_zones_1[which(p_zones_1 < 0.0000000001)] <- 0.0000000001
    # p_zones_2[which(p_zones_2 < 0.0000000001)] <- 0.0000000001
    
    n_a_zones <- obs_zones_counts/(p_zones_1 + p_zones_2 - p_zones_1*p_zones_2)
    est <- c(est,sum(n_a_zones))
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


###data analysis-----------------------------------------------------------------------------------------------------------------------------
obs_counts <- NULL
obs_overlap_count <- NULL
obs_2_all <- NULL
obs_1_all <- NULL
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
}

results <- read.csv("G:/My Drive/trip bias paper/scenarios/zone_homogenity/n_a_interval.csv")

# results_modified <- NULL
# for (i in 1:15) {
#   results_modified <- cbind(results_modified,results[,i][which(results[,i] < quantile(results[,i],0.95) & results[,i] > quantile(results[,i],0.05))])
# }

# colMeans(results)
# apply(results,2,median)
# confidence_interval <- sapply(1:15,function(x) 1.96*sd(results[,x])/sqrt(obs_counts[x]))
# interval_95 <- apply(results,2,function(x) quantile(x,c(0.05,0.5,0.95)))

par(mar=c(4,13,1,1))
plot(results[2,1:4],1:4,pch=19,cex=1.3,ylab="",yaxt="n",xlab="",xlim=c(0,17500),ylim=c(0.5,10))
for (i in 1:3) {
  par(new=T)
  segments(results[1,i],i,results[3,i],i)
}
par(new=T)
plot(obs_counts[1:4],1:4,pch=0,cex=1.3,ylab="",yaxt="n",xlab="",xlim=c(0,17500),ylim=c(0.5,10))
abline(v=16078,col="red",lty=2)
mtext("Total number of trips",1, line=2.2,cex=1.3)
# axis(2,at=1:15,labels=c("Baseline, uniform","Baseline,left-skewed","Baseline,right-skewed",
#                         "Scenario 1, uniform","Scenario 1,left-skewed","Scenario 1,right-skewed",
#                         "Scenario 2, uniform\noverlapping=0.1","Scenario 2,left-skewed\noverlapping=0.1","Scenario 2,right-skewed\noverlapping=0.1",
#                         "Scenario 2, uniform\noverlapping=0.5","Scenario 2,left-skewed\noverlapping=0.5","Scenario 2,right-skewed\noverlapping=0.5",
#                         "Scenario 2, uniform\noverlapping=0.9","Scenario 2,left-skewed\noverlapping=0.9","Scenario 2,right-skewed\noverlapping=0.9"),las=1)
# legend(6000,15.4,pch=c(19,NA,NA),lty=c(NA,1,2),col=c("black","black","red"),legend=c("Estimated number of trips","95% Confidence interval","Ground truth"))
axis(2,at=1:4,labels=c("Low-Low","Low-Mid","Low-High",
                       "Mid-Low"),las=1,cex.axis=1.1)
axis(2,at=1:9,labels=c("Pr(2|1) = 0.1, Pr(2|-1) = 0.1","Pr(2|1) = 0.1, Pr(2|-1) = 0.5","Pr(2|1) = 0.1, Pr(2|-1) = 0.9",
                       "Pr(2|1) = 0.5, Pr(2|-1) = 0.1","Pr(2|1) = 0.5, Pr(2|-1) = 0.5","Pr(2|1) = 0.5, Pr(2|-1) = 0.9",
                       "Pr(2|1) = 0.9, Pr(2|-1) = 0.1","Pr(2|1) = 0.9, Pr(2|-1) = 0.5","Pr(2|1) = 0.9, Pr(2|-1) = 0.9"),las=1,cex.axis=1.1)
legend(9850,10.15,pch=c(19,NA,0),lty=c(NA,2,NA),cex=0.9,col=c("black","red","black"),legend=c("Estimated number of trips","Ground truth","Number of observed trips"))

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



