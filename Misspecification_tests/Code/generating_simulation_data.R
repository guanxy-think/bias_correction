library(foreach)
set.seed(6)

num_people <- 4000
poisson_mean <- 4

num_trips <- rpois(4000, 4)
ppl_set <- rep(1:4000,num_trips)
trip_set <- foreach(i=1:4000, .combine='c') %do% {
  if (num_trips[i] != 0) 1:num_trips[i]
}

total_num_trips <- sum(num_trips)

pop_density <- rpois(5,283)
pop_density <- sort(pop_density)

##Baseline scenario
##Uniform distribution for p(i,a,1)
set.seed(1)
p_a_s_12 <- rbeta(total_num_trips,1,99)
p_a_s_1 <- runif(total_num_trips)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)
for (i in 1:total_num_trips) {
  if (capture_1[i] == 1 & capture_2[i] == 1) {
    if (p_a_s_1[i] > p_a_s_2[i]) {
      capture_2[i] <- 0
    } else {
      capture_1[i] <- 0
    }
  }
  
  if (capture_1[i] == 0 & capture_2[i] == 0) {
    if (p_a_s_1[i] > p_a_s_2[i]) {
      capture_1[i] <- 1
    } else {
      capture_2[i] <- 1
    }
  }
}

#generate population density
popu_density <- rep(NA,total_num_trips)
popu_density[which(p_a_s_1 <= quantile(p_a_s_1,0.2))] <- pop_density[1]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.2) & p_a_s_1 <= quantile(p_a_s_1,0.4))] <- pop_density[2]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.4) & p_a_s_1 <= quantile(p_a_s_1,0.6))] <- pop_density[3]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.6) & p_a_s_1 <= quantile(p_a_s_1,0.8))] <- pop_density[4]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.8))] <- pop_density[5]

length(which(capture_1==1))
length(which(capture_2==1))

dataset_1 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2,popu_density)
write.csv(dataset_1,"G:/My Drive/trip bias paper/scenarios/baseline_unif.csv",row.names=F)


##Baseline scenario
##Beta(2,5) for p(i,a,1)
set.seed(1)
p_a_s_12 <- rbeta(total_num_trips,1,99)
p_a_s_1 <- rbeta(total_num_trips,1,2)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)
for (i in 1:total_num_trips) {
  if (capture_1[i] == 1 & capture_2[i] == 1) {
    if (p_a_s_1[i] > p_a_s_2[i]) {
      capture_2[i] <- 0
    } else {
      capture_1[i] <- 0
    }
  }
  
  if (capture_1[i] == 0 & capture_2[i] == 0) {
    if (p_a_s_1[i] > p_a_s_2[i]) {
      capture_1[i] <- 1
    } else {
      capture_2[i] <- 1
    }
  }
}

#generate population density
popu_density <- rep(NA,total_num_trips)
popu_density[which(p_a_s_1 <= quantile(p_a_s_1,0.2))] <- pop_density[1]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.2) & p_a_s_1 <= quantile(p_a_s_1,0.4))] <- pop_density[2]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.4) & p_a_s_1 <= quantile(p_a_s_1,0.6))] <- pop_density[3]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.6) & p_a_s_1 <= quantile(p_a_s_1,0.8))] <- pop_density[4]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.8))] <- pop_density[5]

length(which(capture_1==1))
length(which(capture_2==1))

dataset_2 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2,popu_density)
write.csv(dataset_2,"G:/My Drive/trip bias paper/scenarios/baseline_beta_left.csv",row.names=F)


##Baseline scenario
##Beta(3,2) for p(i,a,1)
set.seed(1)
p_a_s_12 <- rbeta(total_num_trips,1,99)
p_a_s_1 <- rbeta(total_num_trips,2,1)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)
for (i in 1:total_num_trips) {
  if (capture_1[i] == 1 & capture_2[i] == 1) {
    if (p_a_s_1[i] > p_a_s_2[i]) {
      capture_2[i] <- 0
    } else {
      capture_1[i] <- 0
    }
  }
  
  if (capture_1[i] == 0 & capture_2[i] == 0) {
    if (p_a_s_1[i] > p_a_s_2[i]) {
      capture_1[i] <- 1
    } else {
      capture_2[i] <- 1
    }
  }
}

#generate population density
popu_density <- rep(NA,total_num_trips)
popu_density[which(p_a_s_1 <= quantile(p_a_s_1,0.2))] <- pop_density[1]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.2) & p_a_s_1 <= quantile(p_a_s_1,0.4))] <- pop_density[2]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.4) & p_a_s_1 <= quantile(p_a_s_1,0.6))] <- pop_density[3]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.6) & p_a_s_1 <= quantile(p_a_s_1,0.8))] <- pop_density[4]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.8))] <- pop_density[5]

length(which(capture_1==1))
length(which(capture_2==1))

dataset_3 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2,popu_density)
write.csv(dataset_3,"G:/My Drive/trip bias paper/scenarios/baseline_beta_right.csv",row.names=F)


##Scenario 1
##Uniform distribution for p(i,a,1)
set.seed(1)
p_a_s_12 <- rbeta(total_num_trips,1,99)
p_a_s_1 <- runif(total_num_trips)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)
for (i in 1:total_num_trips) {
  if (capture_1[i] == 1 & capture_2[i] == 1) {
    if (p_a_s_1[i] > p_a_s_2[i]) {
      capture_2[i] <- 0
    } else {
      capture_1[i] <- 0
    }
  }
}

#generate population density
popu_density <- rep(NA,total_num_trips)
popu_density[which(p_a_s_1 <= quantile(p_a_s_1,0.2))] <- pop_density[1]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.2) & p_a_s_1 <= quantile(p_a_s_1,0.4))] <- pop_density[2]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.4) & p_a_s_1 <= quantile(p_a_s_1,0.6))] <- pop_density[3]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.6) & p_a_s_1 <= quantile(p_a_s_1,0.8))] <- pop_density[4]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.8))] <- pop_density[5]

length(which(capture_1==1))
length(which(capture_2==1))

dataset_4 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2,popu_density)
write.csv(dataset_4,"G:/My Drive/trip bias paper/scenarios/scen1_unif.csv",row.names=F)

##Scenario 1
##Beta(1,6) for p(i,a,1)
set.seed(1)
p_a_s_12 <- rbeta(total_num_trips,1,99)
p_a_s_1 <- rbeta(total_num_trips,1,2)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)
for (i in 1:total_num_trips) {
  if (capture_1[i] == 1 & capture_2[i] == 1) {
    if (p_a_s_1[i] > p_a_s_2[i]) {
      capture_2[i] <- 0
    } else {
      capture_1[i] <- 0
    }
  }
}

#generate population density
popu_density <- rep(NA,total_num_trips)
popu_density[which(p_a_s_1 <= quantile(p_a_s_1,0.2))] <- pop_density[1]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.2) & p_a_s_1 <= quantile(p_a_s_1,0.4))] <- pop_density[2]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.4) & p_a_s_1 <= quantile(p_a_s_1,0.6))] <- pop_density[3]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.6) & p_a_s_1 <= quantile(p_a_s_1,0.8))] <- pop_density[4]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.8))] <- pop_density[5]

length(which(capture_1==1))
length(which(capture_2==1))

dataset_5 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2,popu_density)
write.csv(dataset_5,"G:/My Drive/trip bias paper/scenarios/scen1_beta_left.csv",row.names=F)

##Scenario 1
##Beta(6,1) for p(i,a,1)
set.seed(1)
p_a_s_12 <- rbeta(total_num_trips,1,99)
p_a_s_1 <- rbeta(total_num_trips,2,1)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)
for (i in 1:total_num_trips) {
  if (capture_1[i] == 1 & capture_2[i] == 1) {
    if (p_a_s_1[i] > p_a_s_2[i]) {
      capture_2[i] <- 0
    } else {
      capture_1[i] <- 0
    }
  }
}

#generate population density
popu_density <- rep(NA,total_num_trips)
popu_density[which(p_a_s_1 <= quantile(p_a_s_1,0.2))] <- pop_density[1]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.2) & p_a_s_1 <= quantile(p_a_s_1,0.4))] <- pop_density[2]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.4) & p_a_s_1 <= quantile(p_a_s_1,0.6))] <- pop_density[3]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.6) & p_a_s_1 <= quantile(p_a_s_1,0.8))] <- pop_density[4]
popu_density[which(p_a_s_1 > quantile(p_a_s_1,0.8))] <- pop_density[5]

length(which(capture_1==1))
length(which(capture_2==1))

dataset_6 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2,popu_density)
write.csv(dataset_6,"G:/My Drive/trip bias paper/scenarios/scen1_beta_right.csv",row.names=F)


#later
##Scenario 2------------------------------------------------------------------------------------------------------------------------------------------
##p_a_s_12 = 0.1
##Beta(1,6) for p(i,a,1)
set.seed(1)
p_a_s_12 <- 0.1
p_a_s_1 <- rbeta(total_num_trips,1,6)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)

table(capture_1,capture_2)

dataset_7 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2)
write.csv(dataset_7,"G:/My Drive/trip bias paper/scenarios/scen2_0.1_beta_left.csv",row.names=F)


##Scenario 2
##p_a_s_12 = 0.1
##Uniform for p(i,a,1)
set.seed(1)
p_a_s_12 <- 0.1
p_a_s_1 <- runif(total_num_trips)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)

table(capture_1,capture_2)

dataset_8 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2)
write.csv(dataset_8,"G:/My Drive/trip bias paper/scenarios/scen2_0.1_unif.csv",row.names=F)


##Scenario 2
##p_a_s_12 = 0.1
##Beta(6,1) for p(i,a,1)
set.seed(1)
p_a_s_12 <- 0.1
p_a_s_1 <- rbeta(total_num_trips,6,1)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)

table(capture_1,capture_2)

dataset_9 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2)
write.csv(dataset_9,"G:/My Drive/trip bias paper/scenarios/scen2_0.1_beta_right.csv",row.names=F)


##Scenario 2
##p_a_s_12 = 0.5
##Beta(1,6) for p(i,a,1)
set.seed(1)
p_a_s_12 <- 0.5
p_a_s_1 <- rbeta(total_num_trips,1,6)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)

table(capture_1,capture_2)

dataset_10 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2)
write.csv(dataset_10,"G:/My Drive/trip bias paper/scenarios/scen2_0.5_beta_left.csv",row.names=F)


##Scenario 2
##p_a_s_12 = 0.5
##Uniform for p(i,a,1)
set.seed(1)
p_a_s_12 <- 0.5
p_a_s_1 <- runif(total_num_trips)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)

table(capture_1,capture_2)

dataset_11 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2)
write.csv(dataset_11,"G:/My Drive/trip bias paper/scenarios/scen2_0.5_unif.csv",row.names=F)


##Scenario 2
##p_a_s_12 = 0.5
##Beta(6,1) for p(i,a,1)
set.seed(1)
p_a_s_12 <- 0.5
p_a_s_1 <- rbeta(total_num_trips,6,1)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)

table(capture_1,capture_2)

dataset_12 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2)
write.csv(dataset_12,"G:/My Drive/trip bias paper/scenarios/scen2_0.5_beta_right.csv",row.names=F)


##Scenario 2
##p_a_s_12 = 0.9
##Beta(1,6) for p(i,a,1)
set.seed(1)
p_a_s_12 <- 0.9
p_a_s_1 <- rbeta(total_num_trips,1,6)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)

table(capture_1,capture_2)

dataset_13 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2)
write.csv(dataset_13,"G:/My Drive/trip bias paper/scenarios/scen2_0.9_beta_left.csv",row.names=F)


##Scenario 2
##p_a_s_12 = 0.9
##Uniform for p(i,a,1)
set.seed(1)
p_a_s_12 <- 0.9
p_a_s_1 <- runif(total_num_trips)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)

table(capture_1,capture_2)

dataset_14 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2)
write.csv(dataset_14,"G:/My Drive/trip bias paper/scenarios/scen2_0.9_unif.csv",row.names=F)


##Scenario 2
##p_a_s_12 = 0.9
##Beta(6,1) for p(i,a,1)
set.seed(1)
p_a_s_12 <- 0.9
p_a_s_1 <- rbeta(total_num_trips,6,1)
p_a_s_2 <- sapply(p_a_s_12/p_a_s_1, function(x) min(1,x))
rand_1 <- runif(total_num_trips)
rand_2 <- runif(total_num_trips)
capture_1 <- ifelse(p_a_s_1 > rand_1, 1, 0)
capture_2 <- ifelse(p_a_s_2 > rand_2, 1, 0)

table(capture_1,capture_2)

dataset_15 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_a_s_1,p_a_s_2)
write.csv(dataset_15,"G:/My Drive/trip bias paper/scenarios/scen2_0.9_beta_right.csv",row.names=F)

