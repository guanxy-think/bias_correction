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

##Scenario 1
for (p_2_given_1 in c(0.1,0.5,0.9)) {
  for (p_2_given_not_1 in c(0.1,0.5,0.9)) {
    set.seed(1)
    p_1 <- rbeta(total_num_trips,3,2)
    p_2 <- p_2_given_1*p_1 + p_2_given_not_1*(1-p_1)
    
    rand_1 <- runif(total_num_trips)
    rand_2 <- runif(total_num_trips)
    capture_1 <- ifelse(p_1 > rand_1, 1, 0)
    capture_2 <- ifelse(p_2 > rand_2, 1, 0)
    
    table(capture_1,capture_2)
    
    #generate population density
    popu_density <- rep(NA,total_num_trips)
    popu_density[which(p_1 <= quantile(p_1,0.2))] <- pop_density[1]
    popu_density[which(p_1 > quantile(p_1,0.2) & p_1 <= quantile(p_1,0.4))] <- pop_density[2]
    popu_density[which(p_1 > quantile(p_1,0.4) & p_1 <= quantile(p_1,0.6))] <- pop_density[3]
    popu_density[which(p_1 > quantile(p_1,0.6) & p_1 <= quantile(p_1,0.8))] <- pop_density[4]
    popu_density[which(p_1 > quantile(p_1,0.8))] <- pop_density[5]
    
    dataset_1 <- data.frame(ppl_set,trip_set,capture_1,capture_2,p_1,p_2,popu_density)
    file_name <- paste("G:/My Drive/trip bias paper/scenarios/conditional_overlap/scenario_",p_2_given_1,"_",p_2_given_not_1,".csv",sep="")
    write.csv(dataset_1,file_name,row.names=F)
  }
}

datas <- read.csv("G:/My Drive/trip bias paper/scenarios/conditional_overlap/scenario_0.9_0.9.csv")
table(datas$capture_1,datas$capture_2)
