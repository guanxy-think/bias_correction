library(foreach)
library(mc2d)
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
##p_1_2 = [0, 0.2]
##p_n1_n2 = [0, 0.2]
set.seed(1)
p_1_2 <- runif(total_num_trips, 0, 0.2)
p_n1_n2 <- runif(total_num_trips, 0, 0.2)

p_1 <- runif(total_num_trips, p_1_2, 1-p_n1_n2)
p_n1_2 <- 1 - p_1 - p_n1_n2
p_2 <- p_n1_2 + p_1_2

##Scenario 2
##p_1_2 = [0, 0.2]
##p_n1_n2 = [0, 1]
set.seed(1)
p_n1_n2 <- c()
p_1_2 <- c()
for (i in 1:total_num_trips) {
  while (T) {
    p_n1_n2_i <- runif(1,0,1)
    p_1_2_i <- runif(1,0,0.2)
    if (p_n1_n2_i + p_1_2_i <= 1) {
      p_n1_n2 <- c(p_n1_n2, p_n1_n2_i)
      p_1_2 <- c(p_1_2, p_1_2_i)
      break
    }
  }
}

p_1 <- runif(total_num_trips, p_1_2, 1-p_n1_n2)
p_n1_2 <- 1 - p_1 - p_n1_n2
p_2 <- p_n1_2 + p_1_2

##scenario 3
##p_1_2 = [0, 0.2]
##p_n1_n2 = [0.8, 1]
set.seed(1)
p_n1_n2 <- rtriang(total_num_trips, 0.8, 0.8, 1)
p_1_2 <- runif(total_num_trips, 0, 1-p_n1_n2)

p_1 <- runif(total_num_trips, p_1_2, 1-p_n1_n2)
p_n1_2 <- 1 - p_1 - p_n1_n2
p_2 <- p_n1_2 + p_1_2

##scenario 4
##p_1_2 = [0, 1]
##p_n1_n2 = [0, 0.2]
set.seed(1)
p_n1_n2 <- c()
p_1_2 <- c()
for (i in 1:total_num_trips) {
  while (T) {
    p_n1_n2_i <- runif(1,0,0.2)
    p_1_2_i <- runif(1,0,1)
    if (p_n1_n2_i + p_1_2_i <= 1) {
      p_n1_n2 <- c(p_n1_n2, p_n1_n2_i)
      p_1_2 <- c(p_1_2, p_1_2_i)
      break
    }
  }
}

p_1 <- runif(total_num_trips, p_1_2, 1-p_n1_n2)
p_n1_2 <- 1 - p_1 - p_n1_n2
p_2 <- p_n1_2 + p_1_2

##Scenario 5
##p_1_2 = [0,1]
##p_n1_n2 = [0,1]
set.seed(1)
p_1_2 <- rtriang(total_num_trips, 0, 0, 1)
#p_1_2 <- runif(total_num_trips, 0, 1)
p_n1_n2 <- runif(total_num_trips, 0, 1-p_1_2)

p_1 <- runif(total_num_trips, p_1_2, 1-p_n1_n2)
p_n1_2 <- 1 - p_1 - p_n1_n2
p_2 <- p_n1_2 + p_1_2

##Scenario 6
##p_1_2 = [0.8, 1]
##p_n1_n2 = [0, 0.2]
set.seed(1)
p_1_2 <- rtriang(total_num_trips, 0.8, 0.8, 1)
#p_1_2 <- runif(total_num_trips, 0, 1)
p_n1_n2 <- runif(total_num_trips, 0, 1-p_1_2)

p_1 <- runif(total_num_trips, p_1_2, 1-p_n1_n2)
p_n1_2 <- 1 - p_1 - p_n1_n2
p_2 <- p_n1_2 + p_1_2

###Repeat for each scenario
    set.seed(1)

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
    #file_name <- paste("G:/My Drive/trip bias paper/scenarios/joint_prob/scenario_",p_2_given_1,"_",p_2_given_not_1,".csv",sep="")
    file_name <- "G:/My Drive/trip bias paper/scenarios/joint_prob/scenario_high_low.csv"
    write.csv(dataset_1,file_name,row.names=F)


datas <- read.csv("G:/My Drive/trip bias paper/scenarios/conditional_overlap/scenario_0.9_0.9.csv")
table(datas$capture_1,datas$capture_2)

par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(p_1_2,p_n1_n2,xlab="P(1,2)",ylab="P(-1,-2)",xlim=c(0,1),ylim=c(0,1),cex.lab=1.6,cex.axis=1.5)
hist(p_1,main="",xlab="P(1)")
hist(p_2,main="",xlab="P(2)")
cor.test(p_1_2, p_n1_n2)

plot(p_1,p_2,xlab="P(1)",ylab="P(2)",xlim=c(0,1),ylim=c(0,1),cex.lab=1.8,cex.axis=1.7)
cor.test(p_1,p_2)
boxplot(p_2~popu_density,xlab="Pop density (ppl/mile^2)",ylab="P(2)",ylim=c(0,1),cex.lab=1.8,cex.axis=1.7)
cor.test(popu_density,p_2)
