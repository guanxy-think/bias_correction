library(foreach)
library(mc2d)
set.seed(16)

num_people <- 4000
poisson_mean <- 4

num_trips <- rpois(4000, 4)
ppl_set <- rep(1:4000,num_trips)
trip_set <- foreach(i=1:4000, .combine='c') %do% {
  if (num_trips[i] != 0) 1:num_trips[i]
}

total_num_trips <- sum(num_trips)

density_file <- read.csv("G:/My Drive/trip bias paper/data/population_density_county.csv")
max_den <- range(density_file$B01001_calc_PopDensity)[2]
min_den <- range(density_file$B01001_calc_PopDensity)[1]
pop_density <- runif(5,min_den,max_den)
#pop_density <- c(73.913, 22749.6157, 1.262625e+04, 2.501639e+02, 7.047764e+03)
pop_density <- sort(pop_density)

num_trips_zones <- round(total_num_trips*pop_density/sum(pop_density))   #number of trip proportional to pop density
num_trips_zones[4] <- num_trips_zones[4] - 1
num_trips_zones[5] <- num_trips_zones[5] - 1

density_trips <- rep(pop_density,num_trips_zones)

###solve for optional values of beta
small_repre <- c(0.01, 0.01, 0.01, 0.01, 0.01)
small_unrepre <- pop_density*0.1/max(pop_density)
large_unrepre <- pop_density*0.9/max(pop_density)
small_unrepre_rev <- (1/pop_density)*0.1/max(1/pop_density)
large_unrepre_rev <- (1/pop_density)*0.9/max(1/pop_density)

small_repre_logit <- log(small_repre/(1-small_repre))
small_unrepre_logit <- log(small_unrepre/(1-small_unrepre))
large_unrepre_logit <- log(large_unrepre/(1-large_unrepre))
small_unrepre_rev_logit <- log(small_unrepre_rev/(1-small_unrepre_rev))
large_unrepre_rev_logit <- log(large_unrepre_rev/(1-large_unrepre_rev))

beta_small_repre <- lm(small_repre_logit ~ pop_density)$coefficients
beta_small_unrepre <- lm(small_unrepre_logit ~ pop_density)$coefficients
beta_large_unrepre <- lm(large_unrepre_logit ~ pop_density)$coefficients
beta_small_unrepre_rev <- lm(small_unrepre_rev_logit ~ pop_density)$coefficients
beta_large_unrepre_rev <- lm(large_unrepre_rev_logit ~ pop_density)$coefficients

error_small_repre <- 0#abs(mean(beta_small_repre[1] + beta_small_repre[2]*pop_density))
error_small_unrepre <- 0#abs(mean(beta_small_unrepre[1] + beta_small_unrepre[2]*pop_density))
error_large_unrepre <- 0#abs(mean(beta_large_unrepre[1] + beta_large_unrepre[2]*pop_density))
error_small_unrepre_rev <- 0#abs(mean(beta_small_unrepre_rev[1] + beta_small_unrepre_rev[2]*pop_density))
error_large_unrepre_rev <- 0#abs(mean(beta_large_unrepre_rev[1] + beta_large_unrepre_rev[2]*pop_density))

dataset_init <- data.frame(ppl_set, trip_set, density_trips)

###Scenario 1
##DSP 1: small_repre
##DSP 2: small_unrepre
##DSP 3: large_unrepre
set.seed(16)
beta_1 <- beta_small_repre
beta_2 <- beta_small_unrepre
beta_3 <- beta_large_unrepre

error_1 <- error_small_repre
error_2 <- error_small_unrepre
error_3 <- error_large_unrepre 

util_1 <- exp(beta_1[1] + beta_1[2]*density_trips + rnorm(total_num_trips,0,error_1))
util_2 <- exp(beta_2[1] + beta_2[2]*density_trips + rnorm(total_num_trips,0,error_2))
util_3 <- exp(beta_3[1] + beta_3[2]*density_trips + rnorm(total_num_trips,0,error_3))

p_1 <- util_1/(1+util_1)
p_2 <- util_2/(1+util_2)
p_3 <- util_3/(1+util_3)

#-----------------------------------------------------------
# p_1_only <- p_1*(1-p_2)
# p_2_only <- p_2*(1-p_1)
# p_1_2 <- p_1*p_2
# probs <- cbind(p_1_only,p_2_only,p_1_2,p_3)
# capture_1 <- rep(NA,total_num_trips)
# capture_2 <- rep(NA,total_num_trips)
# capture_3 <- rep(NA,total_num_trips)
# for (i in 1:total_num_trips) {
#   cate <- sample(1:4, 1, prob=probs[i,]/sum(probs[i,]))
#   capture_1[i] <- ifelse(cate %in% c(1,3), 1, 0)
#   capture_2[i] <- ifelse(cate %in% c(2,3), 1, 0)
#   capture_3[i] <- ifelse(cate==4, 1, 0)
# }
#-------------------------------------------------------------

# rand_1 <- runif(total_num_trips, 0, 1)
# rand_2 <- runif(total_num_trips, 0, 1)
# rand_3 <- runif(total_num_trips, 0, 1)
# 
# capture_1 <- ifelse(p_1>rand_1, 1, 0)
# capture_2 <- ifelse(p_2>rand_2, 1, 0)
# capture_3 <- ifelse(p_3>rand_3, 1, 0)
# for (i in 1: total_num_trips) {
#   if ((capture_1[i] == 1 | capture_2[i] == 1) & capture_3[i] == 1) {
#     p_capture <- 1 - (1-p_1[i])*(1-p_2[i])
#     if (p_capture > p_3[i]) {
#       capture_3[i] <- 0
#     } else {
#       capture_1[i] <- 0
#       capture_2[i] <- 0
#     }
#   }
# 
#   if (capture_1[i] == 0 & capture_2[i] == 0 & capture_3[i] == 0) {
#     capture <- c(capture_1[i], capture_2[i], capture_3[i])
#     p <- c(p_1[i], p_2[i], p_3[i])
#     capture[which(p==max(p))] <- 1
# 
#     #can use referecne to avoid doing this for every variable
#     capture_1[i] <- capture[1]
#     capture_2[i] <- capture[2]
#     capture_3[i] <- capture[3]
#   }
# }
###----------------------------------------------------------------------------------
###Assign trips to zones directly
prop_1 <- large_unrepre
prop_2 <- large_unrepre_rev

prop_3_min <- sapply(1:5, function(i) max(0,1-prop_1[i]-prop_2[i]))
prop_3_max <- sapply(1:5, function(i) 1 - max(prop_1[i],prop_2[i]))
prop_3 <- sapply(1:5, function(i) mean(c(prop_3_min[i], prop_3_max[i])))

prop_1_2 <- prop_1 + prop_2 + prop_3 - 1

num_1 <- round(num_trips_zones*prop_1)
num_2 <- round(num_trips_zones*prop_2)
num_1_2 <- round(num_trips_zones*prop_1_2)
num_3 <- round(num_trips_zones*prop_3)

##rounding errors
for (i in 1:5) {
  diff <- num_1[i] + num_2[i] + num_3[i] - num_1_2[i] - num_trips_zones[i]
  if (diff != 0) {
    num_3[i] <- num_3[i] - diff
  }
}

capture_1 <- rep(0, total_num_trips)
capture_2 <- rep(0, total_num_trips)
capture_3 <- rep(0, total_num_trips)
for (i in 1:5) {
  den <- pop_density[i]
  indexes_den <- which(dataset_init$density_trips == den)
  
  if (num_1[i] != 0) {
    indexes_den_1 <- indexes_den[1:num_1[i]]
  } else {
    indexes_den_1 <- NULL
  }
  
  if (num_2[i] != 0) {
    indexes_den_2 <- indexes_den[(num_1[i] - num_1_2[i] + 1):(num_1[i] - num_1_2[i] + num_2[i])]
  } else {
    indexes_den_2 <- NULL
  }
  
  if (num_3[i] != 0) {
    indexes_den_3 <- indexes_den[(num_1[i] - num_1_2[i] + num_2[i] + 1): (num_1[i] - num_1_2[i] + num_2[i] + num_3[i])]
  } else {
    indexes_den_3 <- NULL
  }
  
  capture_1[indexes_den_1] <- 1
  capture_2[indexes_den_2] <- 1
  capture_3[indexes_den_3] <- 1
}


dataset_init <- data.frame(ppl_set, trip_set, density_trips, capture_1, capture_2, capture_3)
dataset_excluded <- dataset_init[which(!(dataset_init$capture_1==0 & dataset_init$capture_2==0 & dataset_init$capture_3==0)),]
summary(glm(dataset_excluded$capture_3~dataset_excluded$density_trips))
num_trips_zones_excluded <- sapply(pop_density, function(x) length(which(dataset_excluded$density_trips == x)))
capture_1_zones <- sapply(pop_density, function(x) length(which(dataset_excluded$capture_1 == 1 & dataset_excluded$density_trips == x)))
capture_1_zones/num_trips_zones_excluded
capture_2_zones <- sapply(pop_density, function(x) length(which(dataset_excluded$capture_2 == 1 & dataset_excluded$density_trips == x)))
capture_2_zones/num_trips_zones_excluded
capture_3_zones <- sapply(pop_density, function(x) length(which(dataset_excluded$capture_3 == 1 & dataset_excluded$density_trips == x)))
capture_3_zones/num_trips_zones_excluded

write.csv(dataset_init,"G:/My Drive/trip bias paper/scenarios/representativeness/LU_LURe.csv",row.names=F)


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
