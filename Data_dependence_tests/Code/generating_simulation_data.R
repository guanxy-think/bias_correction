### first generate 4000 individuals and randomly assign imcome to them.
### then calculate the number of people in each zone based on pop density
### then assign the individuals to zones based on the numbers and income
library(foreach)
library(mc2d)
set.seed(16)

num_people <- 4000
ppl_set <- 1:4000

## Income follows gamma distribution with mean $35672
#income_data <- read.csv("county_income.csv")
# mean_income <- 35.672 #mean(income_data$B19301e1)
# var_income <- 51.93392 #var(income_data$B19301e1/1000)
# shape_para <- mean_income^2/var_income
# scale_para <- var_income/mean_income
# set.seed(1)
# income_indiv_1 <- rgamma(nrow(dataset_init), shape=shape_para, scale=scale_para)
# income_zones <- c(27.65726, 28.75588, 24.68900, 24.54322, 36.59123, 38.29400, 18.66728, 42.25576, 21.69917, 30.88431)
threshold <- c(0,seq(2.499,100,by=2.5))
counts <- read.csv("income_tier.csv")$Count
people_counts <- round(num_people*counts/sum(counts))
people_counts[c(38,40)] <- people_counts[c(38,40)]+1
people_income <- c()
for (i in 1:40) {
  people_income_tier <- runif(people_counts[i],threshold[i],threshold[i+1])
  people_income <- c(people_income, people_income_tier)
}
mean_income <- 35.672 #mean(income_data$B19301e1)
var_income <- 51.93392 #var(income_data$B19301e1/1000)
shape_para <- mean_income^2/var_income
scale_para <- var_income/mean_income
set.seed(1)
income_high <- rlnorm(people_counts[41],log(mean_income),log(sqrt(var_income)))+100
people_income <- c(people_income, income_high)
people_income <- sample(people_income, num_people)


density_file <- read.csv("population_density_county.csv")
max_den <- range(density_file$B01001_calc_PopDensity)[2]
min_den <- range(density_file$B01001_calc_PopDensity)[1]
#pop_density <- runif(10,min_den,max_den)
zone_density <- c(473.913*4, 961.6157*4, 1.262625e+04, 5.501639e+02*4, 7.047764e+03, 24902.198,  5715.468, 14748.741, 12893.441, 21729.096)
zone_density <- sort(zone_density)/1000

zone_num_people <- round(num_people*zone_density/sum(zone_density))   #number of trip proportional to pop density
zone_num_people[1] <- zone_num_people[1] + 1
people_density <- rep(zone_density,zone_num_people)


### Rearrange to negatively correlated income and density
set.seed(1)
cor_mat <- matrix(-0.2, 2, 2)
diag(cor_mat) <- 1
library(MASS)
mvdat <- mvrnorm(n = num_people, mu = c(0, 0), Sigma = cor_mat, empirical = TRUE)
rx <- rank(mvdat[ , 1], ties.method = "first")
ry <- rank(mvdat[ , 2], ties.method = "first")
people_density_sorted <- sort(people_density)
people_income_sorted <- sort(people_income)

people_density <- people_density_sorted[rx]
people_income <- people_income_sorted[ry]
##check
cor.test(people_density, people_income)
people_density[which(people_income==max(people_income))]
zone_income <- sapply(zone_density, function(x) mean(people_income[which(people_density==x)]))
cor.test(zone_density, zone_income)

###Generate trips
###Pay attention that some people may make 0 trips #########
poisson_mean <- 4
people_num_trips <- rpois(4000, poisson_mean)
trip_set <- foreach(i=1:4000, .combine='c') %do% {
  if (people_num_trips[i] != 0) 1:people_num_trips[i]
}
trip_density <- rep(people_density, people_num_trips)
trip_income <- rep(people_income, people_num_trips)

zone_trips <- sapply(zone_density, function(x) length(which(trip_density==x)))

total_num_trips <- sum(people_num_trips)


###solve for optional values of beta
small_repre <- rep(0.01, 10)
small_unrepre <- zone_density*0.1/max(zone_density)
large_unrepre <- zone_density*0.9/max(zone_density)
small_unrepre_rev <- (1/zone_density)*0.1/max(1/zone_density)
large_unrepre_rev <- (1/zone_density)*0.9/max(1/zone_density)

small_repre_logit <- log(small_repre/(1-small_repre))
small_unrepre_logit <- log(small_unrepre/(1-small_unrepre))
large_unrepre_logit <- log(large_unrepre/(1-large_unrepre))
small_unrepre_rev_logit <- log(small_unrepre_rev/(1-small_unrepre_rev))
large_unrepre_rev_logit <- log(large_unrepre_rev/(1-large_unrepre_rev))

beta_small_repre <- lm(small_repre_logit ~ zone_density)$coefficients
beta_small_unrepre <- lm(small_unrepre_logit ~ zone_density)$coefficients
beta_large_unrepre <- lm(large_unrepre_logit ~ zone_density)$coefficients
beta_small_unrepre_rev <- lm(small_unrepre_rev_logit ~ zone_density)$coefficients
beta_large_unrepre_rev <- lm(large_unrepre_rev_logit ~ zone_density)$coefficients

error_small_repre <- 0#abs(mean(beta_small_repre[1] + beta_small_repre[2]*pop_density))
error_small_unrepre <- 0#abs(mean(beta_small_unrepre[1] + beta_small_unrepre[2]*pop_density))
error_large_unrepre <- 0#abs(mean(beta_large_unrepre[1] + beta_large_unrepre[2]*pop_density))
error_small_unrepre_rev <- 0#abs(mean(beta_small_unrepre_rev[1] + beta_small_unrepre_rev[2]*pop_density))
error_large_unrepre_rev <- 0#abs(mean(beta_large_unrepre_rev[1] + beta_large_unrepre_rev[2]*pop_density))

###Scenario 1
##DSP 1: small_repre
##DSP 2: small_unrepre
##DSP 3: large_unrepre
set.seed(16)
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
prop_1 <- small_repre
prop_2 <- small_unrepre

prop_3_min <- sapply(1:10, function(i) max(0,1-prop_1[i]-prop_2[i]))
prop_3_max <- sapply(1:10, function(i) 1 - max(prop_1[i],prop_2[i]))
prop_3 <- sapply(1:10, function(i) mean(c(prop_3_min[i], prop_3_max[i])))

prop_1_2 <- prop_1 + prop_2 + prop_3 - 1

num_1 <- round(zone_trips*prop_1)
num_2 <- round(zone_trips*prop_2)
num_1_2 <- round(zone_trips*prop_1_2)
num_3 <- round(zone_trips*prop_3)

##rounding errors
for (i in 1:10) {
  diff <- num_1[i] + num_2[i] + num_3[i] - num_1_2[i] - zone_trips[i]
  if (diff > 0) {
    num_3[i] <- num_3[i] - diff
  }
  if (diff < 0) {
    num_1_2[i] <- num_1_2[i] - diff
  }
}

capture_1 <- rep(0, total_num_trips)
capture_2 <- rep(0, total_num_trips)
capture_3 <- rep(0, total_num_trips)
for (i in 1:10) {
  den <- zone_density[i]
  indexes_den <- which(trip_density == den)
  
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

#write.csv(dataset_init,"SR_SU.csv",row.names=F)

### Regenerate captures based on beta coefficients. For checking internal validity purpose
# obs_1_zones_count <- sapply(zone_density, function(x) length(which(capture_1==1 & trip_density==x)))
# obs_2_zones_count <- sapply(zone_density, function(x) length(which(capture_2==1 & trip_density==x)))
# ground_truth <- sapply(zone_density, function(x) length(which(trip_density==x)))

# obs_1_zones_prop <- obs_1_zones_count/ground_truth
# obs_2_zones_prop <- obs_2_zones_count/ground_truth
# 
# obs_1_zones_logit <- obs_1_zones_prop/(1 - obs_1_zones_prop)
# obs_2_zones_logit <- obs_2_zones_prop/(1 - obs_2_zones_prop)

fit_lm <- glm(people_num_trips ~ people_density, family="poisson")$coefficients#lm(log(ground_truth) ~ zone_density)$coefficients
fit_1_lm <- glm(capture_1 ~ trip_density, family="binomial")$coefficients#lm(log(obs_1_zones_logit) ~ zone_density)$coefficients
fit_2_lm <- glm(capture_2 ~ trip_density, family="binomial")$coefficients#lm(log(obs_2_zones_logit) ~ zone_density)$coefficients

income_coef <- -fit_lm[2]*mean(people_density)/mean(people_income)
constant <- fit_lm[1] - mean(income_coef*people_income)
income_coef_1 <- fit_1_lm[2]*mean(trip_density)/mean(trip_income)
constant_1 <- fit_1_lm[1] - mean(income_coef_1*trip_income)
income_coef_2 <- fit_2_lm[2]*mean(trip_density)/mean(trip_income)
constant_2 <- fit_2_lm[1] - mean(income_coef_2*trip_income)

###adjust the coefficients
new_density_coef <- fit_lm[2]*4000
new_income_coef <- income_coef*4000
new_constant <- median(log(people_num_trips)) - 0.3 - new_density_coef*median(people_density) - new_income_coef*median(people_income)
lambda_new <- exp(new_constant + new_density_coef*people_density + new_income_coef*people_income)
summary(lambda_new)
people_trip_new <- rpois(4000, lambda_new)
summary(people_trip_new)
###

trip_density_new <- rep(people_density, people_trip_new)
trip_income_new <- rep(people_income, people_trip_new)

utility_trips_1 <- constant_1 + fit_1_lm[2]*trip_density_new + income_coef_1*trip_income_new
utility_trips_2 <- constant_2 + fit_2_lm[2]*trip_density_new + income_coef_2*trip_income_new
p_trips_1 <- 1 - 1/(1+exp(utility_trips_1))
p_trips_2 <- 1 - 1/(1+exp(utility_trips_2))

set.seed(1)
rand_unif_1 <- runif(length(p_trips_1),0,1)
rand_unif_2 <- runif(length(p_trips_2),0,1)

capture_1_new <- ifelse(p_trips_1 > rand_unif_1, 1, 0)
capture_2_new <- ifelse(p_trips_2 > rand_unif_2, 1, 0)

dataset_init_new <- data.frame(trip_density_new,trip_income_new,capture_1_new,capture_2_new)
dataset_init_new <- dataset_init_new[order(dataset_init_new$trip_density_new, dataset_init_new$trip_income_new),]
dataset_zone_incomes <- cbind(people_density, people_income)


write.csv(dataset_init_new,"SR_SU_regen_with_income_indiv.csv",row.names=F)
write.csv(dataset_zone_incomes,"zone_incomes_regen_with_income_indiv.csv",row.names=F)





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
    #file_name <- paste("scenario_",p_2_given_1,"_",p_2_given_not_1,".csv",sep="")
    file_name <- "scenario_high_low.csv"
    write.csv(dataset_1,file_name,row.names=F)


datas <- read.csv("scenario_0.9_0.9.csv")
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
