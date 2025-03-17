### first generate 4000 individuals and randomly assign imcome to them.
### then calculate the number of people in each zone based on pop density
### then assign the individuals to zones based on the numbers and income
library(foreach)
library(mc2d)
set.seed(16)

num_people <- 4000
ppl_set <- 1:4000

## Income follows gamma distribution with mean $35672 (https://doi.org/10.2307/1914221, https://data.census.gov/cedsci/table?q=average%20income&tid=ACSSPP1Y2019.S0201)
## individual income by tiers: https://www.census.gov/data/tables/time-series/demo/income-poverty/cps-pinc.html (search "us individual income distribution")
threshold <- c(0,seq(2.499,100,by=2.5))
counts <- read.csv("G:/My Drive/trip bias paper/data/income_tier.csv")$Count
people_counts <- round(num_people*counts/sum(counts))
people_counts[c(38,40)] <- people_counts[c(38,40)]+1
people_income <- c()
for (i in 1:40) {
  people_income_tier <- runif(people_counts[i],threshold[i],threshold[i+1])
  people_income <- c(people_income, people_income_tier)
}
# Gamma distribution for the highest-income tier
mean_income <- 35.672 #mean(income_data$B19301e1)
var_income <- 51.93392 #var(income_data$B19301e1/1000)
shape_para <- mean_income^2/var_income
scale_para <- var_income/mean_income
set.seed(1)
income_high <- rlnorm(people_counts[41],log(mean_income),log(sqrt(var_income)))+100
people_income <- c(people_income, income_high)
people_income <- sample(people_income, num_people)   #randomize the order of income


## Density data
density_file <- read.csv("G:/My Drive/trip bias paper/data/population_density_county.csv")
max_den <- range(density_file$B01001_calc_PopDensity)[2]
min_den <- range(density_file$B01001_calc_PopDensity)[1]
#zone_density <- runif(10,min_den,max_den)
zone_density <- c(473.913*4, 961.6157*4, 1.262625e+04, 5.501639e+02*4, 7.047764e+03, 24902.198,  5715.468, 14748.741, 12893.441, 21729.096)
zone_density <- sort(zone_density)/1000

## Assign people to zones
##random assign
# people_density <- sample(zone_density, num_people, replace = T)
## number of trips in a zone proportional to population density
zone_num_people <- round(num_people*zone_density/sum(zone_density))   #number of trip proportional to pop density
zone_num_people[1] <- zone_num_people[1] + 1
people_density <- rep(zone_density,zone_num_people)
## the number of people in each zone is a random number
# zone_num_people <- rlnorm(10,log(400))
# zone_num_people <- zone_num_people*num_people/sum(zone_num_people)
# zone_num_people <- round(zone_num_people)
# zone_num_people[7] <- zone_num_people[7] - 1
# people_density <- rep(zone_density,zone_num_people)

### Rearrange to negatively correlated income and density
### https://stats.stackexchange.com/questions/134164/how-to-rearrange-2d-data-to-get-given-correlation (search "rearange two variables to have some correlation")
set.seed(1)
cor_mat <- matrix(-0.8, 2, 2)    ##########-0.8 or -0.2
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
zone_income <- sapply(zone_density, function(x) mean(people_income[which(people_density==x)]))    ###### mean or median income
cor.test(zone_density, zone_income)

people_num_trips <- rpois(4000, 4)
trip_density <- rep(people_density, people_num_trips)
trip_income <- rep(people_income, people_num_trips)

num_trips_by_zone <- sapply(zone_density, function(x) length(which(trip_density==x)))

total_num_trips <- sum(people_num_trips)

# ## Travel time data
# trip_file <- read.csv("G:/Shared drives/2017 NHTS/Original data and documentation/Csv/trippub.csv")
# trip_travel_time <- sample(trip_file$TRVLCMIN, length(trip_income), replace=T)
# 
# dataset <- data.frame(trip_density, trip_income, trip_travel_time)

### scenario 2
## Random assign trips in each zone. This is not used.
num_trips_by_zone_capture_1 <- round(num_trips_by_zone*0.01)
percent_capture_2 <- 0.1*zone_density/max(zone_density)
num_trips_by_zone_capture_2 <- round(num_trips_by_zone*percent_capture_2)

capture_scen2_DSP1 <- rep(0,length(trip_income))
capture_scen2_DSP2 <- rep(0,length(trip_income))

for (i in 1:10) {
  density_i <- zone_density[i]
  num_cap_1 <- num_trips_by_zone_capture_1[i]
  num_cap_2 <- num_trips_by_zone_capture_2[i]
  
  index_cap_1 <- sample(which(trip_density == density_i),num_cap_1)
  index_cap_2 <- sample(which(trip_density == density_i),num_cap_2)
  
  capture_scen2_DSP1[index_cap_1] <- 1
  capture_scen2_DSP2[index_cap_2] <- 1
}

dataset <- data.frame(trip_density, trip_income)
dataset$capture_1 <- capture_scen2_DSP1
dataset$capture_2 <- capture_scen2_DSP2

#test
pop_densities_unique <- sort(unique(dataset$trip_density))
obs_zones_counts <- sapply(pop_densities_unique, function(x) length(which((dataset$capture_1==1 | dataset$capture_2==1) & dataset$trip_density==x)))
ground_truth <- sapply(pop_densities_unique, function(x) length(which(dataset$trip_density==x)))
#

write.csv(dataset, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/scenario2(-0.03)_noPredictor.csv")
##

## Assign trips based on parameterization
set.seed(16)      ###########
prop_1 <- rep(0.01, 10)
prop_2 <- zone_density*0.1/max(zone_density)

prop_3_min <- sapply(1:10, function(i) max(0,1-prop_1[i]-prop_2[i]))
prop_3_max <- sapply(1:10, function(i) 1 - max(prop_1[i],prop_2[i]))
prop_3 <- sapply(1:10, function(i) mean(c(prop_3_min[i], prop_3_max[i])))

prop_1_2 <- prop_1 + prop_2 + prop_3 - 1

num_1 <- round(num_trips_by_zone*prop_1)
num_2 <- round(num_trips_by_zone*prop_2)
num_1_2 <- round(num_trips_by_zone*prop_1_2)
num_3 <- round(num_trips_by_zone*prop_3)

#rounding errors
for (i in 1:10) {
  diff <- num_1[i] + num_2[i] + num_3[i] - num_1_2[i] - num_trips_by_zone[i]
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

#### method 1: only population density is significant. Not used in the final version.
fit_lm <- glm(people_num_trips ~ people_density, family="poisson")$coefficients#lm(log(ground_truth) ~ zone_density)$coefficients
fit_1_lm <- glm(capture_1 ~ trip_density, family="binomial")$coefficients#lm(log(obs_1_zones_logit) ~ zone_density)$coefficients
fit_2_lm <- glm(capture_2 ~ trip_density, family="binomial")$coefficients#lm(log(obs_2_zones_logit) ~ zone_density)$coefficients

income_coef <- -fit_lm[2]*mean(people_density)/mean(people_income)
constant <- fit_lm[1] - mean(income_coef*people_income)
income_coef_1 <- fit_1_lm[2]*mean(trip_density)/mean(trip_income)
constant_1 <- fit_1_lm[1] - mean(income_coef_1*trip_income)
income_coef_2 <- fit_2_lm[2]*mean(trip_density)/mean(trip_income)
constant_2 <- fit_2_lm[1] - mean(income_coef_2*trip_income)

###adjust the coefficients        ##########
##adjusting for correlation = -0.2, trial and error with summary(lambda_new) until a reasonal median, mean and max is achieved.
new_density_coef <- -fit_lm[2]*4000    ##########
new_income_coef <- income_coef*4000    ##########
new_constant <- median(log(people_num_trips)) - 0.3 -  new_density_coef*median(people_density) - new_income_coef*median(people_income)    ##########
##adjusting for correlation = -0.9
new_density_coef <- fit_lm[2]*45   ##########
new_income_coef <- income_coef*45    ##########
new_constant <- median(log(people_num_trips)) - 0.09 -  new_density_coef*median(people_density) - new_income_coef*median(people_income)    ##########

lambda_new <- exp(new_constant + new_density_coef*people_density + new_income_coef*people_income)    
summary(lambda_new)
###
####

#### method 2: population density and income are significant. Also to generate negative correlation between density and number of trips.
fit_lm <- glm(people_num_trips ~ people_density + people_income, family="poisson")$coefficients#lm(log(ground_truth) ~ zone_density)$coefficients
fit_1_lm <- glm(capture_1 ~ trip_density + trip_income, family="binomial")$coefficients#lm(log(obs_1_zones_logit) ~ zone_density)$coefficients
fit_2_lm <- glm(capture_2 ~ trip_density + trip_income, family="binomial")$coefficients#lm(log(obs_2_zones_logit) ~ zone_density)$coefficients
##adjusting for correlation = -0.2,trial and error with summary(lambda_new) until a reasonal median, mean and max is achieved.
new_density_coef <- fit_lm[2]*40000
new_income_coef <- -fit_lm[3]
new_constant <- fit_lm[1] + 1.07
##
##adjusting for correlation = -0.9
new_density_coef <- -fit_lm[2]*80
new_income_coef <- -fit_lm[3]
new_constant <- fit_lm[1] + 1.13
##
income_coef_1 <- fit_1_lm[3]
constant_1 <- fit_1_lm[1]
income_coef_2 <- fit_2_lm[3]
constant_2 <- fit_2_lm[1]
####

lambda_new <- exp(new_constant + new_density_coef*people_density + new_income_coef*people_income)    
summary(lambda_new)
people_trip_new <- rpois(4000, lambda_new)
summary(people_trip_new)


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

dataset <- data.frame(trip_density_new,trip_income_new,capture_1_new,capture_2_new)
dataset <- dataset[order(dataset$trip_density_new, dataset$trip_income_new),]

colnames(dataset) <- c("trip_density", "trip_income", "capture_1", "capture_2")

#test
pop_densities_unique <- sort(unique(dataset$trip_density))
obs_zones_counts <- sapply(pop_densities_unique, function(x) length(which((dataset$capture_1==1 | dataset$capture_2==1) & dataset$trip_density==x)))
ground_truth <- sapply(pop_densities_unique, function(x) length(which(dataset$trip_density==x)))
income_zone <- sapply(pop_densities_unique, function(x) median(unique(dataset$trip_income[which(dataset$trip_density==x)])))
fit_check <- lm(log(ground_truth) ~ pop_densities_unique+income_zone)

people_data <- table(dataset$trip_income,dataset$trip_density)
densities_pop <- as.numeric(rownames(people_data))
income_ <- as.numeric(colnames(people_data))
income_pop <- c()
count_pop <- c()
for (i in 1:3771) {
  non0index <- which(people_data[i,] != 0)
  income_pop <- c(income_pop, income_[non0index])
  count_pop <- c(count_pop, people_data[i,][non0index])
}
fit_check <- glm(count_pop ~ densities_pop + income_pop, family="poisson")  #######This does not gives correct coefficients because the regression does not include people with 0 trips.
#

write.csv(dataset, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/scenario2(-0.10)_negNumDensityCor.csv")



table(trip_density_new, capture_1_new)
table(trip_density_new, capture_2_new)


### scenario 3
coef_1 <- glm(dataset$capture_1 ~ dataset$trip_density + dataset$trip_income, family="binomial")$coefficients
coef_2 <- glm(dataset$capture_2 ~ dataset$trip_density + dataset$trip_income, family="binomial")$coefficients

coef_1_TT <- mean(dataset$trip_density)*coef_1[2]/mean(dataset$trip_travel_time)
coef_2_TT <- mean(dataset$trip_density)*coef_2[2]/mean(dataset$trip_travel_time)

## rearrange TT for certain correlations: https://stats.stackexchange.com/questions/134164/how-to-rearrange-2d-data-to-get-given-correlation (search "rearange two variables to have some correlation")
dataset_scen3 <- data.frame(trip_density, trip_income)
dataset_scen3_sorted <- dataset_scen3[order(dataset_scen3$trip_density),]
TT_sorted <- sort(trip_travel_time)

library(MASS)
cor_mat_1 <- matrix(c(1,0,0,1), 2, 2)
mvdat_1 <- mvrnorm(n = nrow(dataset), mu = c(0, 0), Sigma = cor_mat_1, empirical = TRUE)
rx_1 <- rank(mvdat_1[ , 1], ties.method = "first")
ry_1 <- rank(mvdat_1[ , 2], ties.method = "first")
dataset_scen3_sorted_1 <- dataset_scen3_sorted[rx_1,]
TT_sorted_1 <- TT_sorted[ry_1]
dataset_scen3_sorted_1$trip_travel_time <- TT_sorted_1
cor.test(dataset_scen3_sorted_1$trip_density,dataset_scen3_sorted_1$trip_travel_time)

cor_mat_2 <- matrix(c(1,0.2,0.2,1), 2, 2)
mvdat_2 <- mvrnorm(n = nrow(dataset), mu = c(0, 0), Sigma = cor_mat_2, empirical = TRUE)
rx_2 <- rank(mvdat_2[ , 1], ties.method = "first")
ry_2 <- rank(mvdat_2[ , 2], ties.method = "first")
dataset_scen3_sorted_2 <- dataset_scen3_sorted[rx_2,]
TT_sorted_2 <- TT_sorted[ry_2]
dataset_scen3_sorted_2$trip_travel_time <- TT_sorted_2
cor.test(dataset_scen3_sorted_2$trip_density,dataset_scen3_sorted_2$trip_travel_time)

cor_mat_3 <- matrix(c(1,0.5,0.5,1), 2, 2)
mvdat_3 <- mvrnorm(n = nrow(dataset), mu = c(0, 0), Sigma = cor_mat_3, empirical = TRUE)
rx_3 <- rank(mvdat_3[ , 1], ties.method = "first")
ry_3 <- rank(mvdat_3[ , 2], ties.method = "first")
dataset_scen3_sorted_3 <- dataset_scen3_sorted[rx_3,]
TT_sorted_3 <- TT_sorted[ry_3]
dataset_scen3_sorted_3$trip_travel_time <- TT_sorted_3
cor.test(dataset_scen3_sorted_3$trip_density,dataset_scen3_sorted_3$trip_travel_time)

cor_mat_4 <- matrix(c(1,0.8,0.8,1), 2, 2)
mvdat_4 <- mvrnorm(n = nrow(dataset), mu = c(0, 0), Sigma = cor_mat_4, empirical = TRUE)
rx_4 <- rank(mvdat_4[ , 1], ties.method = "first")
ry_4 <- rank(mvdat_4[ , 2], ties.method = "first")
dataset_scen3_sorted_4 <- dataset_scen3_sorted[rx_4,]
TT_sorted_4 <- TT_sorted[ry_4]
dataset_scen3_sorted_4$trip_travel_time <- TT_sorted_4
cor.test(dataset_scen3_sorted_4$trip_density,dataset_scen3_sorted_4$trip_travel_time)

cor_mat_5 <- matrix(c(1,1,1,1), 2, 2)
mvdat_5 <- mvrnorm(n = nrow(dataset), mu = c(0, 0), Sigma = cor_mat_5, empirical = TRUE)
rx_5 <- rank(mvdat_5[ , 1], ties.method = "first")
ry_5 <- rank(mvdat_5[ , 2], ties.method = "first")
dataset_scen3_sorted_5 <- dataset_scen3_sorted[rx_5,]
TT_sorted_5 <- TT_sorted[ry_5]
dataset_scen3_sorted_5$trip_travel_time <- TT_sorted_5
cor.test(dataset_scen3_sorted_5$trip_density,dataset_scen3_sorted_5$trip_travel_time)

utility_1 <- coef_1[1] + coef_1[2]*dataset_scen3_sorted_1$trip_density + coef_1[3]*dataset_scen3_sorted_1$trip_income + coef_1_TT*dataset_scen3_sorted_1$trip_travel_time
utility_2 <- coef_2[1] + coef_2[2]*dataset_scen3_sorted_1$trip_density + coef_2[3]*dataset_scen3_sorted_1$trip_income + coef_2_TT*dataset_scen3_sorted_1$trip_travel_time
dataset_scen3_sorted_1$p_1 <- 1/(1+exp(-utility_1))
dataset_scen3_sorted_1$p_2 <- 1/(1+exp(-utility_2))
rand_num_1 <- runif(length(dataset_scen3_sorted_1$p_1))
rand_num_2 <- runif(length(dataset_scen3_sorted_1$p_2))
dataset_scen3_sorted_1$capture_1 <- ifelse(dataset_scen3_sorted_1$p_1 > rand_num_1, 1, 0)
dataset_scen3_sorted_1$capture_2 <- ifelse(dataset_scen3_sorted_1$p_2 > rand_num_2, 1, 0)

utility_1 <- coef_1[1] + coef_1[2]*dataset_scen3_sorted_2$trip_density + coef_1[3]*dataset_scen3_sorted_2$trip_income + coef_1_TT*dataset_scen3_sorted_2$trip_travel_time
utility_2 <- coef_2[1] + coef_2[2]*dataset_scen3_sorted_2$trip_density + coef_2[3]*dataset_scen3_sorted_2$trip_income + coef_2_TT*dataset_scen3_sorted_2$trip_travel_time
dataset_scen3_sorted_2$p_1 <- 1/(1+exp(-utility_1))
dataset_scen3_sorted_2$p_2 <- 1/(1+exp(-utility_2))
rand_num_1 <- runif(length(dataset_scen3_sorted_2$p_1))
rand_num_2 <- runif(length(dataset_scen3_sorted_2$p_2))
dataset_scen3_sorted_2$capture_1 <- ifelse(dataset_scen3_sorted_2$p_1 > rand_num_1, 1, 0)
dataset_scen3_sorted_2$capture_2 <- ifelse(dataset_scen3_sorted_2$p_2 > rand_num_2, 1, 0)

utility_1 <- coef_1[1] + coef_1[2]*dataset_scen3_sorted_3$trip_density + coef_1[3]*dataset_scen3_sorted_3$trip_income + coef_1_TT*dataset_scen3_sorted_3$trip_travel_time
utility_2 <- coef_2[1] + coef_2[2]*dataset_scen3_sorted_3$trip_density + coef_2[3]*dataset_scen3_sorted_3$trip_income + coef_2_TT*dataset_scen3_sorted_3$trip_travel_time
dataset_scen3_sorted_3$p_1 <- 1/(1+exp(-utility_1))
dataset_scen3_sorted_3$p_2 <- 1/(1+exp(-utility_2))
rand_num_1 <- runif(length(dataset_scen3_sorted_3$p_1))
rand_num_2 <- runif(length(dataset_scen3_sorted_3$p_2))
dataset_scen3_sorted_3$capture_1 <- ifelse(dataset_scen3_sorted_3$p_1 > rand_num_1, 1, 0)
dataset_scen3_sorted_3$capture_2 <- ifelse(dataset_scen3_sorted_3$p_2 > rand_num_2, 1, 0)

utility_1 <- coef_1[1] + coef_1[2]*dataset_scen3_sorted_4$trip_density + coef_1[3]*dataset_scen3_sorted_4$trip_income + coef_1_TT*dataset_scen3_sorted_4$trip_travel_time
utility_2 <- coef_2[1] + coef_2[2]*dataset_scen3_sorted_4$trip_density + coef_2[3]*dataset_scen3_sorted_4$trip_income + coef_2_TT*dataset_scen3_sorted_4$trip_travel_time
dataset_scen3_sorted_4$p_1 <- 1/(1+exp(-utility_1))
dataset_scen3_sorted_4$p_2 <- 1/(1+exp(-utility_2))
rand_num_1 <- runif(length(dataset_scen3_sorted_4$p_1))
rand_num_2 <- runif(length(dataset_scen3_sorted_4$p_2))
dataset_scen3_sorted_4$capture_1 <- ifelse(dataset_scen3_sorted_4$p_1 > rand_num_1, 1, 0)
dataset_scen3_sorted_4$capture_2 <- ifelse(dataset_scen3_sorted_4$p_2 > rand_num_2, 1, 0)

utility_1 <- coef_1[1] + coef_1[2]*dataset_scen3_sorted_5$trip_density + coef_1[3]*dataset_scen3_sorted_5$trip_income + coef_1_TT*dataset_scen3_sorted_5$trip_travel_time
utility_2 <- coef_2[1] + coef_2[2]*dataset_scen3_sorted_5$trip_density + coef_2[3]*dataset_scen3_sorted_5$trip_income + coef_2_TT*dataset_scen3_sorted_5$trip_travel_time
dataset_scen3_sorted_5$p_1 <- 1/(1+exp(-utility_1))
dataset_scen3_sorted_5$p_2 <- 1/(1+exp(-utility_2))
rand_num_1 <- runif(length(dataset_scen3_sorted_5$p_1))
rand_num_2 <- runif(length(dataset_scen3_sorted_5$p_2))
dataset_scen3_sorted_5$capture_1 <- ifelse(dataset_scen3_sorted_5$p_1 > rand_num_1, 1, 0)
dataset_scen3_sorted_5$capture_2 <- ifelse(dataset_scen3_sorted_5$p_2 > rand_num_2, 1, 0)

write.csv(dataset, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/scenario2_n0.2.csv", row.names=F)
write.csv(dataset_scen3_sorted_1, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/scenario_3_0.csv", row.names=F)
write.csv(dataset_scen3_sorted_2, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/scenario_3_02.csv", row.names=F)
write.csv(dataset_scen3_sorted_3, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/scenario_3_05.csv", row.names=F)
write.csv(dataset_scen3_sorted_4, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/scenario_3_08.csv", row.names=F)
write.csv(dataset_scen3_sorted_5, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/scenario_3_1.csv", row.names=F)