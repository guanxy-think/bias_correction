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
people_density <- sample(zone_density, num_people, replace = T)
## number of trips in a zone proportional to population density
# zone_num_people <- round(num_people*zone_density/sum(zone_density))   #number of trip proportional to pop density
# zone_num_people[1] <- zone_num_people[1] + 1
# people_density <- rep(zone_density,zone_num_people)
## the number of people in each zone is a random number
# zone_num_people <- rlnorm(10,log(400))
# zone_num_people <- zone_num_people*num_people/sum(zone_num_people)
# zone_num_people <- round(zone_num_people)
# zone_num_people[7] <- zone_num_people[7] - 1
# people_density <- rep(zone_density,zone_num_people)

### Rearrange to negatively correlated income and density
### https://stats.stackexchange.com/questions/134164/how-to-rearrange-2d-data-to-get-given-correlation (search "rearange two variables to have some correlation")
set.seed(1)
cor_mat <- matrix(-0.1, 2, 2)    ##########-1 or -0.1
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

# ## Travel time data
# trip_file <- read.csv("G:/Shared drives/2017 NHTS/Original data and documentation/Csv/trippub.csv")
# trip_travel_time <- sample(trip_file$TRVLCMIN, length(trip_income), replace=T)
# 
# dataset <- data.frame(trip_density, trip_income, trip_travel_time)

### scenario 2
## Random assign trips in each zone. This created no predictor scenarios.
people_num_trips <- rpois(4000, 4)
trip_density <- rep(people_density, people_num_trips)
trip_income <- rep(people_income, people_num_trips)
num_trips_by_zone <- sapply(zone_density, function(x) length(which(trip_density==x)))

total_num_trips <- sum(people_num_trips)

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
zone_income <- sapply(pop_densities_unique, function(x) median(unique(trip_income[which(trip_density==x)])))
fit <- lm(ground_truth~pop_densities_unique+zone_income)
summary(fit)
#

write.csv(dataset, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/new_version/scenario2(-0.02)_noPredictor.csv")
##

## Assign trips based on parameterization
## Constraints: median population density leads to lambda of 4; mean lambda is around 4
set.seed(16)      ###########
alpha <- 2.06
beta_density <- (log(3) - alpha)/median(people_density)
people_num_trips <- exp(alpha+beta_density*people_density)
summary(people_num_trips)

trip_density <- rep(people_density, people_num_trips)
trip_income <- rep(people_income, people_num_trips)
num_trips_by_zone <- sapply(zone_density, function(x) length(which(trip_density==x)))

prop_1 <- rep(0.01, 10)
prop_2 <- zone_density*0.1/max(zone_density)

num_trips_by_zone_capture_1 <- round(num_trips_by_zone*prop_1)
num_trips_by_zone_capture_2 <- round(num_trips_by_zone*prop_2)

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
zone_income <- sapply(pop_densities_unique, function(x) median(unique(dataset$trip_income[which(dataset$trip_density==x)])))
fit <- lm(ground_truth~pop_densities_unique+zone_income)
summary(fit)
table(dataset$capture_1,dataset$capture_2)
#

write.csv(dataset, "G:/My Drive/trip bias paper/scenarios/scenario_2_and_3/predictor_correlation/new_version/scenario2(-0.02)_negNumDensityCor.csv")





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