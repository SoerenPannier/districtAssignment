# population building 

library(sampling)
library(simFrame)
library(maptools)

# load utility functions
# functions to reduce the number of and rename columns 
source("./R/adjustDataSets.R")
#function to create population data
source("./R/createPop.R")

data("eusilcP")
# creation of a population
test <- createPop(seed = 34106, expension = 1, dispersion = 0.8, data = eusilcP)

dim(test$eusilcHHpop)

summary(as.numeric(table(test$eusilcHHpop$sub_2)))

# choosing initial sample sizes
distSizes <- as.numeric(table(test$eusilcHHpop$sub_2))
smpSizes <- ifelse(distSizes <= quantile(distSizes, 0.25), 1, 100)

# using the 25% smallest domains as out of sample
tempDf <- test$eusilcHHpop[!test$eusilcHHpop$sub_2 %in% 
                             names(table(test$eusilcHHpop$sub_2))[ distSizes <= 
                                                                     quantile(distSizes, 0.25)],]
tempDf$sub_2 <- factor(tempDf$sub_2, levels = unique(tempDf$sub_2))
# truncating sample sizes of remaining domains, at 1 and 200 respectively
smpSizes <- apply(cbind(ceiling(as.numeric(table(tempDf$sub_2)) * 0.1), 200), 1, min)

# stratified sampling
set.seed(seed = test$seed)
smpID2 <- strata( data = tempDf, 
                  stratanames = "sub_2", 
                  size = smpSizes, 
                  method = "srswor")

table(smpID2$sub_2)
smp <- cbind(tempDf[smpID2$ID_unit,], weight = 1 / smpID2$Prob)

dim(smp)

table(smp$sub_2)
summary(by(smp, INDICES = smp$sub_2, function(df) {
  laeken::weightedMean(x = df$eqIncome, weights = df$weight)}))
summary(tapply(test$eusilcHHpop$eqIncome, 
               INDEX = test$eusilcHHpop$sub_2, mean))

unique(test$eusilcHHpop[c("deutsch", "sub_2")])
unique(smp[c("deutsch", "sub_2")])

levels(smp$sub_2)
levels(test$eusilcHHpop$sub_2)

smp$sub_2 <- droplevels(factor(as.character(smp$sub_2), 
                               levels = levels(test$eusilcHHpop$sub_2)))

levels(smp$sub_2)
levels(test$eusilcHHpop$sub_2)

# reducing the number of variables in the dataset and
# assigning more readable names to them
results <- adjust_dataSets(pop = test$eusilcHHpop, smp = smp)

lapply(results, head)

eusilcA_pop <- results$pop
eusilcA_smp <- results$smp

eusilcA_smp <- eusilcA_smp[order(eusilcA_smp$district), ]
levels(eusilcA_smp$district)
levels(eusilcA_pop$district)

summary(as.numeric(table(eusilcA_pop$district)))
summary(as.numeric(table(eusilcA_smp$district)))

save(eusilcA_pop, file = "./Data/eusilcA_pop.rda")
save(eusilcA_smp, file = "./Data/eusilcA_smp.rda")


