#Caleb Hightower
#Advisor: Lance Waller
#03/27/2021
#R 4.0.4 "Lost Library Book"

##############################################################
### Social Vulnerability and Opioid Overdoses in Georgia
### Assessing County Level Vulnerability to Opioid Overdoses
##############################################################

##############################################################
#Data Dimension Reduction
##############################################################

#libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(factoextra)
library(psych)
library(GPArotation)
library(nFactors)
library(car)
library(corrplot)
library(lares)

library(randomForest)

#useful operators
#returns the variables that are not in the intersection of x and y 
'%!in%' <- function(x,y)!('%in%'(x,y))
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}


#import predictors from data cleaning
setwd("H:/chight3/SVI")
pospred <- read.csv("PossiblePredictors.csv", header = T)

#check predictors
str(pospred)
head(pospred)


##########################################################################
#Principal Component Analysis
##########################################################################

#PCA is numeric only so we will drop categorical variables
#remove dummy/factor variables, remove population, FIPS, and all variables related to opioid overdose
numpred <- pospred %>% select_if(is.numeric) %>% select(-FIPS, -population, -starts_with("death"))

#also it can't take NA values so we have to use an imputation method or drop them
numpred <- na.omit(numpred) #drops 7 counties

#check again
head(numpred)

#Supply names of columns that have 0 variance
names(numpred[, sapply(numpred, function(v) var(v, na.rm=TRUE)==0)]) #none

#look at principal components after centering and standardizing
#couple of unique issues:
#PCA is sensitive to scaling and sensitive to missing values: 
#how do we deal with these when we only have 159 counties? i.e. gappy data
opioidPCA <- prcomp(numpred, center = T, scale. = T)

#summary and various scree plots
summary(opioidPCA)
screeplot(opioidPCA)
screeplot(opioidPCA, type = "lines")
fviz_eig(opioidPCA)

#screeplot: Ben Risk said it's the most important tool for selecting number of components
plot(opioidPCA$sdev^2,xlab='Rank of Eigenvalue',ylab='Eigenvalue',main='Screeplot for possible predictors',type='b')

#here, 5 would be a good option since it corresponds to the "elbow"
cumsum(opioidPCA$sdev^2)/sum(opioidPCA$sdev^2)

#elbow suggests 5 PCs should do
#8 if we're really picky with it
#note the rotations aren't correlations, but are just eigenvectors of the covariance matrix
ncomp = 5
opioidPCA$rotation[,1:5]

#scratch everything above, we're going to use the psych package to reduce now
#use psych package to recreate SAS's "Factor patterns" component matrix
opioidPCA <- psych::principal(numpred, rotate="none", nfactors=ncomp, covariance=TRUE)
x <- opioidPCA$loadings

#convert the loadings matrix to data frame
cutoff <- 0.5 # (the default for the `print.loadings()` function)
Lambda <- unclass(x)
p <- nrow(Lambda)
fx <- setNames(Lambda, NULL)
fx[abs(Lambda) < cutoff] <- NA_real_
fx <- as.data.frame(fx)

#literature suggested eliminating variables that didn't correlate higher than 0.5 in any component
#want to remove loading if less than 0.5
PCAreduce <- fx %>% filter(rowSums(is.na(.)) != ncol(.))

#which variables were removed
PCAreduce_variables <- outersect(rownames(PCAreduce), colnames(numpred))
PCAreduce_variables


#############################################
#Factor Analysis
#############################################

#subset data
#remove dummy/factor variables, remove population, FIPS, and all variables related to opioid overdose
numpred2 <- pospred %>% select(-all_of(PCAreduce_variables)) %>%
                        select_if(is.numeric) %>% 
                        select(-FIPS, -population, -starts_with("death"))
numpred2 <- na.omit(numpred2)

#visual correlation matrix quickly
corrplot(cor(numpred2, use="complete.obs"))

#determine number of factors
#Very Simple Structure Test
VSS(numpred2) #1 or 3 factors

#other methods
#parallel analysis
fa.parallel(numpred2, fm="minres", fa="fa") #5 factors
nScree(numpred2, model="factors") #6, 1, 6, and 6 factors recommended  

#plot scree
plot(nScree(numpred2, model="factors"))

#set number of factors we want, in this case we will defer to parallel analysis
nfac <- 5

#different methods of generating factor loadings
#Minimum Residual
(mr <- fa(numpred2, nfac, fm="minres"))

#Principal Axis
(pa <- fa(numpred2, nfac, fm="pa"))

#Weighted Least Squares
#(wls <- fa(numpred2, nfac, fm="wls"))
#singularity error, disregard

#now there are a couple ways to reduce the number of variables from here
#from what I can ascertain, Rickles et al. just uses the 0.5 correlation cutoff as in PCA
#Kathleen's mentor used uniqueness as a cutoff

#using loadings
x <- mr$loadings

#convert the loadings matrix to data frame
cutoff <- 0.5 # (the default for the `print.loadings()` function)
Lambda <- unclass(x)
p <- nrow(Lambda)
fx <- setNames(Lambda, NULL)
fx[abs(Lambda) < cutoff] <- NA_real_
fx <- as.data.frame(fx)

#finally reduce
FA_reduce_loadings <- fx %>% filter(rowSums(is.na(.)) != ncol(.))

#uniqueness:
unique <- as.matrix(mr$uniquenesses)

#we want a uniqeness greater than 0.4
cutoff <- 0.4 # (the default for the `print.loadings()` function)
Lambda <- unclass(unique)
p <- nrow(Lambda)
fu <- setNames(Lambda, NULL)
fu[abs(Lambda) < cutoff] <- NA_real_
fu <- as.data.frame(fu)

#finally reduce
FA_reduce_unique <- fu %>% filter(rowSums(is.na(.)) != ncol(.))

#which variables were removed
FAreduce_variables <- outersect(rownames(FA_reduce_loadings), colnames(numpred2))
FAreduce_variables

#we chose the Rickles method and not the uniqueness method for now
#FAreduce_variables <- outersect(rownames(FA_reduce_unique), colnames(numpred2))


############################
#Remove correlated variables
###########################

#subset data
#remove dummy/factor variables, remove population, FIPS, and all variables related to opioid overdose
#also remove variables from the dimension reduction
numpred3 <- pospred %>% select(-all_of(c(PCAreduce_variables, FAreduce_variables))) %>%
                        select_if(is.numeric) %>%
                        select(-FIPS, -population, -starts_with("death"))
numpred3 <- na.omit(numpred3)

#view correlation
cor_pred <- cor(numpred3)
cor_pred

#visualize correlation
corrplot(cor_pred, method="color",  
         type="upper", order="hclust")

#view top correlated pairs
corr_cross(numpred3) #many EXTREMELY correlated pairs

#resubset data but keep our outcome variable, opioid overdose mortality 
dat <- pospred %>% select(-all_of(c(PCAreduce_variables, FAreduce_variables)), -FIPS,
                          -deathalldrug, -deathalldrugR, -deathallopioidR, -deathheroin, -deathheroinR,
                          -deathsynthetic, -deathsyntheticR, -deathnatural, -deathnaturalR, -deathmethadone, -deathmethadoneR)

#see which ones are missing
which(is.na(dat))

#get rid of variables with missing data
#dat2 <- dat %>% select(-injurydeaths, -prematuredeaths, -Opioid.Dispensing.Rate.per.100)
#which(is.na(dat2))
#nevermind, the fits should be able to deal with missing data for now

#remove erheroin and totvacant since they are giving model trouble
dat3 <- dat %>% select(-totvacant, -erheroin, -erheroinR)

#our outcome data is count data, so we are interested in using a poisson model
#but first we need to deal with multicollinearity 

#idea: create function to get rid of biggest VIF and refit until VIF<10
#save all the variables that were discarded

#first let's test a Poisson glm fit
fit <- glm(deathallopioid~. + offset(log(population)) -County -population, family = poisson(link = "log"), data = dat3)
summary(fit)

#check the variance inflation factor
vif(fit)

#create a function to eliminate variables with GVIF > 10
multicol <- function(dat) {
  variables <- vector()
  vif10 <- 11
    
  while(vif10 > 10) {
    #fit GLM poisson
    fit <- glm(deathallopioid~. + offset(log(population)) -County -population, family = poisson(link = "log"), data = dat)
    
    #get variance inflation factor
    vif.fit <- vif(fit)[,3]^2
    vif10 <- sort(vif.fit, decreasing = T)[1]
    
    #check if it's greater than 10
    if(vif10 > 10) {
      variables <- c(variables, names(vif10)) #add to list and subset 
      dat <- dat %>% select(-names(vif10))
    }
  }
  return(variables) #return list of variables with highest VIFs
}

#check function
Poisreduce_variables <- multicol(dat3) #worked


#Testing model fit and finally removing variables

#resubset data but keep our outcome variable, opioid overdose mortality 
predictors <- pospred %>% select(-all_of(c(PCAreduce_variables, FAreduce_variables, Poisreduce_variables)), 
                                 -FIPS,-deathalldrug, -deathalldrugR, -deathallopioidR, -deathheroin, -deathheroinR,
                                 -deathsynthetic, -deathsyntheticR, -deathnatural, -deathnaturalR, -deathmethadone, -deathmethadoneR)
  
#a GLMM doesn't make sense in this case since we only have one county per estimate

#test poisson fit
fit <- glm(deathallopioid~. + offset(log(population)) -County -population, family = poisson(link = "log"), data = na.omit(predictors))
summary(fit)

#bidirectional stepwise AIC selection
MASS::stepAIC(fit)

#final model
final.fit <- glm(deathallopioid~ pctnonhiswhite + eralldrugR + erallopioidR + SpecialtyCareR + offset(log(population)),
             family = poisson(link = "log"), data = predictors)
summary(final.fit)

#model checks 
mean(predictors$deathallopioid) #5.365
var(predictors$deathallopioid) #123.88

#check histogram
hist(predictors$deathallopioid) #large low counts, big tail

#look at rates
mean(pospred$deathallopioidR)
var(pospred$deathallopioidR)
hist(pospred$deathallopioidR)

library(AER)
#dispersion test
dispersiontest(final.fit) #p = 0.0686

#create a function for a deviance test for goodness of fit
gofdev <- function(fit) {
  return(with(fit, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = F))))
}

#test overdispersion of poisson model
gofdev(final.fit) #p = 0.05, not good
#this likely suggests some overdispersion in the data

ssr <- sum(residuals(final.fit, type="pearson")^2)
pchisq(ssr, 158)

#frankly, a little overdispersed. We could switch to a quasipoisson or a Negative Binomial model
#if we want to use traditional variable selection techniques i.e., AIC we want NB model instead of quasipoisson
#Quasipoisson would have identical estimates to poisson but with adjusted errors
#NB should have similar estimates to poisson but different method

####
#try it with negative binomial
fit.nb <- MASS::glm.nb(deathallopioid~. + offset(log(population)) -County -population, data = dat3)
summary(fit.nb)

#check VIFs
vif(fit.nb)

#multicollinearity removal function for negative binomial 
multicol.nb <- function(dat) {
  variables <- vector()
  vif10 <- 11
  
  while(vif10 > 10) {
    #fit GLM
    fit <- MASS::glm.nb(deathallopioid~. + offset(log(population)) -County -population, data = dat)
    
    #get variance inflation factor
    vif.fit <- vif(fit)[,1]
    vif10 <- sort(vif.fit, decreasing = T)[1]
    
    #check if it's greater than 10
    if(vif10 > 10) {
      variables <- c(variables, names(vif10)) #add to list and subset 
      dat <- dat %>% select(-names(vif10))
    }
  }
  return(variables) #return list of variables with highest VIFs
}

#reduce variables with multicollinearity
nbreduce_variables <- multicol.nb(dat3)

#remove variables from PCA, FA, and empirical reduction
predictors.nb <- pospred %>% select(-all_of(c(PCAreduce_variables, FAreduce_variables, Poisreduce_variables)), 
                                 -FIPS,-deathalldrug, -deathalldrugR, -deathallopioidR, -deathheroin, -deathheroinR,
                                 -deathsynthetic, -deathsyntheticR, -deathnatural, -deathnaturalR, -deathmethadone, -deathmethadoneR)

#fit a nb model to test
fit.nb <- MASS::glm.nb(deathallopioid~. + offset(log(population)) -County -population, data = na.omit(predictors.nb))
summary(fit.nb)

#use bidirectional stepwise AIC to select final variables
MASS::stepAIC(fit.nb)

#select final predictors
pred.final <- predictors %>% select(County, population, deathallopioid, pctnonhiswhite, eralldrugR, erallopioidR, SpecialtyCareR)

#create final model
final.fit.nb <- MASS::glm.nb(deathallopioid~ pctnonhiswhite + eralldrugR + erallopioidR + SpecialtyCareR + offset(log(population)),
                             data = pred.final)
summary(final.fit.nb)

#check goodness of fit
gofdev(final.fit.nb) #0.07, not great but better than poisson

#quasipoisson
#can't use AIC anymore, variable selection seems more difficult
final.fit.quasi <- glm(deathallopioid~ pctnonhiswhite + eralldrugR + erallopioidR + SpecialtyCareR + offset(log(population)),
                       family = "quasipoisson", data = predictors)
summary(final.fit.quasi)

#deviance test for goodness of fit, might not make sense because it's just adjusted poisson
gofdev(final.fit.quasi) 


############################
#Create Vulnerability Score
############################

#get coefficients from nb final model
coeff <- final.fit.nb$coefficients

#create vulnerability ranking and scale it from 0 to 1
#we do this by multiplying all selected variables by the model coefficients, without the intercept
#then ranking them
#and finally scaling the ranking from 0 to 1, where 1 is the most vulnerable
pred.final <- predictors %>% select(County, population, deathallopioid, pctnonhiswhite, eralldrugR, erallopioidR, SpecialtyCareR) %>%
              mutate(vulnerability = coeff[2]*pctnonhiswhite + coeff[3]*eralldrugR + coeff[4]*erallopioidR + coeff[5]*SpecialtyCareR,
                     rank.vuln = dense_rank(desc(vulnerability)),
                     norm.vuln = (rank.vuln-max(rank.vuln))/(min(rank.vuln)-max(rank.vuln)))

#idea: select top 20% of vulnerable counties in both our index, and in SVI, compare
#then compare to GADPH map

#sort by highest vulnerable, and select top quintile
vuln.counties <- pred.final %>% arrange(rank.vuln) %>%
                                filter(row_number() < nrow(.)*0.20)

#import CDC SVI data
setwd("H:/chight3/SVI")

#read it in
GA.county <- read.csv(file = 'Georgia_COUNTY.csv')

#sort by highest vulnerable, and select top quintile
SVI <- GA.county %>% arrange(desc(RPL_THEMES)) %>%
                      filter(row_number() < nrow(.)*0.20)

#lastly, compare to GADPH
#these have to be done by hand
GADPH <- data.frame("County" = c("Fannin", "Towns", "Gilmer", "Pickens", "Dawson", "Bartow", "Jackson", "Madison", "Elbert", "Polk", "Haralson", 
                                 "Carroll", "Barrow", "Walton", "Lincoln", "Upson", "Dodge", "Effingham", "Bryan", "Benh Hill", "Jeff Davis", "Appling",
                                 "Irwin", "Coffee", "Bacon", "Berrien", "Atkinson", "Ware", "Pierce", "Brantley", "Clinch")) %>%
  mutate(County = paste0(County, " County, Georgia"))

#calculate number of overlaps
intersect(vuln.counties$County, SVI$LOCATION) #2 counties overlap

intersect(vuln.counties$County, GADPH$County) #9 counties overlap, or 29%


###################
#Map Vulnerability
###################
library(sf)
#library(raster)
library(RColorBrewer)

#set and rename county to table link with shapefile
GA.vulner <- pred.final %>%
  rename(LOCATION = County)

#read in the shape file with sf package
GA.shp <- st_read(dsn = "Georgia_COUNTY", layer = "SVI2018_GEORGIA_county")

#merge with shape file, i.e., table join in ArcGIS
Vulnerability.shp <- merge(GA.shp, GA.vulner, by = "LOCATION")

#set our quantiles and breaks
qtiles <- 5
breaks <- c(0, 0.202531646, 0.405063291, 0.607594937, 0.810126582, 1)

pal <- brewer.pal(qtiles, "OrRd") # we select 5 colors from the palette, color brewer package
class(pal)

#create choropleth maps for county vulnerability for overall and each theme
plot(Vulnerability.shp["norm.vuln"], 
     main = "Vulnerability Index for Opioid Overdose by County in GA", 
     breaks = breaks, nbreaks = qtiles,
     pal = pal)


#############################
#Random Forest approach
############################

#we are also interested in comparing the traditional data reduction method to a random forest approach
#works with categorical variables

#subset data
rfdat <- pospred %>% select(-FIPS, -population, -County, -deathalldrug, -deathalldrugR, -deathallopioidR, -deathheroin, -deathheroinR,
                            -deathsynthetic, -deathsyntheticR, -deathnatural, -deathnaturalR, -deathmethadone, -deathmethadoneR) 
rfdat <- na.omit(rfdat) #remove missing data

str(rfdat)
dim(rfdat)

#create random forest
set.seed(71) #set seed
rf <-randomForest(deathallopioid~.,data=rfdat, ntree=500) #create random forest with 500 trees
print(rf)

#find best mtry
mtry <- tuneRF(rfdat[-1],rfdat$deathallopioid, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#rerun random forest with best mtry
set.seed(71)
rf <-randomForest(deathallopioid~.,data=rfdat, mtry=best.m, importance=TRUE,ntree=500)
print(rf)

#Evaluate variable importance
important <- importance(rf)
varImpPlot(rf)

#how many to select? 25? 45?
incMSE <- names(head(sort(important[,1], decreasing = T), n = 45))
incNode <- names(head(sort(important[,2], decreasing = T), n = 45))

#try incMSE first
datrf <- pospred %>% select(all_of(incMSE), deathallopioid, population, County, -erheroin, -totvacant,
                            popdecline, urbanrural)

#erheroin and totvacant give model trouble bc of singularities
fit.rf <- MASS::glm.nb(deathallopioid~. + offset(log(population)) -County -population, data = datrf)
summary(fit.rf)
vif(fit.rf)

#remove multicollinear variables
rfreduce_variables <- multicol.nb(datrf)

#reduce predictors 
predrf <- datrf %>% select(-all_of(rfreduce_variables))

#fit a model with reduced predictors
fit.rf <- MASS::glm.nb(deathallopioid~. + offset(log(population)) -County -population, data = na.omit(predrf))
summary(fit.rf)

#bidirectional stepwise AIC variable selection
MASS::stepAIC(fit.rf)

#fit final model using incMSE
final.fit.rf <- MASS::glm.nb(deathallopioid~ pctnonhiswhite + urbanrural + offset(log(population)),
                             data = datrf)
summary(final.fit.rf)

#goodness of fit test
gofdev(final.fit.rf) #pretty bad <0.05


#try incNode
datrf <- pospred %>% select(all_of(incNode), deathallopioid, population, County, -erheroin, -totvacant,
                            popdecline, urbanrural)

#erheroin and totvacant give model trouble bc of singularities
fit.rf <- MASS::glm.nb(deathallopioid~. + offset(log(population)) -County -population, data = datrf)
summary(fit.rf)
vif(fit.rf)

#remove multicollinear variables
rfreduce_variables <- multicol.nb(datrf)

#reduce predictors 
predrf <- datrf %>% select(-all_of(rfreduce_variables))

#fit a model with reduced predictors
fit.rf <- MASS::glm.nb(deathallopioid~. + offset(log(population)) -County -population, data = na.omit(predrf))
summary(fit.rf)

#bidirectional stepwise AIC variable selection
MASS::stepAIC(fit.rf)

#fit final model using incNode
final.fit.rf <- MASS::glm.nb(deathallopioid~ pctnonhiswhite + urbanrural + mobilehome + crowded + offset(log(population)),
                             data = datrf)
summary(final.fit.rf)

#goodness of fit test, deviance
gofdev(final.fit.rf) #pretty bad
