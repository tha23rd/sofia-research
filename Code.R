library(dplyr)
library(GGally)
library(ggplot2)

#Reading data, renaming first column 
cross_sect_data = read.csv("~/Desktop/Datasets IR/CrossSectionalData.csv")
head(cross_sect_data)
dim(cross_sect_data)
names(cross_sect_data)
names(cross_sect_data)[1] = "country"
str(cross_sect_data)  #making sure all variables are read as numeric except country and continent 

#Dealing with NAs 
sum(is.na(cross_sect_data)) #0, no N/As
apply(cross_sect_data, 2, function(x) any(is.na(x))) == TRUE  #all false 

#Looking at correlation amongst regressors 
cross_sect_data %>%
  select(corp_tax_avg, per_tax_avg, cons_tax_avg, tariff_avg, enroll_avg, icrg_avg, inflation_avg, pop_avg) %>%
  ggpairs()

unique(cross_sect_data$continent)

#Creating continent dummies 
cont_europe = c()
cont_asia = c()
cont_eurasia = c()
cont_america = c()

for (i in 1:nrow(cross_sect_data)) {
  if (cross_sect_data$continent[i] == "Europe") {
    cont_europe[i] = 1
    cont_asia[i] = 0 
    cont_eurasia[i] = 0
    cont_america[i] = 0
  }
  else if (cross_sect_data$continent[i] == "Asia"){
    cont_europe[i] = 0
    cont_asia[i] = 1 
    cont_eurasia[i] = 0
    cont_america[i] = 0
  }
  else if (cross_sect_data$continent[i] == "Eurasia"){
    cont_europe[i] = 0
    cont_asia[i] = 0 
    cont_eurasia[i] = 1
    cont_america[i] = 0
  }
  else if (cross_sect_data$continent[i] == "America"){
    cont_europe[i] = 0
    cont_asia[i] = 0 
    cont_eurasia[i] = 0
    cont_america[i] = 1
  }
}

cross_sect_data = cbind(cross_sect_data, cont_europe, cont_asia, cont_eurasia, cont_america)
cross_sect_data

#Potentially problematic correlations 
    #personal and corp tax rates, 0.66
    #inflation and corp tax rates, 0.8
    #enrollment and tariff rate 0.75
    #inflation and personal tax rate, 0.74
    #icrg and corp tax rate 0.6
    #icrg and tariff 0.77

#Looking at distribution of GDP growth rate (outlier = China)
hist(cross_sect_data$gdp_avg, 
     xlab = "GDP growth rate", 
     main = "Distribution of GDP Growth Rate", 
     xlim = c(0,12), 
     ylim = c(0,5), 
     breaks=c(0:12), 
     right = FALSE,)    

#Regression 1: only corporate tax rate 
regression_1 = lm(gdp_avg ~ corp_tax_avg, data = cross_sect_data)
summary(regression_1)  #no signicant variables 

#Regression 2: corporate + control matrix 
regression_2 = lm(gdp_avg ~ corp_tax_avg + tariff_avg + enroll_avg + icrg_avg + inflation_avg + pop_avg + trade_open, data = cross_sect_data)
summary(regression_2)  #significant: corporate tax, enroll average, inflation avg, trade_open

#This is where the issue starts: regressors exceed the number of variables (k>n)
#Regression 3: corporate + control matrix + continent 
regression_3 = lm(gdp_avg ~ continent + corp_tax_avg + tariff_avg + enroll_avg + icrg_avg + inflation_avg + pop_avg + trade_open, data = cross_sect_data)
summary(regression_3) 

#Regression 4: corporate + control matrix + other tax variables 
regression_4 = lm(gdp_avg ~ per_tax_avg + cons_tax_avg + corp_tax_avg + tariff_avg + enroll_avg + inflation_avg + pop_avg, data = cross_sect_data)
summary(regression_4)

#should I include a couple of more countries to be able to run cross sectional?
#will this also be an issue in panel?
#should i include less regressors in cross section to force k to be less than n

#Lasso Solution (only chooses intercept)
library(glmnet)
set.seed(10)
cross_sect_data_lasso =  cross_sect_data[ , ! colnames(cross_sect_data) %in% c("country", "year_open", "cont_europe", "cont_asia", "cont_eurasia", "cont_america")]
X = model.matrix(gdp_avg~., cross_sect_data_lasso)[,-1] 
Y = cross_sect_data_lasso$gdp_avg

lasso_fit = cv.glmnet(X, Y, alpha=1, nfolds=10)
plot(lasso_fit)
coef= coef(lasso_fit, s="lambda.1se") 
coef = coef[which(coef !=0),] 

#Regsubsets?
library(leaps)
fit_all= regsubsets(gdp_avg ~., cross_sect_data_lasso, nvmax= 8, method="exhaustive")
summary = summary(fit_all)
which.min(summary$bic)  #chooses 7 regressors 

#If we limit variables up until 8 the chosen ones are:
#corp_tax_avg, per_tax_avg, cons_tax_avg, tariff_avg, enroll_avg, icrg_avg, inflation, cont_Eurasia
regression_5 = lm(gdp_avg ~ per_tax_avg + cons_tax_avg + corp_tax_avg + tariff_avg + enroll_avg + inflation_avg + cont_eurasia, data = cross_sect_data)
summary(regression_5)





