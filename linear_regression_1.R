##  Introduction
## ══════════════


## Set working directory
setwd()
setwd("C:/Users/uohna/Dropbox/DataSets/Springboard/linear_regression/linear_regression")

getwd() 
"C:/Users/uohna/Dropbox/DataSets/Springboard/linear_regression/linear_regression"

list.files() # files in the dataSets folder
# "dataSets" "linear_regression.R" "linear_regression_1.R" "README.txt" "Rtips.pdf"

## Load the states data
## ────────────────────────

##  read the states data
states.data <- readRDS("dataSets/states.rds") 

##  get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])

##  look at last few labels
tail(states.info, 8)
# names                      var.labels
# 14    csat        Mean composite SAT score
# 15    vsat           Mean verbal SAT score
# 16    msat             Mean math SAT score
# 17 percent       % HS graduates taking SAT
# 18 expense Per pupil expenditures prim&sec
# 19  income Median household income, $1,000
# 20    high             % adults HS diploma
# 21 college         % adults college degree

## Linear regression
## ═══════════════════

## Examine the data before fitting models

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# Min.   :2960   Min.   : 832.0  
# 1st Qu.:4352   1st Qu.: 888.0  
# Median :5000   Median : 926.0  
# Mean   :5236   Mean   : 944.1  
# 3rd Qu.:5794   3rd Qu.: 997.0  
# Max.   :9259   Max.   :1093.0 

## correlation between expense and csat
cor(sts.ex.sat)
#           expense       csat
# expense  1.0000000 -0.4662978
# csat    -0.4662978  1.0000000

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

## scatter plot of expense vs csat
plot(sts.ex.sat)



## Fit linear regression model
sat.mod <- lm(csat ~ expense, data=states.data)

## Summarize and print results
summary(sat.mod) 

# lm(formula = csat ~ expense, data = states.data)

# Residuals:
#  Min        1Q      Median       3Q      Max 
# -131.811  -38.085    5.607   37.852  136.495 

# Coefficients:
#              Estimate   Std. Error   t value   Pr(>|t|)    
# (Intercept)  1.061e+03   3.270e+01   32.44    < 2e-16 ***
#  expense     -2.228e-02  6.037e-03   -3.69   0.000563 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 59.81 on 49 degrees of freedom
# Multiple R-squared:  0.2174,	Adjusted R-squared:  0.2015 
# F-statistic: 13.61 on 1 and 49 DF,  p-value: 0.0005631
  
confint(sat.mod)
#                 2.5%        97.5%
#   (Intercept)   995.01753164 1126.44735626
# expense        -0.03440768   -0.01014361

## Print histogram of model residuals 
hist(residuals(sat.mod))

  ###############

## Fit second linear regression model
sat.mod.per <- (lm(csat ~ expense + percent, data = states.data))

##Print summary of model
summary(sat.mod.per)

#lm(formula = csat ~ expense + percent, data = states.data)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-62.921 -24.318   1.741  15.502  75.623 

#Coefficients:
#            Estimate   Std. Error t value Pr(>|t|)    
#(Intercept) 989.807403  18.395770  53.806  < 2e-16 ***
#  expense   0.008604    0.004204   2.046   0.0462 *  
#  percent   -2.537700   0.224912 -11.283 4.21e-15 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 31.62 on 48 degrees of freedom
#Multiple R-squared:  0.7857,	Adjusted R-squared:  0.7768 
#F-statistic: 88.01 on 2 and 48 DF,  p-value: < 2.2e-16

confint(sat.mod.2)
#                  2.5 %        97.5 %
# (Intercept)  9.528202e+02 1026.79457731
# expense      1.505116e-04    0.01705769
# percent     -2.989915e+00   -2.08548496


## Linear Regression Assumptions
## ─────────────────────────────────

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────


## fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))

## compare using the anova() function
anova(sat.mod, sat.voting.mod)
# Analysis of Variance Table

# Model 1: csat ~ expense
# Model 2: csat ~ expense + house + senate
#   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1     46 169050                              
# 2     44 149284  2     19766 2.9128 0.06486 .
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coef(summary(sat.voting.mod)) 
#                 Estimate   Std. Error    t value     Pr(>|t|)
# (Intercept) 1082.93438041 38.633812740 28.0307405 1.067795e-29
# expense       -0.01870832  0.009691494 -1.9303852 6.001998e-02
# house         -1.44243754  0.600478382 -2.4021473 2.058666e-02
# senate         0.49817861  0.513561356  0.9700469 3.373256e-01

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
sts.metro.energy <- subset(states.data, select = c("metro", "energy"))
summary(sts.metro.energy)
# metro            energy     
# Min.   : 20.40   Min.   :200.0  
# 1st Qu.: 46.98   1st Qu.:285.0  
# Median : 67.55   Median :320.0  
# Mean   : 64.07   Mean   :354.5  
# 3rd Qu.: 81.58   3rd Qu.:371.5  
# Max.   :100.00   Max.   :991.0  
# NA's   :1        NA's   :1 

sts.metro.energy <- na.omit(sts.metro.energy)
str(sts.metro.energy)
#         metro            energy     
# Min.   : 20.40   Min.   :200.0  
# 1st Qu.: 46.98   1st Qu.:285.0  
# Median : 67.55   Median :320.0  
# Mean   : 64.07   Mean   :354.5  
# 3rd Qu.: 81.58   3rd Qu.:371.5  
# Max.   :100.00   Max.   :991.0 

cor(sts.metro.energy)
#           metro     energy
# metro   1.0000000 -0.3397445
# energy -0.3397445  1.0000000

plot(sts.metro.energy)
energy.mod_1 <- lm(energy ~ metro, data=sts.metro.energy)
summary(energy.mod_1)

# Call:
#  lm(formula = energy ~ metro, data = sts.metro.energy)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -215.51  -64.54  -30.87   18.71  583.97 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 501.0292    61.8136   8.105 1.53e-10 ***
#   metro        -2.2871     0.9139  -2.503   0.0158 *  
  ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 140.2 on 48 degrees of freedom
# Multiple R-squared:  0.1154,	Adjusted R-squared:  0.097 
# F-statistic: 6.263 on 1 and 48 DF,  p-value: 0.01578
  
confint(energy.mod_1)
#                 2.5 %      97.5 %
# (Intercept) 376.744602 625.3138541
# metro        -4.124517  -0.4496617

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
plot(energy.mod_1, which = c(1, 2))

##   Select one or more additional predictors to add to your model

energy.mod_2 <- lm(energy ~ metro + density, data=states.data)
summary(energy.mod_2)

# Call:
#   lm(formula = energy ~ metro + density, data = states.data)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -196.38  -68.48  -34.29   16.32  581.87 

# Coefficients:
#               Estimate  Std. Error t value Pr(>|t|)    
# (Intercept)  470.9255    66.9316   7.036   7.21e-09 ***
#   metro      -1.5008     1.1387   -1.318    0.194    
# density      -0.1221     0.1061   -1.151    0.256    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 139.7 on 47 degrees of freedom
# Multiple R-squared:  0.1397,	Adjusted R-squared:  0.1031 
# F-statistic: 3.815 on 2 and 47 DF,  p-value: 0.02916


#################

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────



## Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income, data=states.data) 

#Show the results
coef(summary(sat.expense.by.percent)) 
#                    Estimate   Std. Error   t value     Pr(>|t|)
# (Intercept)     1.380364e+03 1.720863e+02  8.021351 2.367069e-10
# expense        -6.384067e-02 3.270087e-02 -1.952262 5.687837e-02
# income         -1.049785e+01 4.991463e+00 -2.103161 4.083253e-02
# expense:income  1.384647e-03 8.635529e-04  1.603431 1.155395e-01

summary(sat.expense.by.percent)

# Call:
#   lm(formula = csat ~ expense * income, data = states.data)

# Residuals:
#  Min       1Q   Median       3Q      Max 
# -133.725  -29.478   -5.475   26.267  134.190 

# Coefficients:
#                   Estimate  Std. Error  t value   Pr(>|t|)    
# (Intercept)     1.380e+03  1.721e+02    8.021   2.37e-10 ***
# expense        -6.384e-02  3.270e-02   -1.952   0.0569 .  
# income         -1.050e+01  4.991e+00   -2.103   0.0408 *  
# expense:income  1.385e-03  8.636e-04    1.603   0.1155    
---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 57.75 on 47 degrees of freedom
# Multiple R-squared:  0.3002,	Adjusted R-squared:  0.2555 
# F-statistic:  6.72 on 3 and 47 DF,  p-value: 0.0007246

  
#########################

## Regression with categorical predictors
## ──────────────────────────────────────────


# make sure R knows region is categorical
states.data$region <- factor(states.data$region)
str(states.data$region)
# Factor w/ 4 levels "West","N. East",..: 3 1 1 3 1 1 2 3 NA 3 ...


## Add region to the model
sat.mod.region <- lm(csat ~ region, data=states.data)
coef(summary(sat.mod.region))
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     946.31      14.80  63.958  < 2e-16 ***
#  regionN. East   -56.75      23.13  -2.453  0.01800 *  
#  regionSouth     -16.31      19.92  -0.819  0.41719    
#regionMidwest    63.78      21.36   2.986  0.00451 ** 

anova(sat.mod.region) 
# Analysis of Variance Table

# Response: csat
#           Df Sum Sq   Mean Sq  F value  Pr(>F)    
# region     3  82049   27349.8  9.6102   4.859e-05 ***
# Residuals 46  130912  2845.9                      



## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────


## print default contrasts
contrasts(states.data$region)
# N. East South Midwest
# West          0     0       0
# N. East       1     0       0
# South         0     1       0
# Midwest       0     0       1

## change the reference group
coef(summary(lm(csat ~ C(region, base=4), data=states.data)))
#                        Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)          1010.08333   15.39998 65.589930 4.296307e-47
# C(region, base = 4)1  -63.77564   21.35592 -2.986321 4.514152e-03
# C(region, base = 4)2 -120.52778   23.52385 -5.123641 5.798399e-06
# C(region, base = 4)3  -80.08333   20.37225 -3.931000 2.826007e-04
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert), data=states.data)))



## Exercise: interactions and factors
## ────────────────────────────────────────


##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

sat.mod.per.inc <- lm(csat~percent + income , data=states.data)
summary(sat.mod.per.inc)

# Call:
# lm(formula = csat ~ percent + income, data = states.data)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -58.131 -24.273   1.494  15.897  76.235 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 960.7922    27.0214  35.557  < 2e-16 ***
#   percent      -2.6110     0.2271 -11.495 2.18e-15 ***
#   income        2.2584     0.9263   2.438   0.0185 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 31.1 on 48 degrees of freedom
# Multiple R-squared:  0.7927,	Adjusted R-squared:  0.7841 
# F-statistic: 91.78 on 2 and 48 DF,  p-value: < 2.2e-16

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

sat.mod.per.inc.reg <- lm(csat~percent + income + region , data=states.data)
summary(sat.mod.per.inc.reg)

# Call:
#   lm(formula = csat ~ percent + income + region, data = states.data)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -48.364 -15.497  -2.201  16.457  57.583 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   969.1954    26.6080  36.425  < 2e-16 ***
#   percent        -2.9432     0.2736 -10.756 6.74e-14 ***
#   income          1.8998     0.8646   2.197  0.03331 *  
#   regionN. East  57.2115    14.3990   3.973  0.00026 ***
#   regionSouth    -0.1698    10.4219  -0.016  0.98707    
# regionMidwest  22.8707    10.6887   2.140  0.03796 *  

# Residual standard error: 25.43 on 44 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.8664,	Adjusted R-squared:  0.8512 
# F-statistic: 57.05 on 5 and 44 DF,  p-value: < 2.2e-16
  
  