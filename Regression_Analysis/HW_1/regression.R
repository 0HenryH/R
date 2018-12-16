# import data
setwd('D:/Mycloud/R/Regression_Analysis/HW_1')
df = read.table('data.csv',header = TRUE,sep = ',')
df = data.frame(apply(df,2, as.numeric))
# Import Packages 
library('MASS')

# fit original linear regression
# put in all the features
lm_ori = lm('Y~.', data = df)
summary(lm_ori)

# selecting feature : based on AIC
lm_step = stepAIC(object = lm_ori,test = 'F')

# After feature selecting
lmfinal = lm(lm_step$call$formula,data = df)
summary(lmfinal)


# rms
library('rms')
ols = ols(Y~.,data = df)
fastbw(ols)

ols = ols(Y~X1,data = df)
