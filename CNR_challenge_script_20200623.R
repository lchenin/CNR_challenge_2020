if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")

# Note: this process could take a couple of minutes

X_test <- read_csv("https://raw.github.com/lchenin/CNR_challenge_2020/master/X_test_v21.csv")
X_train <- read_csv("https://raw.github.com/lchenin/CNR_challenge_2020/master/X_train_v21.csv")
Y_test_r <- read_csv("https://raw.github.com/lchenin/CNR_challenge_2020/master/Y_test_random.csv")
Y_train <- read_csv("https://raw.github.com/lchenin/CNR_challenge_2020/master/Y_train_sl9m6Jh.csv")

length(X_train$WF =="WF1")

n_distinct(X_train$WF)

X_train %>% group_by(WF) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

head(X_train)

X_test %>% group_by(WF) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

X_test <- X_test %>% cbind(Y_test_r[,2])
X_train <- X_train %>% cbind(Y_train[,2])

WFs <- c("WF1", "WF2", "WF3", "WF4", "WF5", "WF6")
X_train %>% filter(WF %in% WFs) %>% ggplot(aes(x = dmy_hm(Time), y = Production)) +
  geom_line(size = 1, color = "darkgrey") +
  geom_smooth(method = "loess") +
  facet_wrap(~WF)

X_test %>% filter(WF %in% WFs) %>% ggplot(aes(x = dmy_hm(Time), y = Production)) +
  geom_line(size = 1, color = "darkgrey") +
  geom_smooth(method = "loess") +
  facet_wrap(~WF)


sum(is.na(X_train))/102/37375  # or 46 % of the content is NA

sum(is.na(X_test))/102/36529 # or 76 %  of the content is NA 

sum(Y_train == 0)/37375 # or 9.4 % of the content is zero

# hourly count of production

X_train %>% filter(WF %in% WFs) %>% 
  ggplot(aes(Production)) + geom_histogram(binwidth = 0.1) + ggtitle("Hourly count X_train") + facet_wrap(~WF)

# the majority of no hourly production is born by WF1

# count of production greater than zero

X_train %>% filter(WF %in% WFs) %>% filter(Production > 0.05) %>% 
  ggplot(aes(Production)) + geom_histogram(binwidth = 0.1) + ggtitle("Hourly count X_train: Production > 0.05") + facet_wrap(~WF)

# set all NA's to zero and assume in a first step that no interpolation or extraploation 
# of NA's with neighbors is required 

X_test[is.na(X_test)] <- 0
X_train[is.na(X_train)] <- 0

# however we'll add predictors as the challenge provider stated that that wind speed is a good predictor of 
# wind production. We use the formula 'wind speed' = (NWP1_00h_D_1_U^2 + NWP1_00h_D_1_V^2)^0.5 and take the power 3 
# of it to get a variable that is proportionnal to the electrical production.

X_train <- X_train %>% mutate(V1 = (NWP1_00h_D_1_U^2 + NWP1_00h_D_1_V^2)^1.5,
           V2 = (NWP1_06h_D_1_U^2 + NWP1_06h_D_1_V^2)^1.5,
           V3 = (NWP1_12h_D_1_U^2 + NWP1_12h_D_1_V^2)^1.5,
           V4 = (NWP1_18h_D_1_U^2 + NWP1_18h_D_1_V^2)^1.5,
           V5 = (NWP2_00h_D_1_U^2 + NWP2_00h_D_1_V^2)^1.5,
           V6 = (NWP2_12h_D_1_U^2 + NWP2_12h_D_1_V^2)^1.5,
           V7 = (NWP3_00h_D_1_U^2 + NWP3_00h_D_1_V^2)^1.5,
           V8 = (NWP3_06h_D_1_U^2 + NWP3_06h_D_1_V^2)^1.5,
           V9 = (NWP3_12h_D_1_U^2 + NWP3_12h_D_1_V^2)^1.5,
           V10 = (NWP3_18h_D_1_U^2 + NWP3_18h_D_1_V^2)^1.5,
           V11 = (NWP4_00h_D_1_U^2 + NWP4_00h_D_1_V^2)^1.5,
           V12 = (NWP4_12h_D_1_U^2 + NWP4_12h_D_1_V^2)^1.5, 
           V13 = (NWP1_00h_D_2_U^2 + NWP1_00h_D_2_V^2)^1.5,
           V14 = (NWP1_06h_D_2_U^2 + NWP1_06h_D_2_V^2)^1.5,
           V15 = (NWP1_12h_D_2_U^2 + NWP1_12h_D_2_V^2)^1.5,
           V16 = (NWP1_18h_D_2_U^2 + NWP1_18h_D_2_V^2)^1.5,
           V17 = (NWP2_00h_D_2_U^2 + NWP2_00h_D_2_V^2)^1.5,
           V18 = (NWP2_12h_D_2_U^2 + NWP2_12h_D_2_V^2)^1.5,
           V19 = (NWP3_00h_D_2_U^2 + NWP3_00h_D_2_V^2)^1.5,
           V20 = (NWP3_06h_D_2_U^2 + NWP3_06h_D_2_V^2)^1.5,
           V21 = (NWP3_12h_D_2_U^2 + NWP3_12h_D_2_V^2)^1.5,
           V22 = (NWP3_18h_D_2_U^2 + NWP3_18h_D_2_V^2)^1.5,
           V23 = (NWP4_00h_D_2_U^2 + NWP4_00h_D_2_V^2)^1.5,
           V24 = (NWP4_12h_D_2_U^2 + NWP4_12h_D_2_V^2)^1.5, 
           V25 = (NWP1_00h_D_U^2 + NWP1_00h_D_V^2)^1.5,
           V26 = (NWP1_06h_D_U^2 + NWP1_06h_D_V^2)^1.5,
           V27 = (NWP1_12h_D_U^2 + NWP1_12h_D_V^2)^1.5,
           V28 = (NWP1_18h_D_U^2 + NWP1_18h_D_V^2)^1.5,
           V29 = (NWP2_00h_D_U^2 + NWP2_00h_D_V^2)^1.5,
           V30 = (NWP2_12h_D_U^2 + NWP2_12h_D_V^2)^1.5,
           V31 = (NWP3_00h_D_U^2 + NWP3_00h_D_V^2)^1.5,
           V32 = (NWP3_06h_D_U^2 + NWP3_06h_D_V^2)^1.5,
           V33 = (NWP3_12h_D_U^2 + NWP3_12h_D_V^2)^1.5,
           V34 = (NWP3_18h_D_U^2 + NWP3_18h_D_V^2)^1.5,
           V35 = (NWP4_00h_D_U^2 + NWP4_00h_D_V^2)^1.5,
           V36 = (NWP4_12h_D_U^2 + NWP4_12h_D_V^2)^1.5, 
           tot = V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+
             V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+
             V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36)

X_test <- X_test %>% mutate(V1 = (NWP1_00h_D_1_U^2 + NWP1_00h_D_1_V^2)^1.5,
                                V2 = (NWP1_06h_D_1_U^2 + NWP1_06h_D_1_V^2)^1.5,
                                V3 = (NWP1_12h_D_1_U^2 + NWP1_12h_D_1_V^2)^1.5,
                                V4 = (NWP1_18h_D_1_U^2 + NWP1_18h_D_1_V^2)^1.5,
                                V5 = (NWP2_00h_D_1_U^2 + NWP2_00h_D_1_V^2)^1.5,
                                V6 = (NWP2_12h_D_1_U^2 + NWP2_12h_D_1_V^2)^1.5,
                                V7 = (NWP3_00h_D_1_U^2 + NWP3_00h_D_1_V^2)^1.5,
                                V8 = (NWP3_06h_D_1_U^2 + NWP3_06h_D_1_V^2)^1.5,
                                V9 = (NWP3_12h_D_1_U^2 + NWP3_12h_D_1_V^2)^1.5,
                                V10 = (NWP3_18h_D_1_U^2 + NWP3_18h_D_1_V^2)^1.5,
                                V11 = (NWP4_00h_D_1_U^2 + NWP4_00h_D_1_V^2)^1.5,
                                V12 = (NWP4_12h_D_1_U^2 + NWP4_12h_D_1_V^2)^1.5, 
                                V13 = (NWP1_00h_D_2_U^2 + NWP1_00h_D_2_V^2)^1.5,
                                V14 = (NWP1_06h_D_2_U^2 + NWP1_06h_D_2_V^2)^1.5,
                                V15 = (NWP1_12h_D_2_U^2 + NWP1_12h_D_2_V^2)^1.5,
                                V16 = (NWP1_18h_D_2_U^2 + NWP1_18h_D_2_V^2)^1.5,
                                V17 = (NWP2_00h_D_2_U^2 + NWP2_00h_D_2_V^2)^1.5,
                                V18 = (NWP2_12h_D_2_U^2 + NWP2_12h_D_2_V^2)^1.5,
                                V19 = (NWP3_00h_D_2_U^2 + NWP3_00h_D_2_V^2)^1.5,
                                V20 = (NWP3_06h_D_2_U^2 + NWP3_06h_D_2_V^2)^1.5,
                                V21 = (NWP3_12h_D_2_U^2 + NWP3_12h_D_2_V^2)^1.5,
                                V22 = (NWP3_18h_D_2_U^2 + NWP3_18h_D_2_V^2)^1.5,
                                V23 = (NWP4_00h_D_2_U^2 + NWP4_00h_D_2_V^2)^1.5,
                                V24 = (NWP4_12h_D_2_U^2 + NWP4_12h_D_2_V^2)^1.5, 
                                V25 = (NWP1_00h_D_U^2 + NWP1_00h_D_V^2)^1.5,
                                V26 = (NWP1_06h_D_U^2 + NWP1_06h_D_V^2)^1.5,
                                V27 = (NWP1_12h_D_U^2 + NWP1_12h_D_V^2)^1.5,
                                V28 = (NWP1_18h_D_U^2 + NWP1_18h_D_V^2)^1.5,
                                V29 = (NWP2_00h_D_U^2 + NWP2_00h_D_V^2)^1.5,
                                V30 = (NWP2_12h_D_U^2 + NWP2_12h_D_V^2)^1.5,
                                V31 = (NWP3_00h_D_U^2 + NWP3_00h_D_V^2)^1.5,
                                V32 = (NWP3_06h_D_U^2 + NWP3_06h_D_V^2)^1.5,
                                V33 = (NWP3_12h_D_U^2 + NWP3_12h_D_V^2)^1.5,
                                V34 = (NWP3_18h_D_U^2 + NWP3_18h_D_V^2)^1.5,
                                V35 = (NWP4_00h_D_U^2 + NWP4_00h_D_V^2)^1.5,
                                V36 = (NWP4_12h_D_U^2 + NWP4_12h_D_V^2)^1.5, 
                                tot = V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+
                                  V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+
                                  V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36)


# split the train and test sets into 6 sub-sets for the six WF 

X1_train <- filter(X_train, WF == "WF1") 

X2_train <- filter(X_train, WF == "WF2")
X3_train <- filter(X_train, WF == "WF3")

X4_train <- filter(X_train, WF == "WF4")
X5_train <- filter(X_train, WF == "WF5")

X6_train <- filter(X_train, WF == "WF6")

X1_test <- filter(X_test, WF == "WF1")

X2_test <- filter(X_test, WF == "WF2")
X3_test <- filter(X_test, WF == "WF3")

X4_test <- filter(X_test, WF == "WF4")
X5_test <- filter(X_test, WF == "WF5")

X6_test <- filter(X_test, WF == "WF6")

# check the maximum of production per Wind Farm, as the supplementary data allowed us 
# to determine number and power of the wind turbines

max(X1_train$Production) # for 10.14 MW comprising 4 turbines of power 2.25 MW each, or 10.0
max(X2_train$Production) # for 11.92 MW comprising 6 turbines of power 2.0 MW each, or 12.0
max(X3_train$Production) # for 13.47 MW comprising 6 turbines of power 2.25 MW each, or 13.5
max(X4_train$Production) # for  9.05 MW comprising 4 turbines of power 2.25 MW each, or 10.0
max(X5_train$Production) # for 11.97 MW comprising 6 turbines of power 2.25 MW each, or 13.5
max(X6_train$Production) # for  5.07 MW comprising 2 turbines of power 2.0 MW each, or 5.0

# Arima procedure on mutated 'tot' 

tot<- ts(X1_train$tot)

plot(tot)

auto.arima(X1_train$Production)
auto.arima(tot)

# Series: tot 
# ARIMA(5,1,5) 

# Coefficients:
#   ar1      ar2     ar3     ar4     ar5     ma1     ma2     ma3     ma4      ma5
# -1.2254  -1.1678  0.2221  0.4537  0.4295  0.9415  0.6856  0.1521  0.0321  -0.1273
# s.e.   0.0609   0.0844  0.0864  0.0447  0.0259  0.0611  0.0707  0.0593  0.0473   0.0263

# sigma^2 estimated as 41300161:  log likelihood=-63544.81
# AIC=127111.6   AICc=127111.7   BIC=127185.8

X1.train <- Arima(tot, order=c(5,1,5))

X1_train_fit <- fitted(X1.train)

autoplot(tot) + autolayer(X1_train_fit)

# normalize X1_train_fit and compare/ correlate to Y1_train

Y1_train <- ts(select(X1_train, Production)) 

autoplot(X1_train_fit/30000) + autolayer(Y1_train)

# second work with the test set

totest<- ts(X1_test$tot)

plot(totest)

X1.test <- Arima(totest, model = X1.train)

X1_predic <- fitted(X1.test)

autoplot(totest) + autolayer(X1_predic)

# normalize X1_predic and compare/ correlate to max(Y1_test) that is unknown => take max(Y1_train)

autoplot(X1_predic/15000) # by inspection of maximum close to 10.0 

Y1_test <- X1_predic/15000

# now for all WFs # 2 to 6

# WF 2 Arima procedure on mutated 'tot' 

tot<- ts(X2_train$tot)

plot(tot)

auto.arima(tot)

# ARIMA(5,1,3) with drift 

# Coefficients:
#   ar1      ar2     ar3     ar4     ar5     ma1     ma2     ma3    drift
# -0.8077  -0.9033  0.2204  0.0733  0.1868  0.4006  0.3923  0.2021  -1.4281
# s.e.   0.0579   0.0697  0.0403  0.0441  0.0400  0.0575  0.0564  0.0209  38.0953

# sigma^2 estimated as 11334074:  log likelihood=-59511.98
# AIC=119043.9   AICc=119044   BIC=119111.3 

X2.train <- Arima(tot, order=c(5,1,3))

X2_train_fit <- fitted(X2.train)

autoplot(tot) + autolayer(X2_train_fit)

# normalize X2_train_fit and compare/ correlate to Y2_train

Y2_train <- ts(select(X2_train, Production)) 

autoplot(X2_train_fit/12000) + autolayer(Y2_train)

# second work with the test set

totest<- ts(X2_test$tot)

plot(totest)

X2.test <- Arima(totest, model = X2.train)

X2_predic <- fitted(X2.test)

autoplot(totest) + autolayer(X2_predic)

# normalize X2_predic and compare/ correlate to max(Y2_test) that is unknown => take max(Y2_train)

autoplot(X2_predic/10500) # by inspection of maximum close to 12.0 

Y2_test <- X2_predic/10500

# WF 3 Arima procedure on mutated 'tot' 

tot<- ts(X3_train$tot)

plot(tot)

auto.arima(tot)

# Series: tot 
# ARIMA(3,1,3) 

# Coefficients:
#   ar1      ar2     ar3     ma1     ma2     ma3
# -0.5532  -0.5331  0.3894  0.1948  0.2113  0.2273
# s.e.   0.0334   0.0332  0.0326  0.0334  0.0228  0.0169

# sigma^2 estimated as 8872060:  log likelihood=-58749.55
# AIC=117513.1   AICc=117513.1   BIC=117560.3

X3.train <- Arima(tot, order=c(3,1,3))

X3_train_fit <- fitted(X3.train)

autoplot(tot) + autolayer(X3_train_fit)

# normalize X3_train_fit and compare/ correlate to Y3_train

Y3_train <- ts(select(X3_train, Production)) 

autoplot(X3_train_fit/12000) + autolayer(Y3_train)

# second work with the test set

totest<- ts(X3_test$tot)

plot(totest)

X3.test <- Arima(totest, model = X3.train)

X3_predic <- fitted(X3.test)

autoplot(totest) + autolayer(X3_predic)

# normalize X3_predic and compare/ correlate to max(Y3_test) that is unknown => take max(Y3_train)

autoplot(X3_predic/8500) # by inspection of maximum close to 13.5 

Y3_test <- X3_predic/8500

# WF 4 Arima procedure on mutated 'tot' 

tot<- ts(X4_train$tot)

plot(tot)

auto.arima(tot)

# Series: tot 
# ARIMA(5,1,3) 

# Coefficients:
#   ar1     ar2     ar3      ar4      ar5      ma1      ma2     ma3
# -0.0098  0.1930  0.7041  -0.2151  -0.3882  -0.4376  -0.3576  0.3331
# s.e.   0.0521  0.0522  0.0231   0.0398   0.0369   0.0510   0.0537  0.0180

# sigma^2 estimated as 17221103:  log likelihood=-60817.44
# AIC=121652.9   AICc=121652.9   BIC=121713.5

X4.train <- Arima(tot, order=c(5,1,3))

X4_train_fit <- fitted(X4.train)

autoplot(tot) + autolayer(X4_train_fit)

# normalize X4_train_fit and compare/ correlate to Y4_train

Y4_train <- ts(select(X4_train, Production)) 

autoplot(X4_train_fit/15000) + autolayer(Y4_train)

# second work with the test set

totest<- ts(X4_test$tot)

plot(totest)

X4.test <- Arima(totest, model = X4.train)

X4_predic <- fitted(X4.test)

autoplot(totest) + autolayer(X4_predic)

# normalize X4_predic and compare/ correlate to max(Y4_test) that is unknown => take max(Y4_train)

autoplot(X4_predic/11000) # by inspection of maximum close to 9.0 

Y4_test <- X4_predic/11000

# WF 5 Arima procedure on mutated 'tot' 

tot<- ts(X5_train$tot)

plot(tot)

auto.arima(tot)

# Series: tot 
# ARIMA(3,1,3) 

# Coefficients:
#   ar1      ar2     ar3     ma1     ma2     ma3
# -0.6152  -0.5894  0.3343  0.1012  0.1462  0.2912
# s.e.   0.0345   0.0344  0.0337  0.0337  0.0192  0.0148

# sigma^2 estimated as 15300141:  log likelihood=-59877.79
# AIC=119769.6   AICc=119769.6   BIC=119816.7

X5.train <- Arima(tot, order=c(3,1,3))

X5_train_fit <- fitted(X5.train)

autoplot(tot) + autolayer(X5_train_fit)

# normalize X3_train_fit and compare/ correlate to Y3_train

Y5_train <- ts(select(X5_train, Production)) 

autoplot(X5_train_fit/15000) + autolayer(Y5_train)

# second work with the test set

totest<- ts(X5_test$tot)

plot(totest)

X5.test <- Arima(totest, model = X5.train)

X5_predic <- fitted(X5.test)

autoplot(totest) + autolayer(X5_predic)

# normalize X5_predic and compare/ correlate to max(Y5_test) that is unknown => take max(Y5_train)

autoplot(X5_predic/10000) # by inspection of maximum close to 12.0 

Y5_test <- X5_predic/10000

# WF 6 Arima procedure on mutated 'tot' 

tot<- ts(X6_train$tot)

plot(tot)

auto.arima(tot)

# Series: tot 
# ARIMA(3,1,1) 

# Coefficients:
#   ar1      ar2     ar3      ma1
# -0.2579  -0.2399  0.6842  -0.1608
# s.e.   0.0147   0.0135  0.0132   0.0197

# sigma^2 estimated as 9352680:  log likelihood=-58915.01
# AIC=117840   AICc=117840   BIC=117873.7

X6.train <- Arima(tot, order=c(3,1,1))

X6_train_fit <- fitted(X6.train)

autoplot(tot) + autolayer(X6_train_fit)

# normalize X6_train_fit and compare/ correlate to Y6_train

Y6_train <- ts(select(X6_train, Production)) 

autoplot(X6_train_fit/18000) + autolayer(Y6_train)

# second work with the test set

totest<- ts(X6_test$tot)

plot(totest)

X6.test <- Arima(totest, model = X6.train)

X6_predic <- fitted(X6.test)

autoplot(totest) + autolayer(X6_predic)

# normalize X6_predic and compare/ correlate to max(Y6_test) that is unknown => take max(Y6_train)

autoplot(X6_predic/14000) # by inspection of maximum close to 5.0 

Y6_test <- X6_predic/14000

# recap all Y_predict or Y1_test to Y6_test

Yt <-   rbind(as.data.frame(as.matrix(Y1_test)), 
              as.data.frame(as.matrix(Y2_test)),
              as.data.frame(as.matrix(Y3_test)), 
              as.data.frame(as.matrix(Y4_test)),
              as.data.frame(as.matrix(Y5_test)), 
              as.data.frame(as.matrix(Y6_test))) 

colnames(Yt) <- c("Production")

Yt1 <-  select(Y_test_r, ID)

Y_true10 <- cbind(Yt1, Yt)

Y_true10 <- replace(Y_true10, Y_true10 < 0, 0)

write.csv(Y_true10, row.names = FALSE, paste0("Y_true_10",".csv"))

# Random forest

# the random forest algorithm is used on the 102+37 = 139 predictors

# 1 

mat1 <- select(X1_train,  starts_with("NWP"), V1:V36, tot)

mat1t <- select(X1_test,  starts_with("NWP"), V1:V36, tot)

model1 <- randomForest(x = as.matrix(mat1), y = as.matrix(X1_train$Production),
                       ntree = 46) # number of trees

# check out the details
model1

y_hat1 <- predict(model1, as.matrix(mat1t))

plot(as.ts(3*y_hat1))

# 2 

mat2 <- select(X2_train,  starts_with("NWP"), V1:V36, tot)

mat2t <- select(X2_test,  starts_with("NWP"), V1:V36, tot)

model2 <- randomForest(x = as.matrix(mat2), y = as.matrix(X2_train$Production),
                       ntree = 46) # number of trees

# check out the details
model2

y_hat2 <- predict(model2, as.matrix(mat2t))

plot(as.ts(8*y_hat2))

# 3

mat3 <- select(X3_train,  starts_with("NWP"), V1:V36, tot)

mat3t <- select(X3_test,  starts_with("NWP"), V1:V36, tot)

model3 <- randomForest(x = as.matrix(mat3), y = as.matrix(X3_train$Production),
                       ntree = 46) # number of trees

# check out the details
model3

y_hat3 <- predict(model3, as.matrix(mat3t))

plot(as.ts(7.2*y_hat3))

# 4 

mat4 <- select(X4_train,  starts_with("NWP"), V1:V36, tot)

mat4t <- select(X4_test,  starts_with("NWP"), V1:V36, tot)

model4 <- randomForest(x = as.matrix(mat4), y = as.matrix(X4_train$Production),
                       ntree = 46) # number of trees

# check out the details
model4

y_hat4 <- predict(model4, as.matrix(mat4t))

plot(as.ts(8.5*y_hat4))

# 5

mat5 <- select(X5_train,  starts_with("NWP"), V1:V36, tot)

mat5t <- select(X5_test,  starts_with("NWP"), V1:V36, tot)

model5 <- randomForest(x = as.matrix(mat5), y = as.matrix(X5_train$Production),
                       ntree = 46) # number of trees

# check out the details
model5

y_hat5 <- predict(model5, as.matrix(mat5t))

plot(as.ts(10*y_hat5))

# 6 

mat6 <- select(X6_train,  starts_with("NWP"), V1:V36, tot)

mat6t <- select(X6_test,  starts_with("NWP"), V1:V36, tot)

model6 <- randomForest(x = as.matrix(mat6), y = as.matrix(X6_train$Production),
                       ntree = 46) # number of trees

# check out the details
model6

y_hat6 <- predict(model6, as.matrix(mat6t))

plot(as.ts(4*y_hat6))

# recap all Y_predict or Y1 to 6 _ test

Yt <-   rbind(as.data.frame(as.matrix(y_hat1)), 
              as.data.frame(as.matrix(y_hat2)),
              as.data.frame(as.matrix(y_hat3)), 
              as.data.frame(as.matrix(y_hat4)),
              as.data.frame(as.matrix(y_hat5)), 
              as.data.frame(as.matrix(y_hat6))) 

colnames(Yt) <- c("Production")

Yt1 <-  select(Y_test_r, ID)  

Y_true11 <- cbind(Yt1, Yt)

sum(Y_true11 < 0)  # no zero values

write.csv(Y_true11, row.names = FALSE, paste0("Y_true_11",".csv"))

plot(ts(Y_true11[,2]))
