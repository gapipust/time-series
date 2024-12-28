library(xts)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(astsa)

Sys.setlocale("LC_ALL", "English") # to have labels (months) on graphs in English

# df_website <- read.csv("daily-website-visitors.csv")
df_website <- read.csv("daily-website-visitors.csv")
head(df_website)
df_website$Returning.Visits <- as.numeric(gsub(",", ".", gsub("\\.", "", df_website$Returning.Visits)))
df_website$Page.Loads <- as.numeric(gsub(",", ".", gsub("\\.", "", df_website$Page.Loads)))
df_website$Date <- as.Date(df_website$Date,format = "%m/%d/%Y")

######################### EDA ################################
# time series object
# ts_website <- xts(df_website$Returning.Visits, df_website$Date)
# plot(ts_website)
# I chose Page.Loads but later we can try also Returning.Visits
ts_website <- xts(df_website$Page.Loads, df_website$Date)
plot(ts_website)
plot(ts_website, main = "Daily Page Loads", ylab = "Page Loads", xlab = "Date")

# I also checked columns Unique.Visits and First.Time.Visits but a plot didn't look nice

# Training set: First 4.5 years, Test set: Last 6 months
cutoff_date <- as.Date("2020-02-19")
train_data <- window(ts_website, end = cutoff_date)
test_data <- window(ts_website, start = cutoff_date + 1)
plot(train_data, main = "Training and Test Data", col = "blue", xlim = range(index(ts_website)), ylab = "Page Loads")
lines(test_data, col = "red")
legend("topright", legend = c("Training", "Test"), col = c("blue", "red"), lty = 1)

summary(df_website$Page.Loads)

ggplot(df_website, aes(Page.Loads))+
  geom_histogram(fill="peachpuff3",color="black")+
  labs(x="Page Loads")+
  theme_minimal()

ggplot(df_website,aes(x=factor(Day,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),y=Page.Loads, group=Day))+
  geom_boxplot(aes(fill=Day))+
  labs(x="Day of the week", y="Page Loads")+
  theme_minimal()+
  theme(legend.position = "none")

# Check the stationarity - I tried for the ARMA models but later I did with seasonality
adf.test(ts_website, alternative = "stationary")
# TS is stationary

acf2(ts_website,max.lag= 80)

lag1.plot(ts_website,16)

# Removing seasonal component
dlm1 <- diff(train_data,1)
plot(dlm1,lwd=1,main=expression(paste(Delta, "Page Loads train")))
acf2(dlm1,main=expression(paste(Delta, "Page Loads train")))

dlm7 <- diff(train_data,7)
plot(dlm7,lwd=1,,main=expression(paste(Delta[7], "Page Loads train")))
acf2(dlm7,main=expression(paste(Delta[7], "Page Loads train")))

dlm7.1 <- diff(diff(train_data,7),1)
plot(dlm7.1,lwd=1,main=expression(paste(Delta,Delta[7], "Page Loads train")))
acf2(dlm7.1,main=expression(paste(Delta,Delta[7], "Page Loads train")))


######################## Weekly Aggregation ##########################
ts_website_weekly <- apply.weekly(ts_website, mean)
plot(ts_website_weekly, main = "Weekly Average Page Loads", ylab = "Page Loads", xlab = "Date")

# Training set: First 4.5 years, Test set: Last 6 months
cutoff_date <- as.Date("2020-02-19")
train_data_weekly <- window(ts_website_weekly, end = cutoff_date)
test_data_weekly <- window(ts_website_weekly, start = cutoff_date + 1)

plot(train_data_weekly, main = "Weekly Page Loads", ylab = "Page Loads", xlab = "Date")

acf2(ts_website_weekly,max.lag= 80)

adf.test(ts_website_weekly, alternative = "stationary")

######################## Monthly Aggregation #########################
ts_website_monthly <- apply.monthly(ts_website, mean)
plot(ts_website_monthly, main = "Monthly Average Page Loads", ylab = "Page Loads", xlab = "Date")

# Training set: First 4.5 years, Test set: Last 6 months
cutoff_date <- as.Date("2020-02-29")
train_data_monthly <- window(ts_website_monthly, end = cutoff_date)
test_data_monthly <- window(ts_website_monthly, start = cutoff_date + 1)

plot(train_data_monthly, main = "Weekly Page Loads", ylab = "Page Loads", xlab = "Date")

acf2(ts_website_monthly)

adf.test(ts_website_monthly, alternative = "stationary") # not stationary

######################### Models ##############################

# we see seasonality trend in ACF and PACF (lag 7 = weekly seasonality)

# seasonal component
ts_seasonal_diff <- diff(ts_website, lag = 7)
ts_seasonal_diff <- na.omit(ts_seasonal_diff)
plot(ts_seasonal_diff, main = "Seasonally Differenced Series")
acf(ts_seasonal_diff, main = "ACF of Seasonally Differenced Series")
pacf(ts_seasonal_diff, main = "PACF of Seasonally Differenced Series")

# SARIMA
dlm1 = diff(ts_website,1)
dlm7 = diff(ts_website,14)
dlm7.1 = diff(diff(ts_website,14),1)
dlm7.1 <- na.omit(dlm7.1)

plot(dlm7.1, main = expression(paste(Delta, Delta[7], "daily_visitors")))

acf(dlm7.1, main=expression(paste("ACF for ", Delta, Delta[7], "daily_visitors")))
pacf(dlm7.1, main=expression(paste("PACF for ", Delta, Delta[7], "daily_visitors")))

visitors.fit1=sarima(ts_website,0,1,0,1,1,0,7)
visitors.fit1
# AIC 0.89

visitors.fit2=sarima(ts_website,1,1,0,1,1,0,7)
visitors.fit2
# AIC 0.89

visitors.fit3=sarima(ts_website,1,1,1,1,1,0,7)
visitors.fit3
# AIC 0.81

visitors.fit4=sarima(ts_website,1,1,1,1,1,1,7)
visitors.fit4
# AIC 0.64

visitors.fit5=sarima(ts_website,5,1,3,0,1,2,7)
visitors.fit5
# AIC 0.607 - optimal I guess (I tried modifying the values)

# for weekly series - yearly seasonality, optimal model
visitors.fit6=sarima(ts_website_weekly,3,1,0,2,1,1,52)
visitors.fit6

# for monthly series - yearly seasonality, optimal model
visitors.fit7=sarima(ts_website_monthly,0,1,0,0,1,1,12)
visitors.fit7

# some examples of biweekly and monthly seasonality with daily and weekly data - I can make some more if we want to include it, but the best results were for others
visitors.fit8=sarima(ts_website_weekly,3,1,0,2,1,1,4)
visitors.fit8

visitors.fit9=sarima(ts_website,5,1,3,0,1,2,14)
visitors.fit9


# from here was just playing, I thing that nothing is particularly useful, but I leave it here
#sarima_model <- auto.arima(ts_seasonal_diff, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
#summary(sarima_model)

#checkresiduals(sarima_model)

#manual_sarima <- arima(ts_website, order = c(2, 0, 5), seasonal = list(order = c(1, 1, 4), period = 14))
#summary(manual_sarima)

#checkresiduals(manual_sarima)

# Set the frequency of the time series to 7 for weekly seasonality
#ts_website <- ts(ts_website, frequency = 7)

# Generate Fourier terms with K = 3
#fourier_terms <- fourier(ts_website, K = 3)

# Fit SARIMA model with Fourier terms as external regressors
#sarima_with_fourier <- auto.arima(ts_website, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, xreg = fourier_terms)
#summary(sarima_with_fourier)

# Check residuals
#checkresiduals(sarima_with_fourier)

#ts_seasonal_diff_2 <- diff(ts_website, lag = 7, differences = 2)

#manual_sarima <- arima(ts_seasonal_diff, order = c(2, 0, 5), seasonal = list(order = c(2, 0, 4), period = 7))
#summary(manual_sarima)


# Fit the SARIMA model to the once-differenced series (you can start with auto.arima)
#sarima_model_1 <- auto.arima(ts_seasonal_diff, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
#summary(sarima_model_1)

# Check residuals of the model
#checkresiduals(sarima_model_1)


# Apply seasonal differencing twice (D = 2) to your time series
#ts_seasonal_diff_2 <- diff(ts_website, lag = 7, differences = 2)
#ts_seasonal_diff_2 <- na.omit(ts_seasonal_diff_2)  # Remove any NA values resulting from differencing

# Plot the twice-differenced series
#plot(ts_seasonal_diff_2, main = "Seasonally Differenced Series (D = 2)")

# Check ACF and PACF of the twice-differenced series
#acf(ts_seasonal_diff_2, main = "ACF of Twice-Differenced Series")
#pacf(ts_seasonal_diff_2, main = "PACF of Twice-Differenced Series")

# Fit the SARIMA model to the twice-differenced series (you can start with auto.arima)
#sarima_model_2 <- auto.arima(ts_seasonal_diff_2, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
#summary(sarima_model_2)

# Check residuals of the model
#checkresiduals(sarima_model_2)

#sarima_model_2_manual <- arima(ts_seasonal_diff_2, order = c(2, 0, 5), seasonal = list(order = c(2, 0, 4), period = 7))
#summary(sarima_model_2_manual)
