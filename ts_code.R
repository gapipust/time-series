library(xts)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
Sys.setlocale("LC_ALL", "English") # to have labels (months) on graphs in English

df_website <- read.csv("daily-website-visitors.csv")
head(df_website)
df_website$Returning.Visits <- as.numeric(gsub(",", ".", gsub("\\.", "", df_website$Returning.Visits)))
df_website$Page.Loads <- as.numeric(gsub(",", ".", gsub("\\.", "", df_website$Page.Loads)))
df_website$Date <- as.Date(df_website$Date,format = "%m/%d/%Y")

# time series object
ts_website <- xts(df_website$Returning.Visits, df_website$Date)
plot(ts_website)

# I chose Page.Loads but later we can try also Returning.Visits
ts_website <- xts(df_website$Page.Loads, df_website$Date)
plot(ts_website)
plot(ts_website, main = "Daily Page Loads", ylab = "Page Loads", xlab = "Date")

# I also checked columns Unique.Visits and First.Time.Visits but a plot didn't look nice

# Weekly Aggregation
ts_website_weekly <- apply.weekly(ts_website, mean)
plot(ts_website_weekly, main = "Weekly Average Page Loads", ylab = "Page Loads", xlab = "Date")

# Monthly Aggregation
ts_website_monthly <- apply.monthly(ts_website, mean)
plot(ts_website_monthly, main = "Monthly Average Page Loads", ylab = "Page Loads", xlab = "Date")

# summary(ts_website)
acf(ts_website, main = "ACF of Daily Page Loads")
pacf(ts_website, main = "PACF of Daily Page Loads")


# Training set: First 4.5 years, Test set: Last 6 months
cutoff_date <- as.Date("2020-02-19")
train_data <- window(ts_website, end = cutoff_date)
test_data <- window(ts_website, start = cutoff_date + 1)
plot(train_data, main = "Training and Test Data", col = "blue", xlim = range(index(ts_website)), ylab = "Page Loads")
lines(test_data, col = "red")
legend("topright", legend = c("Training", "Test"), col = c("blue", "red"), lty = 1)


