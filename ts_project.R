library(xts)
Sys.setlocale("LC_ALL", "English") # to have labels (months) on graphs in English

df_website <- read.csv("daily-website-visitors.csv")
head(df_website)
df_website$Returning.Visits <- as.numeric(gsub(",", ".", gsub("\\.", "", df_website$Returning.Visits)))
df_website$Page.Loads <- as.numeric(gsub(",", ".", gsub("\\.", "", df_website$Page.Loads)))
df_website$Date <- as.Date(df_website$Date,format = "%m/%d/%Y")

# time series object
ts_website <- xts(df_website$Returning.Visits, df_website$Date)
plot(ts_website)

ts_website <- xts(df_website$Page.Loads, df_website$Date)
plot(ts_website)

# I also checked columns Unique.Visits and First.Time.Visits but a plot didn't look nice

# aggregation to weekly time series
ts_website_weekly <- apply.weekly(ts_website, mean)
plot(ts_website_weekly)

# aggregation to monthly time series
ts_website_monthly <- apply.monthly(ts_website, mean)
plot(ts_website_monthly)
