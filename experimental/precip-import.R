
library(ggplot2)
library(plotly)

url = "https://data.geo.admin.ch/ch.meteoschweiz.ogd-nime/ghs/ogd-nime_ghs_d_recent.csv"
#url = "https://data.geo.admin.ch/ch.meteoschweiz.ogd-nime/ghs/ogd-nime_ghs_d_historical.csv"
url = "https://data.geo.admin.ch/ch.meteoschweiz.ogd-nime/szb/ogd-nime_szb_d_recent.csv"
url = "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn-precip/mur/ogd-smn-precip_mur_d_recent.csv"
url = "https://data.geo.admin.ch/ch.meteoschweiz.ogd-nime/reg/ogd-nime_reg_d_recent.csv"
precip <- read.csv(url, sep = ";")
precip
str(precip)

precip$DateTime <- as.POSIXct(precip$reference_timestamp, 
                                      format="%d.%m.%Y %H:%M") 
# date in the format: YearMonthDay Hour:Minute 

hist(precip$rre150d0)
#precip.boulder$HPCP[precip.boulder$HPCP==999.99] <- NA 
#sum(is.na(precip.boulder))


precPlot_hourly <- ggplot(data=precip,  # the data frame
                          aes(DateTime, rre150d0)) +   # the variables of interest
  geom_bar(stat="identity") +   # create a bar graph
  xlab("Date") + ylab("Precipitation (mm)") +  # label the x & y axes
  ggtitle("Daily Precipitation")  # add a title

precPlot_hourly

ggplotly(precPlot_hourly)
