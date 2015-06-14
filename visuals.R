library(ggmap)
library(scales)
library(plyr)
library('png')

options(scipen=999)
ds = read.csv('turnstile_weather_v2.csv')
ds$rain <- as.factor(ds$rain)
ds['turnover'] = ds['ENTRIESn_hourly'] + ds['EXITSn_hourly']
data_agg = stats::aggregate(turnover ~ DATEn + station + longitude + latitude, ds, sum)
data_st_agg = stats::aggregate(turnover ~ station + longitude + latitude, data_agg, mean)
data_st_agg = data_st_agg[data_st_agg$turnover > 0,]
ds_weather = ds[c('ENTRIESn_hourly', 'rain','fog')]
ds_weather$rain = revalue(ds_weather$rain, c('0' = 'Not raining', '1' = 'Raining'))


facets_plot = ggplot(aes(x=ENTRIESn_hourly), data=ds_weather) + 
  geom_histogram() + 
  facet_grid(. ~ rain) +
  ggtitle('Hourly entries to subway stations by rain') +
  xlab('Number of entries per hour') +
  theme_bw()
print(facets_plot)

watercolor <- get_map(location=c(lon=-73.91, lat=40.729335), 
                      source = "stamen",
                      maptype = "toner-lite",
                      zoom=11)

m <- ggmap(watercolor)  +
  geom_point(data = data_st_agg, aes(x=longitude, y=latitude,size=turnover),colour = "red",alpha = .5) +
  scale_size_area(labels = comma, name="Average daily turnover") +
  ggtitle('Passenger turnover in NYC subway stations') +
  theme(
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )
#png(paste("figure_4.png", sep=""), width = 900, height = 900)
print(m)  
#dev.off()

plot = ggplot(aes(x=ENTRIESn_hourly, fill=rain), data=ds) + geom_histogram(alpha=0.5, position='identity') + 
  xlab("Number of entries to subway station") + 
  ylab('') + 
  ggtitle('Distribution of entries to subway stations') +
  scale_x_continuous(labels = comma) + 
  scale_fill_discrete(labels=c("Not raining", "Raining")) +
  theme_bw() +
  theme(
    legend.title=element_blank()
  )
#png(paste("figure_3.png", sep=""), width = 900, height = 900)
print(plot)  
#dev.off()
