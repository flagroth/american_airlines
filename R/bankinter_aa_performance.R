rm(list=ls())
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
source("R/aux_functions.R")

# Uncomment to download and unzip data into data folder (may take a while)
# getData()
# Uncomment to merge monthly files into a yearly file (may take a while)
# mergeFiles()

perf <- fread("data/On_Time_On_Time_Performance_2016_full.csv", header = T, data.table = FALSE, stringsAsFactors = T)

perf_aa <- filter(perf,Carrier=="AA")
perf_aa <- droplevels(perf_aa)

perf <- filter(perf,Carrier!="AA")
perf <- droplevels(perf)

# Departures by airport
# AA
departures_aa <- 
  perf_aa %>% 
  group_by(OriginCityName) %>% 
  count() 

departures_aa <- 
  departures_aa %>% 
  filter(n>10000)
# Others
departures_others <- 
  perf %>% 
  filter(OriginCityName %in% departures_aa$OriginCityName) %>% 
  group_by(OriginCityName) %>% 
  count() 

# Informed departure delay
# AA
mean_delay_aa <- 
  perf_aa %>% 
  filter(!is.na(DepDelayMinutes) &
           DepDelayMinutes>0 &
           !is.na(CarrierDelay) &
           !is.na(WeatherDelay) &
           !is.na(NASDelay) &
           !is.na(SecurityDelay) &
           !is.na(LateAircraftDelay)
  ) %>%
  group_by(OriginCityName) %>% 
  select(OriginCityName,DepDelayMinutes) %>%
  summarise_each(funs=c("mean","sd")) 

# Others
mean_delay_others <- 
  perf %>% 
  filter(OriginCityName %in% departures_aa$OriginCityName &
           !is.na(DepDelayMinutes) &
           DepDelayMinutes>0 &
           !is.na(CarrierDelay) &
           !is.na(WeatherDelay) &
           !is.na(NASDelay) &
           !is.na(SecurityDelay) &
           !is.na(LateAircraftDelay)
  ) %>%
  group_by(OriginCityName) %>% 
  select(OriginCityName,DepDelayMinutes) %>%
  summarise_each(funs=c("mean","sd")) 

# Why is AA having delays?
delays_by_airport_aa <- 
  perf_aa %>% 
  filter(!is.na(DepDelayMinutes) & 
           DepDelayMinutes>0 &
           !is.na(CarrierDelay) &
           !is.na(WeatherDelay) &
           !is.na(NASDelay) &
           !is.na(SecurityDelay) &
           !is.na(LateAircraftDelay)
  ) %>%  
  group_by(OriginCityName) %>% 
  select(OriginCityName,DepDelayMinutes,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay) %>% 
  summarise_each(funs=c("mean","sd"))

# Why are the other carriers having delays?
delays_by_airport_others <- 
  perf %>% 
  filter(OriginCityName %in% departures_aa$OriginCityName &
           !is.na(DepDelayMinutes) & 
           DepDelayMinutes>0 &
           !is.na(CarrierDelay) &
           !is.na(WeatherDelay) &
           !is.na(NASDelay) &
           !is.na(SecurityDelay) &
           !is.na(LateAircraftDelay)
  ) %>%  
  group_by(OriginCityName) %>% 
  select(OriginCityName,DepDelayMinutes,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay) %>% 
  summarise_each(funs=c("mean","sd"))

# Let's join the number of departures with the reason of the delays
# AA
delays_by_airport_aa <- 
  departures_aa %>% 
  left_join(delays_by_airport_aa) %>% 
  arrange(desc(DepDelayMinutes_mean,Distance_mean,n))

delays_by_airport_aa[is.na(delays_by_airport_aa)] <- 0

# Others
delays_by_airport_others <- 
  departures_others %>% 
  left_join(delays_by_airport_others)%>% 
  arrange(desc(DepDelayMinutes_mean,Distance_mean,n))

delays_by_airport_others[is.na(delays_by_airport_others)] <- 0

print(delays_by_airport_aa[1:10,])
print(delays_by_airport_others[1:10])

# Correlación de número de viajes
cor(delays_by_airport_aa[,2:9])[1,]
cor(delays_by_airport_others[,2:9])[1,]

# Correlaciones de distancia
cor(delays_by_airport_aa[,2:9])[2,]
cor(delays_by_airport_others[,2:9])[2,]

delay <- summarise(perf_aa %>% group_by(Month,DistanceGroup),
                   count = n(),
                   dist = mean(Distance, na.rm = TRUE),
                   delay = mean(CarrierDelay, na.rm = TRUE))

delay <- filter(delay, !is.nan(delay))

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  labs(x="Distancia (millas)", y="Avión tarde (mins.)") +
  geom_smooth(method = 'gam') +
  scale_size_area() +
  ggtitle("Relación entre distancia \ny retrasos en los vuelos (por meses)") +
  scale_radius(name="Núm. vuelos") +
  facet_wrap(~ Month,nrow = 4)

delay_others <- summarise(perf %>% group_by(Month,DistanceGroup),
                   count = n(),
                   dist = mean(Distance, na.rm = TRUE),
                   delay = mean(CarrierDelay, na.rm = TRUE))

delay_others <- filter(delay_others, !is.nan(delay))

ggplot(delay_others, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  labs(x="Distancia (millas)", y="Avión tarde (mins.)") +
  geom_smooth(method = 'gam') +
  scale_size_area() +
  ggtitle("Relación entre distancia \ny retrasos en los vuelos (por meses)") +
  scale_radius(name="Núm. vuelos") +
  facet_wrap(~ Month,nrow = 4)


ps_by_carrier <- rbind(perf,perf_aa) %>%
  filter(!is.na(CarrierDelay) &
           OriginCityName=="San Antonio, TX"
  ) %>%
  group_by( Carrier)

p0 <- ggplot(ps_by_carrier, aes(Carrier, DepDelayMinutes)) +
  geom_boxplot(outlier.shape=NA) +
  labs(x="Carrier",y="Delay") +
  ggtitle("Departure delay in San Antonio, TX")

ylim1 = boxplot.stats(ps_by_carrier$DepDelayMinutes)$stats[c(1, 5)]

p0 <- p0 + coord_cartesian(ylim = ylim1*1.05)

plot(p0)

p1 <- ggplot(ps_by_carrier, aes(Carrier, CarrierDelay)) +
  geom_boxplot(outlier.shape=NA) +
  labs(x="Carrier",y="Delay") +
  ggtitle("Departure delay caused by carrier in San Antonio, TX")

ylim1 = boxplot.stats(ps_by_carrier$CarrierDelay)$stats[c(1, 5)]

p1 <- p1 + coord_cartesian(ylim = ylim1*1.05)

plot(p1)

p2 <- ggplot(ps_by_carrier, aes(Carrier, LateAircraftDelay)) +
  geom_boxplot(outlier.shape=NA) +
  labs(x="Carrier",y="Delay") +
  ggtitle("Departure delay caused by late aircraft in San Antonio, TX")

ylim1 = boxplot.stats(ps_by_carrier$LateAircraftDelay)$stats[c(1, 5)]

p2 <- p2 + coord_cartesian(ylim = ylim1*1.05)

plot(p2)



x <- data_frame(perf_aa$CarrierDelay,perf_aa$TaxiIn,perf_aa$TaxiOut)
x <- x[complete.cases(x),]
colnames(x) <- c("delay","taxiIn","taxiOut")
summary(x)
summary(glm(data=x,delay~.))
