library(dplyr)
library(lubridate)

##getting the data
data<- data.frame()
z<-c(2016,2017,2018,2019,2020)
for(i in z) {
  url <- paste("http://www.aire.cdmx.gob.mx/opendata/promedios_diarios/promedios_",i,"_ps.csv", sep = "")
  path <- paste("C:/Users/jesse/Desktop/R/dplyr",i, sep = "")
  download.file(url, path)
  data <- rbind(data, read.csv(path,header = T, skip = 8))
}

pm<- tibble(data)
rm(data)
summary(pm)


##clean
pmclean <- pm %>%
  mutate(datex = strptime(date,"%d/%m/%Y"), month = month(datex), year = year(datex)) %>%
  filter(!is.na(value)) %>%
  select(datex, id_station:value, month, year) %>%
  rename(date = datex)

pmsummary <- pmclean %>%  
  group_by(id_parameter, year, month) %>%
  summarize("mean" = mean(value), "median" = median(value), 
            "min" = min(value), "max" = max(value))

plot(pmsummary$month,pmsummary$mean, 
     col = ifelse(pmsummary$year == "2020", "coral1", "grey"),
     pch = ifelse(pmsummary$id_parameter == 'PM10', 19,2),
     xlab = "Month",
     ylab = "PM Mean Concentration (ug/m3)",
     cex = 2,
     type ="p" )
legend("bottomright", c("PM10", "PM2.5", "2020"),
       col = c("grey","grey", "coral1"),
       pch = c(20, 2, 1))

plot(pmsummary$month ~ pmsummary$mean)


pm10 <- pmclean %>%
  filter(id_parameter == "PM10") %>%
  select(date,id_station, value, month, year)
pm2.5 <- pmclean %>%
  filter(id_parameter == "PM2.5") %>%
  select(date, id_station, value, month, year)


