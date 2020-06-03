library(dplyr)
library(lubridate)

##getting the data
data<- data.frame()
z<-c(2016,2017,2018,2019,2020)
for(i in z) {
  url <- paste("http://www.aire.cdmx.gob.mx/opendata/promedios_diarios/promedios_",i,"_ps.csv", sep = "")
  path <- paste("C:/Users/jesse/Desktop/R/dplyr/data/",i, sep = "")
  download.file(url, path)
  data <- rbind(data, read.csv(path,header = T, skip = 8))
}
##create tibble and summary of tibble
pm <- tibble(data)
summary(pm)

##clean pm using dplyr functions
pmclean <- pm %>%
  mutate(datex = strptime(date,"%d/%m/%Y"), 
         month = month(datex), 
         year = year(datex)) %>%
  filter(!is.na(value)) %>%
  select(datex, id_station:value, month, year) %>%
  rename(date = datex)
##create a tibble giving summary datat by month and year
pmsummary <- pmclean %>%  
  group_by(id_parameter, year, month) %>%
  summarize("mean" = mean(value), "median" = median(value), 
            "min" = min(value), "max" = max(value))

    ##Graph the summary data
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

## download Mexico City air quality monitoring station data
url <- "http://www.aire.cdmx.gob.mx/opendata/catalogos/cat_estacion.csv"
path <- "C:/Users/jesse/Desktop/R/dplyr/stations.csv"
download.file(url, path)
datastations <- tibble(read.csv(path,header = T, skip = 1))

##select columns and rename our join column
stations <- datastations %>%
   select(cve_estac:obs_estac)%>%
   rename(id_station = cve_estac)

##join clean PM data with station data
pmstations<- inner_join(pmclean,stations)

##use arrange and desc to identify inconsistencies. 
arrange(pmstations,desc(obs_estac))

