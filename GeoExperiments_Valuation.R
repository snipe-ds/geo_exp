### Geo Experiments Environment Setup & Measure Script ###


### Load Packages
library(tidyverse) # data wrangling
library(GeoexperimentsResearch) # core geo experiment models
library (anomalize) # anomaly id functions


### Input Variables
csv.geo <- paste0(getwd(), "/geoassignment.csv") # file name of the geo assignments data set
csv.sc <- paste0(getwd(), "/salesandcost.csv") # file name of the sales and cost by date and geo data set
date.pre <- "2015-01-05" # set pre-wave start date from the data set
date.test <- "2015-02-16" # experiment start date
date.post <- "2015-03-15" # start of post-period
date.end <- "2015-04-07" # end of post
  

### Load Data
geoassignment <- read.csv (file = csv.geo, 
                           colClasses = c(geo="character", geo.group="integer"))

salesandcost <- read.csv (file = csv.sc, 
                          colClasses = c(date="Date", geo="character", sales="numeric", cost="numeric"))


### Anomaly Detection
geosalestrend <- salesandcost %>% select (date, sales) %>% group_by(date) %>% summarize(sum(sales)) #sum sales by date
colnames(geosalestrend) <- c("date", "sales") #clean up column names
geosalestrendanom <- geosalestrend %>% time_decompose(sales, merge = TRUE) %>% anomalize(remainder) %>% time_recompose() #anomoly detection and flagging in the dataframe
plot_anomalies(geosalestrendanom) #simple plot of the anomalous observations
plot_anomaly_decomposition(geosalestrendanom) #more detailed plot including underlying trends


### Set Experiment Dates 
periods = ExperimentPeriods(
  period.dates = c(date.pre, date.test, date.post, date.end), 
  period.names = c("PreTest", "Test", "Cooldown"))


### Combine Inputs and Time-series Data
data = GeoExperimentData(GeoTimeseries(salesandcost, metrics=c("sales", "cost")), periods=periods, geo.assignment=GeoAssignment(geoassignment))
view(aggregate(data, by=c('period', 'geo.group'))) # check that sales and spend observations are in the groups and time-periods expected


### Valuation Functions (GBR and TBR)
GEOresult = DoGBRROASAnalysis(data, response='sales', cost='cost', pretest.period=0, intervention.period=1, cooldown.period=2, control.group=1, treatment.group=2)
TSresult = DoTBRAnalysis(data, response='sales', model='tbr1', pretest.period=0, intervention.period=1, cooldown.period=2, control.group=1, treatment.group=2)
TSROASresult = DoTBRROASAnalysis(data, response='sales', cost='cost', model='tbr1', pretest.period=0, intervention.period=1, cooldown.period=2, control.group=1, treatment.group=2)


### Valuation Reporting (GBR and TBR)
summary(TSresult) # absolute sales lift summary of TS based regression
summary(TSROASresult) # roas results summary of TS based regression
summary(GEOresult, level=0.90, threshold=3.0, interval.type="two-sided") # roas results summary of geo based regression
ROASFinal <- rbind(summary(TSROASresult), summary(GEOresult))

### Plots
plot(TSresult) #timeseries plot of predicted and observed sales in treament group


### Export Results
write_csv(ROASFinal, file = 'ROASFinal.csv') # exports a csv of both gbr and tbr valuation summaries
pdf("TSresult.pdf") # exports pdf of the tsr lift plots
plot(TSresult) # exports pdf of the tsr lift plots
dev.off() # exports pdf of the tsr lift plots

### Other
#geobalance <- geoassignment %>% group_by(geo.group) %>% tally() 
#aggregate(data, by='.weekindex') #summarize sales and cost by week
