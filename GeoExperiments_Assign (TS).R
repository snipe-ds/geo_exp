### Geo Experiments Environment Setup & Assign Script (With TS Data) ###
 #PSA - The built in functions here require a rigid data scheme with a time dimension that might be hard to source from an advertiser before a campaign.


### Load Packages
library(GeoexperimentsResearch) #core geo experiment functions
library(tidyverse)  # data manipulation
library (anomalize) #anomaly id functions


### Input Variables
csv.import <- paste0(getwd(), "/geotsvar1.csv") # file name of the geo timeseries data
csv.export <- paste(Sys.Date(), "geoassignTS.csv", sep = " ") # file name of the geo assignment export
nperstratum <- 4 # set the number of geos you would like per stratum / segment

### Load Data
geotsvar1 <- read.csv (file = csv.import, colClasses = c(date="Date", geo="character", var1="numeric"))


### Anomaly Detection
geosalestrend <- geotsvar1 %>% select (date, var1) %>% group_by(date) %>% summarize(sum(var1)) #summarize var1 by date
colnames(geosalestrend) <- c("date", "var1_sum") #clean up column names
geosalestrendanom <- geosalestrend %>% time_decompose(var1_sum, merge = TRUE) %>% anomalize(remainder) %>% time_recompose() #anomoly detection and flagging in the dataframe
plot_anomalies(geosalestrendanom) #simple plot of the anomalous observations
plot_anomaly_decomposition(geosalestrendanom) #more detailed plot including underlying trends


### Univariate Stratified Random Geo Assignment
geoassign <- Randomize(ExtractGeoStrata(GeoTimeseries(geotsvar1, metrics="var1"), volume="var1", n.groups=nperstratum)) #stratifies by volume, strata sizes of n.groups, and randomly assigns 1:n.groups to each geo within a strata


### Export Results
write_csv(geoassign, file = csv.export) #Export csv in the working direct


### Other
### Pre-Analysis Estimation - Very WIP
#geosalesnts <- GeoTimeseries(geosales, metrics="var1")
#pre.predict <- DoROASPreanalysis(geosalesnts, response="var1", geos=geoassign, prop.to="var1", period.lengths=c(20, 15, 7))
#pre.summary <- summary(pre.predict, level=0.90, type="one-sided", precision=1.0)
#view(pre.summary)


