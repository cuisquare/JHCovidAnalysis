#remove objects in workspace
rm(list = ls())
#remove existing plots in RStudio
while (dev.cur()>1) dev.off()
#extracting the latest data (updating git + calculated data)
source("JHGetData.R", encoding='utf-8')
#JH plotting and data extracting functions
source("JHFunctions.R")

library(tidyverse)
library(lubridate)
options(digits = 3)   # report 3 significant digits

##check discrepancy UK Gov vs JH
#CompUKvsJH <- checkDiscJHvsUKGOV(master_data) #TODO broken, fix



##Historical Data - Best vs Worst
#Progress
CountryList <- c("United Kingdom","France","Italy","Germany","Belgium","Greece")

Progress_Increase_Deaths <- master_data %>% 
  JHGetProgress(COuntryList,"Increase_Deaths_Avg") 

Progress_Increase_Weighted_Deaths <- master_data %>%
  JHGetProgress(COuntryList,"Increase_Weighted_Deaths_Avg") 

Progress <- Progress_Increase_Deaths %>%
  left_join(Progress_Increase_Weighted_Deaths)

master_data %>%
  JHplot_CountryLevel(CountryList,"Increase_Weighted_Deaths_Avg_Avg")

master_data %>%
  filter(Date>ymd("20200601")) %>%
  JHplot_CountryLevel(CountryList,"Increase_Weighted_Deaths")


##Latest data
LatestData_UK <- getLatestData(master_data, "United Kingdom")

LatestData_20days_NoRegions_UK <- getLatestData(master_data, "United Kingdom",20) %>% filter(is.na(Province_State))
LatestData_20days_NoRegions_FR <- getLatestData(master_data, "France",20) %>% filter(is.na(Province_State))
LatestData_20days_NoRegions_IT <- getLatestData(master_data, "Italy",20) %>% filter(is.na(Province_State))
LatestData_20days_NoRegions_BL <- getLatestData(master_data, "Belgium",20) %>% filter(is.na(Province_State))


#weird coming back to life France
#TODO: filter for start date last date before first discrepancy
check_death <- getLatestData(master_data, "France",75) %>% 
  filter(is.na(Province_State)) %>%
  mutate(deaths_decrease = Deaths-lag(Deaths,1) <0) 
date_first_disc <- check_death %>%
  filter(deaths_decrease == TRUE) %>%
  slice_min(Date) %>% pull(Date)

check_death %>%
  filter(!is.na(deaths_decrease)) %>%
  filter(Date >= (date_first_disc-days(1))) %>%
  ggplot() +
  geom_line(aes(Date,Deaths)) +
  geom_point(aes(Date,Deaths,color=deaths_decrease))  

LatestData_FR <- getLatestData(master_data, "France")
LatestData_US <- getLatestData(master_data, "United States")

##Ranks
#TODO Maybe include ranks as mutated column ?
last_day <- master_data %>% pull(Date) %>% max()
ntop <- 50
top_Deaths <- master_data %>%
  filter(Date == last_day) %>%
  slice_max(order_by = Deaths,n=ntop) %>% 
  select(Country_Region,Deaths)
top_Weighted_Deaths <- master_data %>%
  filter(Date == last_day) %>%
  slice_max(order_by = Weighted_Deaths,n=ntop) %>% 
  select(Country_Region,Weighted_Deaths)
top_Weighted_Confirmed <- master_data %>%
  filter(Date == last_day) %>%
  slice_max(order_by = Weighted_Deaths,n=ntop) %>% 
  select(Country_Region,Weighted_Confirmed)
top_Increase_Weighted_Deaths <- master_data %>%
  filter(Date == last_day) %>%
  slice_max(order_by = Increase_Weighted_Deaths,n=ntop) %>%
  select(Country_Region,Increase_Weighted_Deaths)
top_Increase_Weighted_Confirmed <- master_data %>%
  filter(Date == last_day) %>%
  slice_max(order_by = Increase_Weighted_Confirmed,n=ntop) %>%
  select(Country_Region,Increase_Weighted_Confirmed)

CountryList_top10_Increase_Weighted_Deaths <- top_Increase_Weighted_Deaths %>% 
  slice_max(order_by = Increase_Weighted_Deaths,n=10) %>%
  pull(Country_Region)
#very jittery
#DONE rolling average
master_data %>%
  JHplot_CountryLevel(CountryList_top10_Increase_Weighted_Deaths,
                      "Increase_Weighted_Deaths_Avg_Avg")


#TODO create a function for this
nbottom <- 100
bottom_base <- master_data %>%
  filter(Date == last_day) %>%
  group_by(Country_Region,Date) %>%
  summarise(Deaths = sum(Deaths),
            Weighted_Deaths = sum(Weighted_Deaths),
            Weighted_Confirmed = sum(Weighted_Confirmed),
            Increase_Weighted_Deaths = sum(Increase_Weighted_Deaths),
            Increase_Weighted_Confirmed = sum(Increase_Weighted_Confirmed)) %>%
  ungroup() 
bottom_Deaths <- bottom_base %>%
  slice_min(order_by = Deaths,n=nbottom) %>% 
  select(Country_Region,Deaths)
bottom_Weighted_Deaths <- bottom_base %>%
  slice_min(order_by = Weighted_Deaths,n=nbottom) %>% 
  select(Country_Region,Weighted_Deaths)
bottom_Weighted_Confirmed <- bottom_base %>%
  slice_min(order_by = Weighted_Deaths,n=nbottom) %>% 
  select(Country_Region,Weighted_Confirmed)
bottom_Increase_Weighted_Deaths <- bottom_base %>%
  slice_min(order_by = Increase_Weighted_Deaths,n=nbottom) %>%
  select(Country_Region,Increase_Weighted_Deaths)
bottom_Increase_Weighted_Confirmed <- bottom_base %>%
  slice_min(order_by = Increase_Weighted_Confirmed,n=nbottom) %>%
  select(Country_Region,Increase_Weighted_Confirmed)

##Plots
#UK
master_data %>%
  filter(!is.na(Province_State)) %>%
  JHplot_ProvinceLevel("United Kingdom","Deaths")
master_data %>%
  JHplot_CountryLevel("United Kingdom","Increase_Deaths")
master_data %>%
  JHplot_CountryLevel("United Kingdom","Increase_Confirmed")

#Europe
CountryList <- c("France","United Kingdom","Spain","Germany","Italy","Belgium")
master_data %>%
  filter(Date > ymd("20200601")) %>%
  JHplot_CountryLevel(CountryList,"Increase_Deaths")
master_data %>%
  filter(Date > ymd("20200601")) %>%
  JHplot_CountryLevel(CountryList,"Increase_Weighted_Deaths")
master_data %>%
  filter(Date > ymd("20200601")) %>%
  JHplot_CountryLevel(CountryList,"Increase_Weighted_Confirmed")

#World
CountryList <- c("France",
                 "United Kingdom",
                 "Spain",
                 "Germany",
                 "United States",
                 "Brazil",
                 "Italy",
                 "Belgium",
                 "Croatia",
                 "Greece")
master_data %>%
  filter(Date > ymd("20200201")) %>%
  JHplot_CountryLevel(CountryList,"Weighted_Deaths")
master_data %>%
  filter(Date > ymd("20200201")) %>%
  JHplot_CountryLevel(CountryList,"Deaths")
master_data %>%
  filter(Date > ymd("20200201")) %>%
  JHplot_CountryLevel(CountryList,"Weighted_Confirmed")
master_data %>%
  filter(Date > ymd("20200201")) %>%
  JHplot_CountryLevel(CountryList,"Confirmed")
master_data %>%
  filter(Date > ymd("20200201")) %>%
  JHplot_CountryLevel(CountryList,"Increase_Weighted_Deaths")
master_data %>%
  filter(Date > ymd("20200201")) %>%
  JHplot_CountryLevel(CountryList,"Increase_Deaths")
master_data %>%
  filter(Date > ymd("20200201")) %>%
  JHplot_CountryLevel(CountryList,"Increase_Weighted_Confirmed")
master_data %>%
  filter(Date > ymd("20200201")) %>%
  JHplot_CountryLevel(CountryList,"Increase_Confirmed")

##exploration of robust country to country comparison


#France vs UK
check_UKFR <- master_data %>%
  filter(Country_Region %in% c("France","United Kingdom") & is.na(Province_State)) %>%
  select(Country_Region,Increase_Weighted_Deaths,Date) %>%
  filter(Date == last_day)
master_data %>%
  JHplot_CountryLevel(c("France","United Kingdom"),"Increase_Weighted_Deaths")

#at which date did France get as good as UK is now ?
#TODO: find peak and search from there, or define up and downs search, or get all candidates
#at moment only looking for the downward match from peak

Date_FranceWasUkNow <- JHDateYWasXWhen(
  master_data,
  "Increase_Weighted_Deaths",
  "United Kingdom",
  "France",
  getLatestAvailableDate(master_data,"United Kingdom"),
  ymd("20200501"))

#Italy vs UK
master_data %>%
  JHplot_CountryLevel(c("Italy","United Kingdom"),"Increase_Weighted_Deaths")

#at which date did Italy get as good as UK is now ?
Date_ItalyWasUkNow <- JHDateYWasXWhen(
  master_data,
  "Increase_Weighted_Deaths",
  "United Kingdom",
  "Italy",
  getLatestAvailableDate(master_data,"United Kingdom"),
  ymd("20200501"))

#TODO for each day, calculate the lag in days and plot vs Date
#TODO: this could be applied to any metric chosen deemed appropriate to compare (inc, incinc, incrate)

#TODO plot ranks over time ?

CountryList <- c("Italy","France","United Kingdom","Germany","Spain","Croatia","Greece")

master_data %>%
  JHplot_CountryLevel(CountryList,"Increase_Weighted_Deaths")

#at which date did Italy get as good as UK is now ?
Date_ItalyWasUkNow <- JHDateYWasXWhen(
  master_data,
  "Increase_Weighted_Deaths",
  "United Kingdom",
  "Italy",
  getLatestAvailableDate(master_data,"United Kingdom"),
  ymd("20200501"))
Lag_ItalyUkNow <-getLatestAvailableDate(master_data,"United Kingdom") - Date_ItalyWasUkNow #39

Date_ItalyWasUkEarlyJune <- JHDateYWasXWhen(
  master_data,
  "Increase_Weighted_Deaths",
  "United Kingdom",
  "Italy",
  ymd("20200601"),
  ymd("20200501"))
Lag_ItalyUkEarlyJune <- ymd("20200601") - Date_ItalyWasUkEarlyJune #23
#23 earlier vs 39 days now, does it mean UK is recovering slower than Italy or is this 
#explained solely by curve shape ?
#TODO: plot curve of UK translated by diff days to compare like for like
#TODO: get rate of variation which should contain same information numerically

#plot rate
master_data %>% 
  filter(Date > ymd(20200501)) %>%
  filter(Country_Region %in% c("Italy","France","United Kingdom","Germany")) %>%
  filter(is.na(Province_State)) %>%
  ggplot(aes(Date,Increase_Increase_Deaths,color = Country_Region)) +
  geom_point() + geom_line()

#negative means the increase curve is going down, positive means the increase curve
#is going up 
#TODO: compare that increase_increase at the same when increase had the same value
#current values of increase increase are about the same but UK starting from a much 
#higher value of increase

CountryList <- c("Italy","France","United Kingdom","Germany","Spain","Croatia","Greece")

master_data %>% 
  filter(Date > ymd(20200501)) %>%
  filter(Country_Region %in% CountryList) %>%
  filter(is.na(Province_State)) %>%
  ggplot(aes(Date,Increase_Deaths,color = Country_Region)) +
  geom_point() + geom_line()

#TODO: maybe increase increase should be divided by increase, so that it is weighted
#and comparable at a given time ?  

master_data %>% 
  filter(Date > ymd(20200601)) %>%
  filter(Country_Region %in% CountryList) %>%
  filter(is.na(Province_State)) %>%
  ggplot(aes(Date,Rate_Increase_Deaths,color = Country_Region)) +
  geom_point() + geom_line()

#at which date did Germany get as good as UK is now ?
Date_GermanyWasUkNow <- JHDateYWasXWhen(
  master_data,
  "Increase_Weighted_Deaths",
  "United Kingdom",
  "Germany",
  getLatestAvailableDate(master_data,"United Kingdom"),
  ymd("20200501"))

#TODO: get moving average
#TODOMAYBE: ? summarise results by country and create a "all provinces" value (for countries which don't have national values)


##Committee meeting

##Historical Data - Best vs Worst
CountryList <- c("United Kingdom","France","Italy","Germany","Belgium")
pos_min <- function(x) {
  min(x[x>0])
}
Progress_IncreaseDeaths <- master_data %>% 
  filter(Country_Region %in% CountryList & is.na(Province_State)) %>%
  filter(Date > ymd("20200401")) %>%
  group_by(Country_Region,Population) %>%
  summarise_at(c("Increase_Deaths","Increase_Weighted_Deaths"),list(pos_min, max))%>%
  rename(min_Increase_Deaths = Increase_Deaths_fn1, max_Increase_Deaths = Increase_Deaths_fn2) %>%
  rename(min_Increase_Weighted_Deaths = Increase_Weighted_Deaths_fn1, max_Increase_Weighted_Deaths = Increase_Weighted_Deaths_fn2)

master_data %>%
  JHplot_CountryLevel(CountryList,"Increase_Weighted_Deaths")

master_data %>%
  filter(Date>ymd("20200601")) %>%
  JHplot_CountryLevel(CountryList,"Increase_Weighted_Deaths")

master_data %>%
  JHplot_CountryLevel(CountryList,"Increase_Deaths")

master_data %>%
  filter(Date>ymd("20200601")) %>%
  JHplot_CountryLevel(CountryList,"Increase_Deaths")

JHDateYWasUKNow <- function(CountryName) {
  JHDateYWasXWhen(
  master_data,
  "Increase_Weighted_Deaths",
  "United Kingdom",
  CountryName,
  getLatestAvailableDate(master_data,"United Kingdom"),
  ymd("20200501")) }


DateCountriesWereUkNow <- master_data %>% 
  filter(Country_Region %in% CountryList & is.na(Province_State)) %>%
  filter(Date > ymd("20200401")) %>%
  group_by(Country_Region) %>%
  summarise(JHDateYWasUKNow(Country_Region))
colnames(DateCountriesWereUkNow) = c("Country","Date UK Now Achieved")


