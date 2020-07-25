library(tidyverse)
library(lubridate)
options(digits = 3)   # report 3 significant digits

system("UpdateData")
data_folder <- "./Data/JH_Data_GitHubClone/csse_covid_19_data/csse_covid_19_time_series/"

##Deaths
data_file_deaths <- "time_series_covid19_deaths_global.csv"
master_data_raw_deaths <- read_csv(paste(data_folder,data_file_deaths,sep=""))

#change into tidy data
last_day_col <- ncol(master_data_raw_deaths)
master_data_deaths <- master_data_raw_deaths %>% 
  gather(key="Date",value="Deaths",5:last_day_col) 

#reformat dates and long char column names
master_data_deaths <- master_data_deaths  %>%
  mutate(Date = mdy(Date))  %>% 
  rename(Country_Region = `Country/Region`, Province_State = `Province/State`)

##Confirmed Cases
data_file_confirmed <- "time_series_covid19_confirmed_global.csv"
master_data_raw_confirmed <- read_csv(paste(data_folder,data_file_confirmed,sep=""))

#change into tidy data
last_day_col <- ncol(master_data_raw_confirmed)
master_data_confirmed <- master_data_raw_confirmed %>% 
  gather(key="Date",value="Confirmed",5:last_day_col) 

#reformat dates and long char column names
master_data_confirmed <- master_data_confirmed %>%
  mutate(Date = mdy(Date))  %>% 
  rename(Country_Region = `Country/Region`, Province_State = `Province/State`)

##Recovered Cases
data_file_recovered <- "time_series_covid19_recovered_global.csv"
master_data_raw_recovered <- read_csv(paste(data_folder,data_file_recovered,sep=""))

#change into tidy data
last_day_col <- ncol(master_data_raw_recovered)
master_data_recovered <- master_data_raw_recovered %>% 
  gather(key="Date",value="Recovered",5:last_day_col) 

#reformat dates and long char column names
master_data_recovered <- master_data_recovered %>%
  mutate(Date = mdy(Date))  %>% 
  rename(Country_Region = `Country/Region`, Province_State = `Province/State`)

#Merging data
master_data <- master_data_deaths
master_data <- left_join(master_data,master_data_confirmed)
master_data <- left_join(master_data,master_data_recovered)

#DONE: get demographic information so results can be normalised
path <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
h <- xml2::read_html(path)
tab <- h %>% rvest::html_nodes("table")
countries_population <- tab[[1]] %>% 
  rvest::html_table(fill=TRUE) 
countries_population <- countries_population %>%
  rename(Country_Region = "Country (or dependent territory)") %>%
  select(Country_Region,Population) %>%
  mutate(Population = as.numeric(str_replace_all(Population,",",""))) %>%
  mutate(Country_Region = str_replace_all(Country_Region,"\\[.+\\]",""))

#checking data not found
JHCountries_NotFound_Before <- master_data %>%
  select(Country_Region) %>%
  unique() %>%
  filter(!(Country_Region %in% countries_population$Country_Region))

#correcting the names for not found data
master_data <- master_data %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Taiwan\\*", "Taiwan"))
master_data <- master_data %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Holy See", "Vatican City"))
master_data <- master_data %>%
  mutate(Country_Region = str_replace_all(Country_Region,"^US$", "United States"))
master_data <- master_data %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Czechia", "Czech Republic"))
master_data <- master_data %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Korea, South", "South Korea"))
countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Ivory Coast", "Cote d'Ivoire"))
countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Cape Verde", "Cabo Verde"))

#replacement of São Tomé and Príncipe with check before and after change:
#Sao_Name_Before <- countries_population %>% filter(Population > 205000 & Population < 220000) %>% pull(Country_Region)
#print(paste("***CHECK*** Before replacement, Sao name in coutries_population df (found based on population) is: ",Sao_Name_Before,sep=""))
countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"São Tomé and Príncipe", "Sao Tome and Principe")) #São Tomé and Príncipe
#Sao_Name_After <- countries_population %>% filter(Population > 205000 & Population < 220000) %>% pull(Country_Region)
#print(paste("***CHECK*** After replacement, Sao name in coutries_population df (found based on population) is: ",Sao_Name_After,sep=""))

countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"DR Congo", "Congo (Kinshasa)"))
countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Congo$", "Congo (Brazzaville)"))
countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Myanmar", "Burma"))
countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"East Timor", "Timor-Leste"))
countries_population <- countries_population %>%
  add_row(Country_Region = "West Bank and Gaza", Population = 4543126)

#cote d'ivoire not found was ivory coast on wiki DONE
#cabo verde not found was cape verde on wiki DONE
#Taiwan* not found was Taiwan on wiki DONE
#Holy See not found was Vatican City on wiki DONE
#US not found was United States on wiki DONE
#Czechia not found was Czech Republic DONE
#Korea, South not found was South Korea DONE
#Sao Tome and Principe not found was São Tomé and Príncipe DONE
#Congo (Brazzaville) not found was Republic of the Congo # https://en.wikipedia.org/wiki/Republic_of_the_Congo DONE
#Congo (Kinshasa) not found was #https://en.wikipedia.org/wiki/Democratic_Republic_of_the_Congo DONE
#Burma not found  was Myanmar on wiki DONE
#Timor-Leste not found was East Timor on wiki DONE
#West Bank and Gaza not found DONE STATIC
# https://en.wikipedia.org/wiki/Palestinian_territories 
#https://en.wikipedia.org/wiki/Demographics_of_the_Palestinian_territories
#MS Zaandam not found is a cruise ship - will be dropped by inner_join
#Diamond Princess not found is a cruise ship - will be dropped by inner_join

#DONE: include check of how many countries are not found after correction (Sanity check)
#DONE: bug - the São Tomé and Príncipe entry does not seem to get correctly corrected when the file is sourced
#compared to run separately - this was because the encoding of sourced file in the call was not specified so 
#the special characters were not recognised.

#checking data not found
JHCountries_NotFound_After <- master_data %>%
  select(Country_Region) %>%
  unique() %>%
  filter(!(Country_Region %in% countries_population$Country_Region))

Nb_JHCountries_NotFound_After <- nrow(JHCountries_NotFound_After) #expected: 2

print(paste("After population data extraction, ",
            Nb_JHCountries_NotFound_After,
            " countries in master_data were not found ",
            "in countries_population (and will be dropped):",sep=""))
print(JHCountries_NotFound_After$Country_Region)

#final join, will leave out anything not found
master_data <- master_data %>%
  inner_join(countries_population)



Nb_JHCountriesTotal_After <- master_data %>%  select(Country_Region) %>% unique() %>% nrow() #186

#TODOMAYBE ?get countries area data?
#https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area


#added data
lagvaluedays <- 7
master_data <- master_data %>%
  group_by(Country_Region,Province_State) %>%
  mutate(Increase_Deaths = (Deaths - lag(Deaths,lagvaluedays))/lagvaluedays) %>%
  mutate(Increase_Confirmed = (Confirmed - lag(Confirmed,lagvaluedays))/lagvaluedays) %>%
  mutate(Weighted_Deaths = Deaths/Population) %>%
  mutate(Weighted_Confirmed = Confirmed/Population) %>%
  mutate(Increase_Weighted_Deaths = (Weighted_Deaths - lag(Weighted_Deaths,lagvaluedays))/lagvaluedays) %>%
  mutate(Increase_Weighted_Confirmed = (Weighted_Confirmed - lag(Weighted_Confirmed,lagvaluedays))/lagvaluedays) %>%
  ungroup()

#new data not yet confirmed if useful
master_data <- master_data %>%
  group_by(Country_Region, Province_State) %>%
  mutate(Increase_Increase_Confirmed = (Increase_Weighted_Confirmed - lag(Increase_Weighted_Confirmed,lagvaluedays))/lagvaluedays) %>%
  mutate(Increase_Increase_Deaths = (Increase_Weighted_Deaths - lag(Increase_Weighted_Deaths,lagvaluedays))/lagvaluedays) %>%
  mutate(Rate_Increase_Deaths = Increase_Increase_Deaths/Increase_Deaths) %>%
  mutate(Rate_Increase_Confirmed = Increase_Increase_Confirmed/Increase_Confirmed) %>%
  ungroup()