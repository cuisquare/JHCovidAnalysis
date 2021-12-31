
#' looking into the vaccine data from JH using a direct file download from github approach
#' because the size of the repo (10Gb) is prohibitive compared to the size of the data (4
#' 
#' https://github.com/               govex/COVID-19/blob/4b07952e5abcaa37cecc7a192db7b42f7cf66696/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv
#' https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv
#' https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv
#' https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/vaccine_data_global.csv
#' 

time_series_covid19_vaccine_doses_admin_global  <- read_csv(file = "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
names(time_series_covid19_vaccine_doses_admin_global)
unique(time_series_covid19_vaccine_doses_admin_global$Country_Region)
#change into tidy data
last_day_col <- ncol(time_series_covid19_vaccine_doses_admin_global)
master_data_vaccine_doses <- time_series_covid19_vaccine_doses_admin_global %>% 
  gather(key="Date",value="Vaccine_Doses",13:last_day_col)  #this gives total vaccine doses regardless of who got them
View(master_data_vaccine_doses)


time_series_covid19_vaccine_global <- read_csv(file = "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv")
View(time_series_covid19_vaccine_global) #i think that's the one i want

#this is just the last value
vaccine_data_global <- read_csv(file = "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/vaccine_data_global.csv")
