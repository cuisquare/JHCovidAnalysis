library(plotly)
library(ggrepel)
library(directlabels)
library(zoo)

getLatestAvailableDate <- function(JH_Data,CountryName) {
  JH_Data %>% 
    filter(Country_Region == CountryName) %>%
    pull(Date) %>%  
    max()
}

getLatestDataSimple <- function(JH_Data,CountryName) {
  LatestAvailableDate <- getLatestAvailableDate(JH_Data,CountryName)
    
  JH_Data %>% 
    filter(Country_Region == CountryName) %>%
    filter(Date == LatestAvailableDate)
}

getLatestData <- function(JH_Data,CountryList,spanDays=FALSE) {
  
  LatestAvailableDate <- as_date(min(sapply(CountryList,
                                    FUN=function(CN) {
                                      as_date(getLatestAvailableDate(master_data,CN))})))
  
  print(paste("spanDays =",spanDays))
  
  if (spanDays==FALSE | !(is.numeric(spanDays))) {
    StartDate <- LatestAvailableDate
    print("spanDays not assigned or not numeric")
  } else {
    StartDate <- LatestAvailableDate - days(spanDays-1)
  }
  print(paste("StartDate = ",StartDate))
  
  JH_Data %>% 
    filter(Country_Region %in% CountryList) %>%
    filter(Date <= LatestAvailableDate & Date >= StartDate) 
}

JHplot_ProvinceLevel <- function(JH_Data,CountryList,VarName,simple=TRUE) {
  theplot <- JH_Data %>% 
    filter(Date>ymd("20200315")) %>%
    filter(Country_Region %in% CountryList) %>%
    ggplot(aes_string("Date",VarName,color="Province_State")) +
    geom_point() + geom_line()
  if (simple) {
    print(theplot)
  } else {
    print(ggplotly(theplot))    
  }
  #return(p)
}

JHGetdata_CountryLevel <- function(JH_Data,CountryList,VarName) {
  #'TODO Bug ? this does not give right answer for countries including France
  #'possibly because of the regional aspect for that country
  #'810 milions for actual value of 67 gives ratio of 12 which is the 
  #'number of france regions. this might be due to how the population is
  #'joined to the original data, it might get added even for rows for which 
  #'the province is not NA.
  #'initial attempt is to remove regional information unfortunately this 
  #'does not work for countries ike australia for which there is only regional
  #'data available. therefore this needs to be refined
  
  thedata <- JH_Data %>% 
    filter(Date>ymd("20200315")) %>%
    filter(Country_Region %in% CountryList) 
  
  thedata <- thedata %>%
    filter(is.na(Province_State)) #added 20211228 to deal with pop count bug
  
  # thedata <- thedata  %>% 
  #   mutate(isRegionalRow = !is.na(Province_State))  %>%
  #   group_by(Country_Region) %>%
  #   mutate(isRegionalOnly = (sum(isRegionalRow) == n())) %>%
  #   ungroup()
  
  # thedata_notregionalonly <- thedata %>%
  #   filter(!isRegionalOnly) %>%
  #   filter(!is.na(Province_State))
  # 
  # thedata_regionalonly <- thedata %>%
  #   filter(isRegionalOnly) 
  # 
  # thedata <- thedata_notregionalonly %>%
  #   bind_rows(thedata_regionalonly)
  
  thedata <- thedata  %>% 
    filter(!is.na(!!as.name(VarName))) %>% 
    group_by(Country_Region,Date) %>%
    summarise(!!VarName := sum(!!as.name(VarName))) %>%
    ungroup()
  
  thedata <- thedata %>%
    mutate(Country_Region= fct_reorder(Country_Region,!!as.name(VarName),function(x) {-tail(x,1)}))
  
}

JHGetplot_CountryLevel <- function(JH_Data,CountryList,VarName,add_label = FALSE,adjust_label = FALSE, mindiffval = 5) {
  VarNameString <- VarName
  VarName <- sym(VarName)
  
  thedata <- JH_Data %>%
    JHGetdata_CountryLevel(CountryList,VarName)
  
  adjust_vals <- function(vals,mindiffval) {
    #assumes vals sorted in ascending order
    #but beware this might have been changed by a previous adjustment
    for (valindex in 1:(length(vals)-1)) {
      if (vals[valindex+1]- vals[valindex] < mindiffval) {
        vals[valindex+1] <- vals[valindex] + mindiffval
      }
    }
    return(vals)
  }
  
  thelabeldata <- thedata %>% 
    filter(Date == max(Date)) %>%
    arrange(!!as.name(VarName))
  
  if (adjust_label) {
    thelabeldata$adjustedvarname <- adjust_vals(thelabeldata[[VarNameString]],mindiffval)
  } else {
    thelabeldata <- thelabeldata %>%
      mutate(adjustedvarname = !!VarName)
  }
  
  
  
  #TODO order the countries by order of value at the last available date
  
  #TODO do the nudging at the labeldata df level but that would require being able to access VarName data
  #on the fly and not being able to so far is the reason we are currently using aes_string
  
  
  theplot <- ggplot(data =thedata, 
                    mapping = aes(Date,!!VarName,color=Country_Region)
  ) +
    geom_line() 
  
  if (add_label) {
    theplot <- theplot + 
      geom_text( data=thelabeldata, 
                 mapping = aes(x = Date,
                               y = adjustedvarname, #!!VarName
                               label = paste0(Country_Region," (",round(!!as.name(VarName),2),")"),
                               colour = Country_Region
                 ),
                 check_overlap = TRUE,
                 nudge_x = 20#,
                 # nudge_y = rnorm(n=rep(1,length(thelabeldata[[VarNameString]])),
                 #                 mean = 0,
                 #                 sd = 0.2*thelabeldata[[VarNameString]]
                 #                 )
                 
      )    
  }
  return(theplot)
}

JHplot_CountryLevel <- function(JH_Data,CountryList,VarName,simple=FALSE,add_label = FALSE,adjust_label = FALSE, mindiffval = 5) {
  
 theplot <- JHGetplot_CountryLevel(JH_Data,CountryList,VarName,add_label,adjust_label,mindiffval)

  if (simple) {
    print(theplot)
  } else {
    ggplotly(theplot,dynamicTicks = TRUE) #%>% layout(xaxis = list(rangeslider = list(type = "date")))
  }
  #return(theplot)
}

Top_N_CountryLevel <- function(JH_Data,VarName,ntop,datetop) {
  thedata <- JH_Data %>% 
    filter(Date==datetop) %>%
    filter(!is.na(!!as.name(VarName))) %>%
    group_by(Country_Region) 
  
  thedata <-thedata %>%
    summarise(!!VarName := sum(!!as.name(VarName))) 
  
  thedata <-thedata%>%
    ungroup() %>%
    slice_max(order_by = !!as.name(VarName),n=ntop)
  
  return(thedata)
}

Bottom_N_CountryLevel <- function(JH_Data,VarName,nbottom,datebottom) {
  thedata <- JH_Data %>% 
    filter(Date==datebottom) %>%
    filter(!is.na(!!as.name(VarName))) %>%
    group_by(Country_Region) %>%
    summarise(!!VarName := sum(!!as.name(VarName))) %>%
    ungroup() %>%
    slice_min(order_by = !!as.name(VarName),n=nbottom)
  return(thedata)
}

JHGetProgress <- function(JH_Data,CountryList,VarName) {
  CountryList <- c("United Kingdom","France","Italy","Germany","Belgium","Greece")
  
  pos_min <- function(x) {
    min(x[x>0])
  }
  Progress <- master_data %>% 
    filter(Country_Region %in% CountryList & is.na(Province_State)) %>%
    filter(Date > ymd("20200401")) %>%
    group_by(Country_Region,Population) %>%
    summarise_at(VarName,list(pos_min, max)) 
  
  Progress <- Progress %>%
    rename(!!as.name(paste("min_",VarName,sep="")) := !!as.name("fn1"),
           !!as.name(paste("max_",VarName,sep="")) := !!as.name("fn2"))
  
  LatestData <- master_data %>%
    filter(is.na(Province_State)) %>%
    getLatestData(CountryList) %>%
    select(Country_Region,!!as.name(VarName)) 
  
  LatestData <- LatestData %>%
    rename(!!as.name(paste("current_",VarName,sep="")) := !!as.name(VarName))
  
  Progress <- Progress %>%
    left_join(LatestData,by="Country_Region")
  
  return (Progress)
}

JHDateYWasXWhen <- function(JH_Data,VarName,CountryX,CountryY,DateX,DateLookUpFrom) {
  XVarNameVal <- JH_Data %>% 
    filter(Country_Region == CountryX) %>%
    filter(Date == DateX) %>% 
    filter(is.na(Province_State)) %>% 
    pull(VarName)
  
  DateYWasXWhen <- JH_Data %>% 
    filter(Country_Region == CountryY & is.na(Province_State) & Date > DateLookUpFrom) %>%
    filter(!!as.name(VarName) < XVarNameVal) %>%
    slice_min(Date,1) %>%
    pull(Date)
  
  #at which date did France get as good as UK is now ?
  #TODO: find peak and search from there, or define up and downs search, or get all candidates
  #at moment only looking for the downward match from peak
}


JHDateXWillBeYWhen <- function(JH_Data,VarName,CountryX,CountryY,DateX,DateLookUpFrom) {
  #take a few values of countryX varname vals with corresponding dateX
  #get corresponding dateYs that countryY had same vals
  #get average time span dateX-DateY
  #TODO write
  
}

checkDiscJHvsUKGOV <- function(master_data) {
  
  #note that master_data must be obtained using the script JHGetData.R 
  
  library(tidyverse)
  options(digits = 3)   # report 3 significant digits
  
  #DONE investigate difference between JH and data.gov.uk website june 111 data.gov 500 ish JH
  #https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv uk version
  #DONE raise issue on github on discrepancy: https://github.com/CSSEGISandData/COVID-19/issues/2861
  #answer received that discrepancy was before of UK gov modif of historical data which was not updated on JH side
  
  UK_Deaths_JH <- master_data %>% 
    filter(Country_Region == "United Kingdom" & is.na(Province_State)) %>%
    mutate(Inc_Deaths = Deaths - lag(Deaths,1)) %>%
    select(Date,Deaths,Inc_Deaths) %>%
    rename(Deaths_JH = Deaths, Inc_Deaths_JH = Inc_Deaths)
  
  UK_Deaths_GOV <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv")
  colnames(UK_Deaths_GOV) = c("Area","Drop1","Drop2","Date","Inc_Deaths_GOV","Deaths_GOV")
  UK_Deaths_GOV <- UK_Deaths_GOV %>% 
    select(-Drop1,-Drop2) %>%
    filter(Area == "United Kingdom") %>%
    select(-Area) %>%
    arrange(Date)
  UK_Deaths_GOV <- UK_Deaths_GOV[,c(1,3,2)]
  
  #uncomment for testing that discrepancies are picked up and 
  #output correctly created when none exist
  #UK_Deaths_GOV <- UK_Deaths_GOV %>% mutate(Deaths_GOV = 2*Deaths_GOV)
  
  UK_Deaths_JHvsGOVComp <- left_join(UK_Deaths_GOV,UK_Deaths_JH) 
  UK_Deaths_JHvsGOVComp <- UK_Deaths_JHvsGOVComp %>%
    mutate(Diff_Deaths = Deaths_JH-Deaths_GOV) %>%
    mutate(Diff_Inc_Deaths = Inc_Deaths_JH-Inc_Deaths_GOV) 
  UK_Deaths_JHvsGOVComp <- UK_Deaths_JHvsGOVComp[,c(1,2,4,6,3,5,7)]
  
  
  
  Disc_TotalDeaths <- UK_Deaths_JHvsGOVComp %>%
    filter(Diff_Deaths != 0)
  if (nrow(Disc_TotalDeaths) > 0) {
    #TODO print out what the discrepancy was
    print(paste("WARNING: Discrepancy found in UK Total Deaths Data (JH vs UK Gov)",
                "on the following date:",pull(Disc_TotalDeaths,Date),sep=" "))
    #saving comparison as a file
    date_now <- now()
    date_now_str <- paste(year(date_now),str_pad(month(date_now), 2, pad = "0"),str_pad(day(date_now), 2, pad = "0"),sep="_")
    filename <- paste("UK","Deaths","JHvsGOVComp",date_now_str,"DiscFound.csv",sep="_")
    UK_Deaths_JHvsGOVComp %>% write_csv(filename)
    print(paste("See output file",filename,"for more details",sep=" "))
  } else {
    print("No discrepancy found in UK Total Deaths Data (JH vs UK Gov)")
  }
  #Note: the data from JH is exactly the same on WHO website
  
  output <- UK_Deaths_JHvsGOVComp
  
}

addMissingCountryWideData <- function(JH_Data) {
  regional_only <- JH_Data %>% 
    select(Country_Region,Province_State) %>%
    unique() %>%
    mutate(isRegional = !is.na(Province_State)) %>%
    group_by(Country_Region,isRegional) %>%
    summarise(count = n()) %>%
    group_by(Country_Region) %>%
    mutate(hasRegional = (sum(isRegional)>0)) %>%
    mutate(isRegionalOnly =(hasRegional & (n()==1))) %>%
    filter(isRegionalOnly) %>%
    select(Country_Region) %>%
    unique()
  
  curr_names <- c("Province_State", "Country_Region", "Lat", "Long", 
                  "Deaths", "Confirmed", "Recovered", "Doses_admin", "People_partially_vaccinated", 
                  "People_fully_vaccinated", "Report_Date_String", "UID")
  
  JH_Data_countrylevel_for_regionalonly <- JH_Data %>%
    filter(Country_Region %in% unique(regional_only$Country_Region)) %>%
    group_by(across(-c("Province_State","Lat","Long", "Report_Date_String","UID", "Deaths", "Confirmed", "Recovered", "Doses_admin", "People_partially_vaccinated",
                       "People_fully_vaccinated"))) %>%
    #group_by(c("Province_State", "Date")) %>%
    summarise_at(.vars = c("Deaths", "Confirmed", "Recovered", "Doses_admin", "People_partially_vaccinated","People_fully_vaccinated"),
    .funs = function(x) {sum(x,na.rm = TRUE)}
    ) 
  
  JH_Data <- JH_Data %>%
    bind_rows(JH_Data_countrylevel_for_regionalonly)
  
  return(JH_Data)
}