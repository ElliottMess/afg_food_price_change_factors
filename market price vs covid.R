########################
#                      #
# Author: Amin Sherzad #
#                      #
#######################

### What am I doing? finding the relationship between food item price change and 1) COVID cases, 2) Country collapse (in 2021)
#Note: The analysis shows findings only for Afghanistan
#Disclaimer: I don't take any responsibility of any decision or publication based on this analysis. However, the datasets are publicly published but analysis is not validated, just tested by myself

#Include libraries
library(tidyverse)
library(readxl)
library(janitor) #for cleaning the datasets header


##Import data
#important! I've slightly renamed the files after downloaded from HDX, in JMMI, just renamed the month from name to number, e.g: January as 01, April as 04, this is just to make my life easier when importing it.
#E: you could use the  rhdx package to dowload the dataset from the API directly. It shows capacity to deal with APIs, which is saught after. 
#link on HDX: https://data.humdata.org/dataset/afghanistan-covid-19-statistics-per-province
covid_data <- read.csv("./input/market price data/covid data.csv", stringsAsFactors = F) %>% # E: use read_csv to avoid stringsAsFactors
  filter(Date != "#date") 

#JMMI (Joint Market Monitoring Initiative) data


#E: You have standardized files names, so you could use a purrr call to iterate over a list:
market_price_data_files <- list.files("input/market price data/")
price_per_item_files <- paste0("./input/market price data/",market_price_data_files[grep("copy-of-afg.*$", market_price_data_files)])
price_per_item_dfs <- purrr::map_dfr(price_per_item_files, ~{
  date <- str_replace(str_extract(.x, "[0-9]{2}_2021", "_", "-")
  date
  read_excel(.x, sheet = "Price per item - district") %>% 
    select(c(afg_dist:tomatoes_price_cr_median)) %>% 
    mutate(date = paste0(date, "-01")) %>% 
    select(c(afg_dist:tomatoes_price_cr_median))
})

#link on HDX: https://data.humdata.org/dataset/joint-market-monitoring-initiative-jmmi-r
jmmi_01 <- read_excel("./input/market price data/copy-of-afg_reach_cvwg_jmmi_01_2021.xlsx", sheet = "Price per item - district") %>% 
  select(c(afg_dist:tomatoes_price_cr_median))
jmmi_02 <- read_excel("./input/market price data/copy-of-afg_reach_cvwg_jmmi_02_2021.xlsx", sheet = "Price per item - district") %>% 
  select(c(afg_dist:tomatoes_price_cr_median))
jmmi_04 <- read_excel("./input/market price data/copy-of-reach_afg_cvwg_jmmi_dataset_04_2021.xlsx", sheet = "Price per item in AFN") %>% 
  clean_names() %>% type.convert(as.is = T)
jmmi_05 <- read_excel("./input/market price data/copy-of-reach_afg_cvwg_jmmi_dataset_05_2021.xlsx", sheet = "Price per item in AFN") %>% 
  clean_names() %>% type.convert(as.is = T)
jmmi_06 <- read_excel("./input/market price data/copy-of-reach_afg_cvwg_jmmi_dataset_06_2021.xlsx", sheet = "Price per item in AFN") %>% 
  clean_names() %>% type.convert(as.is = T)
jmmi_07 <- read_excel("./input/market price data/copy-of-reach_afg_cvwg_jmmi_dataset_07_2021.xlsx", sheet = "Price per item in AFN") %>% 
  clean_names() %>% type.convert(as.is = T)
jmmi_08 <- read_excel("./input/market price data/copy-of-reach_afg_cvwg_jmmi_dataset_08_2021.xlsx", sheet = "Price per item in AFN") %>% 
  clean_names() %>% type.convert(as.is = T)
jmmi_09 <- read_excel("./input/market price data/copy-of-reach_afg_cvwg_jmmi_dataset_09_2021.xlsx", sheet = "Price per item in AFN") %>% 
  clean_names() %>% type.convert(as.is = T)
jmmi_10 <- read_excel("./input/market price data/copy-of-reach_afg_cvwg_jmmi_dataset_10_2021.xlsx", sheet = "Price per item in AFN") %>% 
  clean_names() %>% type.convert(as.is = T)
jmmi_11 <- read_excel("./input/market price data/copy-of-reach_afg_cvwg_jmmi_dataset_11_2021.xlsx", sheet = "Price per item in AFN") %>% 
  clean_names() %>% type.convert(as.is = T)
jmmi_12 <- read_excel("./input/market price data/copy-of-reach_afg_cvwg_jmmi_dataset_12_2021.xlsx", sheet = "Price per item in AFN") %>% 
  clean_names() %>% type.convert(as.is = T)
  
#district list
dist_list <- read_excel("./input/market price data/copy-of-afg_reach_cvwg_jmmi_01_2021.xlsx", sheet = "Choices") %>% 
  filter(list_name == "afg_dist") %>% 
  select(name, district = `label::English`)

#add date info to jmmi data
jmmi_01$date <- "2021-01-01"
jmmi_02$date <- "2021-02-01"
jmmi_04$date <- "2021-04-01"
jmmi_05$date <- "2021-05-01"
jmmi_06$date <- "2021-06-01"
jmmi_07$date <- "2021-07-01"
jmmi_08$date <- "2021-08-01"
jmmi_09$date <- "2021-09-01"
jmmi_10$date <- "2021-10-01"
jmmi_11$date <- "2021-11-01"
jmmi_12$date <- "2021-12-01"

##Transform and clean data
#clean column names
covid_data <- covid_data %>% clean_names()
covid_data <- covid_data %>% select(-c(recoveries, active_cases, tests))
covid_data_cln <- covid_data %>% mutate_all(
  ., funs(str_replace(., ",", ""))
)

#jmmi
jmmi_1_2 <- bind_rows(jmmi_01, jmmi_02) %>%  select(afg_dist, date, wheat_local = wheat_local_price_cr_median,
                              wheat_imported = wheat_imported_price_cr_median,
                              rice_local = local_rice_price_cr_median,
                              veg_oil = veg_oil_price_cr_median,
                              lentils = pulses_lentils_price_cr_median,
                              beans = pulses_beans_price_cr_median,
                              split_peas = pulses_split_peas_price_cr_median,
                              salt = salt_price_cr_median,
                              sugar = sugar_price_cr_median,
                              tomatoes = tomatoes_price_cr_median
                              )

jmmi_1_2 <- jmmi_1_2 %>% left_join(dist_list, by = c("afg_dist" = "name")) %>% 
  select(district, everything(), -c(afg_dist))

jmmi_1_2[jmmi_1_2 == "N/A"] <- NA
jmmi_1_2 <- jmmi_1_2 %>% type.convert(as.is = T)

#merging all together
jmmi_4_12 <- bind_rows(jmmi_04, jmmi_05, jmmi_06, jmmi_07, jmmi_08, jmmi_09, jmmi_10, jmmi_11, jmmi_12) %>% 
  select(district = district_name, date, wheat_local:tomatoes, -pulses_median)
  
jmmi_all <- bind_rows(jmmi_1_2, jmmi_4_12)

#covert types
covid_data_cln$cases <- as.numeric(covid_data_cln$cases)
covid_data_cln$deaths <- as.numeric(covid_data_cln$deaths)
covid_data_cln$date <- as.Date(covid_data_cln$date)

#subset by date (we only need 2021)
covid_data_cln <- covid_data_cln %>% filter(date >= "2021-01-01" & date <= "2021-12-31")

##Manipulation
#adding month variable into both datasets
covid_data_cln$month <- months(covid_data_cln$date)
jmmi_all$month <- months(as.Date(jmmi_all$date))


##Integrate data
jmmi_covid <- covid_data_cln %>% left_join(jmmi_all, by = "month")

##Explore data
#structure
glimpse(covid_data_cln)

#Outlier check
boxplot(covid_data_cln$cases)
boxplot(covid_data_cln$deaths)

#NA check
covid_data_NAs <- covid_data_cln %>% summarise(
  province_na = sum(is.na(province)),
  cases_na = sum(is.na(cases)),
  deaths_na = sum(is.na(deaths)),
  date_na = sum(is.na(date)),
)

jmmi_NAs <- jmmi_all %>% summarise(
  wheat_local_na = sum(is.na(wheat_local)),
  wheat_imported_na = sum(is.na(wheat_imported)),
  rice_local_na = sum(is.na(rice_local)),
  veg_oil_na = sum(is.na(veg_oil)),
  lentils_na = sum(is.na(lentils)),
  beans_na = sum(is.na(beans)),
  split_peas_na = sum(is.na(split_peas)),
  salt_na = sum(is.na(salt)),
  sugar_na = sum(is.na(sugar)),
  tomatoes_na = sum(is.na(tomatoes)),
)

##Descriptive data analysis
summary(covid_data_cln)
summary(jmmi_all)

covid_smry <- covid_data_cln %>% group_by(month) %>% 
  summarise(
    cases_avg = mean(cases, na.rm = T)
  )

jmmi_smry <- jmmi_all %>% group_by(month) %>% 
  summarise(
    wheat_local_avg = mean(wheat_local, na.rm = T),
    wheat_imported_avg = mean(wheat_imported, na.rm = T),
    rice_local_avg = mean(rice_local, na.rm = T),
    veg_oil_avg = mean(veg_oil, na.rm = T),
    lentils_avg = mean(lentils, na.rm = T),
    beans_avg = mean(beans, na.rm = T),
    split_peas_avg = mean(split_peas, na.rm = T),
    sugar_avg = mean(sugar, na.rm = T),
    tomatoes_avg = mean(tomatoes, na.rm = T),
    #calculating average of food items
    food_items_avg = (wheat_local_avg+wheat_imported_avg+rice_local_avg+veg_oil_avg+lentils_avg+
                        beans_avg+split_peas_avg+sugar_avg+tomatoes_avg)/9
  )

#categorizing by before and after collapse of country
price_vs_collapse <- jmmi_smry %>% mutate(
  collapse = case_when(
    month %in% c("January", "February", "March","April", "May", "June", "July") ~ "before",
    month %in% c("August", "September", "October", "November", "December") ~ "after",
    TRUE ~ month
  )
)

#Merging summaries, and taking out the March data, because there is no JMMI data available for this month
price_vs_covid <- covid_smry %>% left_join(jmmi_smry, by = "month") %>% filter(month != "March")


##Visualization; explore if there is any trend
covid_data_cln %>% ggplot(aes(x = month, y = cases))+
  geom_bar(stat = 'identity', position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))+
  labs(title = "COVID-19 Cases change by month")

#of course there is direct relationship between these two indicators, just displaying it
covid_data_cln %>% ggplot(aes(x = cases, y = deaths))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  labs(title = "Relationship between death and cases")













