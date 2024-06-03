
library(tidyverse)
library(cowplot)
library(brms)
library(tidybayes)
library(lubridate)
library(readr)
library(readxl)
library(zoo)

phyto <- read_excel("Combined_Phytoplankton_San_Fran_Bay_2016-2021.xlsx")
forModelling2016 <- read_csv("SF/253279_41.53_-115.50_2016.csv")
forModelling2017 <- read_csv("SF/253279_41.53_-115.50_2017.csv")
forModelling2018 <- read_csv("SF/253279_41.53_-115.50_2018.csv")
forModelling2019 <- read_csv("SF/253279_41.53_-115.50_2019.csv")
forModelling2020 <- read_csv("SF/253279_41.53_-115.50_2020.csv")
forModelling2021 <- read_csv("SF/253279_41.53_-115.50_2021.csv")
rainfall <-  read_excel("SF/Rainfall_San Fran 2016-2023.xlsx")

phyto <- phyto %>%
  filter(!is.na(`Total Phyto`)) 

phyto <- phyto %>%
  select(`Date...2`, `Total Phyto`) %>%
  rename(Date = `Date...2`,
         total_phytoplankton = `Total Phyto`)

solar <- forModelling2016 %>%
  bind_rows(forModelling2017) %>% 
  bind_rows(forModelling2018) %>%
  bind_rows(forModelling2019) %>%
  bind_rows(forModelling2020) %>%
  bind_rows(forModelling2021) 

solar <- solar[-c(1:2),]

solar <- solar %>%
  select(Latitude, Source, `Location ID`, City)

solar$Date <- ymd(paste0(solar$Source, "-", solar$`Location ID`, "-", solar$City), format = "%Y-%m-%d")[1:52570]

colnames(rainfall) <- c("Day",
                        "Jan","Jan-Year",
                        "Feb", "Feb-Year",
                        "March", "March-Year",
                        "April", "April-Year",
                        "May", "May-Year",
                        "June", "June-Year",
                        "July", "July-Year",
                        "Aug", "Aug-Year",
                        "Sept", "Sept-Year",
                        "Oct", "Oct-Year",
                        "Nov", "Nov-Year",
                        "Dec", "Dec-Year"
)

rainfall <- rainfall[-1,]

rainfall <- rainfall %>%
  mutate(
    `Feb-Year` = as.numeric(`Feb-Year`),
    `April-Year` = as.numeric(`April-Year`),
    `June-Year` = as.numeric(`June-Year`),
    `Sept-Year` = as.numeric(`Sept-Year`),
    `Nov-Year` = as.numeric(`Nov-Year`)
  ) %>%
  pivot_longer(c(-Day, -Jan, -Feb, -March, -April, -May, -June, -July, -Aug, -Sept, -Oct, -Nov, -Dec)) %>%
  select(-name) %>%
  rename(Year = value) %>%
  mutate(Day = as.numeric(Day)) %>%
  pivot_longer(c(-Day, -Year)) %>%
  rename(rainfall = value) %>%
  mutate(rainfall = as.numeric(rainfall)*2.5) %>%
  mutate(Date = ymd(paste0(Year, "-", name, "-", Day)))

solar <- solar %>%
  group_by(Date) %>%
  summarise(solar_exposure = mean(Latitude))

forModelling <- phyto %>%
  left_join(solar, by = "Date") %>% 
  left_join(rainfall, by = "Date")

forModelling <- forModelling %>%
  fill(rainfall, .direction = "downup")

forModelling$log10Phytoplankton <- log10(forModelling$total_phytoplankton)

forModelling <- forModelling %>%
  filter(!is.na(total_phytoplankton))

forModelling$overall_fire_impact <- factor(ifelse(forModelling$Date >= "2018-07-14" & forModelling$Date <= "2018-11-30", "high", "zero"), levels = c("high", "low", "med", "zero"))
forModelling$overall_fire_impact_2 <- factor(ifelse(forModelling$Date >= "2018-12-01" & forModelling$Date <= "2019-01-31", "high", "zero"), levels = c("high", "low", "med", "zero"))

forModelling$Location <- factor("San Francisco", levels = c("Camden Haven", "Georges River", "Hastings River",  
                                                            "Hawkesbury River", "Manning River", "Pambula", "Port Stephens", "Shoalhaven", 
                                                            "Wagonga", "Wallis Lake", "Wapengo", "Wonboyn", "San Francisco"))

forModelling$month_of_year <- factor(month(forModelling$Date), levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

forModelling$rainfall48 = rollmean(forModelling$rainfall, 2, fill = NA)
forModelling$rainfall72 = rollmean(forModelling$rainfall, 3, fill = NA)

forModelling$temp48 = rollmean(as.numeric(forModelling$Temperature), 2, fill = NA)
forModelling$temp72 = rollmean(as.numeric(forModelling$Temperature), 3, fill = NA)

forModelling$depth48 = rollmean(as.numeric(forModelling$Depth), 2, fill = NA)
forModelling$depth72 = rollmean(as.numeric(forModelling$Depth), 3, fill = NA)

forModelling$salinity48 = rollmean(as.numeric(forModelling$Salinity), 2, fill = NA)
forModelling$salinity72 = rollmean(as.numeric(forModelling$Salinity), 3, fill = NA)

forModelling$Calculated_Chlorophyll <- as.numeric(forModelling$Calculated_Chlorophyll)
