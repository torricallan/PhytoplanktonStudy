
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
  select(`Date...2`, `Total Phyto`, `Dinophysis spp.`, `Pseudo-nitzschia spp.`, `Alexandrium spp.`) %>%
  rename(Date = `Date...2`,
         total_phytoplankton = `Total Phyto`,
         dinophysis = `Dinophysis spp.`,
         pseudo = `Pseudo-nitzschia spp.`,
         alexandrium = `Alexandrium spp.`)

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

forModelling$Pseudo_Total_Scaled <- forModelling$pseudo/max(forModelling$pseudo, na.rm = TRUE)
forModelling$Alexandrium_Total_Scaled <- forModelling$alexandrium/max(forModelling$alexandrium, na.rm = TRUE)
forModelling$Dinophysis_Total_Scaled <- forModelling$dinophysis/max(forModelling$dinophysis, na.rm = TRUE)


fit1 <- brm(bf(Pseudo_Total_Scaled ~ 1 +
                 month_of_year +
                 s(rainfall72) +
                 s(solar_exposure) +
                 overall_fire_impact_2
),
family = lognormal(),
data = forModelling,
prior = c(prior('normal(0, 1)', class = Intercept),
          prior('normal(0, 1)', class = b),
          prior('normal(0, 1)', class = sds)
          #prior('normal(0, 1)', class = Intercept, dpar = "hu"),
          #prior('normal(0, 1)', class = b, dpar = "hu")
),
chains = 4, iter = 2000)

fit1

forModelling %>%
  filter(Date >= "2018-07-14") %>%
  filter(Date <= "2018-11-30") %>%
  bind_rows(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit1, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              select(overall_fire_impact, Pseudo_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(Date >= "2018-07-14") %>%
                          filter(Date <= "2018-11-30") %>%
                          select(overall_fire_impact, Pseudo_Total_Scaled) %>%
                          mutate(overall_fire_impact = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-overall_fire_impact, -Pseudo_Total_Scaled)) %>%
  mutate(overall_fire_impact = factor(overall_fire_impact, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-overall_fire_impact)) %>%
  mutate(type = relevel(factor(ifelse(name == "Pseudo_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact == 'high' ~ 'High',
    overall_fire_impact == 'med' ~ 'Medium',
    overall_fire_impact == 'low' ~ 'Low',
    overall_fire_impact == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = log(value*285.1943)) %>%
  ggplot() + 
  geom_density(aes(x = value, fill = type)) +
  xlab("Log Pseudo Counts") +
  facet_grid(rows = vars(`Bushfire Impact`), labeller = label_both) + 
  theme_cowplot() + 
  theme(strip.background = element_blank()) + 
  labs(fill='') 


### Calculate number of days HABs is above the action standards
### 50000 cells/L

forModelling %>%
  filter(Date >= "2018-07-14") %>%
  filter(Date <= "2018-11-30") %>%
  bind_rows(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit1, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              select(Date, overall_fire_impact, Pseudo_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(Date >= "2018-07-14") %>%
                          filter(Date <= "2018-11-30") %>%
                          select(overall_fire_impact, Pseudo_Total_Scaled) %>%
                          mutate(overall_fire_impact = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-Date, -overall_fire_impact, -Pseudo_Total_Scaled)) %>%
  mutate(overall_fire_impact = factor(overall_fire_impact, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-Date, -overall_fire_impact)) %>%
  mutate(type = relevel(factor(ifelse(name == "Pseudo_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact == 'high' ~ 'High',
    overall_fire_impact == 'med' ~ 'Medium',
    overall_fire_impact == 'low' ~ 'Low',
    overall_fire_impact == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = log(value*285.1943)) %>%
  filter(name == 'value') %>%
  mutate(above_detection_limit = ifelse(value >= 50000, 1, 0)) %>%
  group_by(Date, `Bushfire Impact`) %>%
  summarise(mean_above_detection_limit = mean(above_detection_limit)) %>% arrange(Date) %>% 
  ggplot(aes(x = `Bushfire Impact`, y = mean_above_detection_limit, col = factor(`Bushfire Impact`))) + 
  geom_point() + 
  ylab("Probability of observing counts above the detection limit") +
  theme_classic()

fit2 <- brm(bf(Dinophysis_Total_Scaled ~ 1 +
                 month_of_year +
                 s(rainfall72) +
                 s(solar_exposure) +
                 overall_fire_impact
),
family = lognormal(),
data = forModelling,
prior = c(prior('normal(0, 1)', class = Intercept),
          prior('normal(0, 1)', class = b),
          prior('normal(0, 1)', class = sds)
          #prior('normal(0, 1)', class = Intercept, dpar = "hu"),
          #prior('normal(0, 1)', class = b, dpar = "hu")
),
chains = 4, iter = 2000)

fit2

forModelling %>%
  filter(Date >= "2018-07-14") %>%
  filter(Date <= "2018-11-30") %>%
  bind_rows(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit2, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              select(overall_fire_impact, Alexandrium_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(Date >= "2018-07-14") %>%
                          filter(Date <= "2018-11-30") %>%
                          select(overall_fire_impact, Alexandrium_Total_Scaled) %>%
                          mutate(overall_fire_impact = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-overall_fire_impact, -Alexandrium_Total_Scaled)) %>%
  mutate(overall_fire_impact = factor(overall_fire_impact, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-overall_fire_impact)) %>%
  mutate(type = relevel(factor(ifelse(name == "Alexandrium_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact == 'high' ~ 'High',
    overall_fire_impact == 'med' ~ 'Medium',
    overall_fire_impact == 'low' ~ 'Low',
    overall_fire_impact == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = log(value)) %>%
  ggplot() + 
  geom_density(aes(x = value, fill = type)) +
  xlab("Log Alexandrium Counts") +
  facet_grid(rows = vars(`Bushfire Impact`), labeller = label_both) + 
  theme_cowplot() + 
  theme(strip.background = element_blank()) + 
  labs(fill='') 


### Calculate number of days HABs is above the action standards
### 200 cells/L

forModelling %>%
  filter(Date >= "2018-07-14") %>%
  filter(Date <= "2018-11-30") %>%
  bind_rows(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit2, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              select(Date, overall_fire_impact, Alexandrium_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(Date >= "2018-07-14") %>%
                          filter(Date <= "2018-11-30") %>%
                          select(overall_fire_impact, Alexandrium_Total_Scaled) %>%
                          mutate(overall_fire_impact = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-Date, -overall_fire_impact, -Alexandrium_Total_Scaled)) %>%
  mutate(overall_fire_impact = factor(overall_fire_impact, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-Date, -overall_fire_impact)) %>%
  mutate(type = relevel(factor(ifelse(name == "Alexandrium_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact == 'high' ~ 'High',
    overall_fire_impact == 'med' ~ 'Medium',
    overall_fire_impact == 'low' ~ 'Low',
    overall_fire_impact == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = log(value)) %>%
  filter(name == 'value') %>%
  mutate(above_detection_limit = ifelse(value >= 200, 1, 0)) %>%
  group_by(Date, `Bushfire Impact`) %>%
  summarise(mean_above_detection_limit = mean(above_detection_limit)) %>% arrange(Date) %>% 
  ggplot(aes(x = `Bushfire Impact`, y = mean_above_detection_limit, col = factor(`Bushfire Impact`))) + 
  geom_point() + 
  ylab("Probability of observing counts above the detection limit") +
  theme_classic()

fit3 <- brm(bf(Alexandrium_Total_Scaled ~ 1 +
                 month_of_year +
                 rainfall72 +
                 s(solar_exposure) +
                 overall_fire_impact
),
family = lognormal(),
data = forModelling,
prior = c(prior('normal(0, 1)', class = Intercept),
          prior('normal(0, 1)', class = b),
          prior('normal(0, 1)', class = sds)
          #prior('normal(0, 1)', class = Intercept, dpar = "hu"),
          #prior('normal(0, 1)', class = b, dpar = "hu")
),
chains = 4, iter = 2000)

fit3

forModelling %>%
  filter(Date >= "2018-07-14") %>%
  filter(Date <= "2018-11-30") %>%
  bind_rows(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit3, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              select(overall_fire_impact, Dinophysis_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(Date >= "2018-07-14") %>%
                          filter(Date <= "2018-11-30") %>%
                          select(overall_fire_impact, Dinophysis_Total_Scaled) %>%
                          mutate(overall_fire_impact = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-overall_fire_impact, -Dinophysis_Total_Scaled)) %>%
  mutate(overall_fire_impact = factor(overall_fire_impact, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-overall_fire_impact)) %>%
  mutate(type = relevel(factor(ifelse(name == "Dinophysis_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact == 'high' ~ 'High',
    overall_fire_impact == 'med' ~ 'Medium',
    overall_fire_impact == 'low' ~ 'Low',
    overall_fire_impact == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = log(value*14.5276)) %>%
  ggplot() + 
  geom_density(aes(x = value, fill = type)) +
  xlab("Log Dinophysis Counts") +
  facet_grid(rows = vars(`Bushfire Impact`), labeller = label_both) + 
  theme_cowplot() + 
  theme(strip.background = element_blank()) + 
  labs(fill='') 


### Calculate number of days HABs is above the action standards
### 200 cells/L

forModelling %>%
  filter(Date >= "2018-07-14") %>%
  filter(Date <= "2018-11-30") %>%
  bind_rows(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit3, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= "2018-07-14") %>%
              filter(Date <= "2018-11-30") %>%
              select(Date, overall_fire_impact, Dinophysis_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(Date >= "2018-07-14") %>%
                          filter(Date <= "2018-11-30") %>%
                          select(overall_fire_impact, Dinophysis_Total_Scaled) %>%
                          mutate(overall_fire_impact = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-Date, -overall_fire_impact, -Dinophysis_Total_Scaled)) %>%
  mutate(overall_fire_impact = factor(overall_fire_impact, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-Date, -overall_fire_impact)) %>%
  mutate(type = relevel(factor(ifelse(name == "Dinophysis_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact == 'high' ~ 'High',
    overall_fire_impact == 'med' ~ 'Medium',
    overall_fire_impact == 'low' ~ 'Low',
    overall_fire_impact == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = log(value*14.5276)) %>%
  filter(name == 'value') %>%
  mutate(above_detection_limit = ifelse(value >= 200, 1, 0)) %>%
  group_by(Date, `Bushfire Impact`) %>%
  summarise(mean_above_detection_limit = mean(above_detection_limit)) %>% arrange(Date) %>% 
  ggplot(aes(x = `Bushfire Impact`, y = mean_above_detection_limit, col = factor(`Bushfire Impact`))) + 
  geom_point() + 
  ylab("Probability of observing counts above the detection limit") +
  theme_classic()
