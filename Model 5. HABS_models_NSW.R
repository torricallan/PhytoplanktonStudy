
library(tidyverse)
library(cowplot)
library(brms)
library(tidybayes)
library(lubridate)
library(readxl)

options(mc.cores = parallel::detectCores())

forModelling <- read_excel("AllSites.xlsx")
forModelling$time <- interval(min(forModelling$CollectionDate), forModelling$CollectionDate) %/% weeks(1)
forModelling$month_of_year <- factor(month(forModelling$CollectionDate))

forModelling$Location <- paste0(forModelling$Area, " ", forModelling$Area2)

forModelling$Location <- ifelse(forModelling$Location == 'Maning River', 'Manning River', 
                                ifelse(forModelling$Location == 'NA Lake', 'Pambula', 
                                       ifelse(forModelling$Location == 'Pambula Lake', 'Pambula',
                                              ifelse(forModelling$Location == 'Pambula River', 'Pambula',
                                                     ifelse(forModelling$Location == 'Port Stephen', 'Port Stephens',
                                                            ifelse(forModelling$Location == 'Port Stephes', 'Port Stephens',
                                                                   ifelse(forModelling$Location == 'Shoahaven River', 'Shoalhaven',
                                                                          ifelse(forModelling$Location == 'Shoalhaven Crookhaven', 'Shoalhaven',
                                                                                 ifelse(forModelling$Location == 'Shoalhaven River', 'Shoalhaven',
                                                                                        ifelse(forModelling$Location == 'Wagonga Inlet', 'Wagonga',
                                                                                               ifelse(forModelling$Location == 'Wapengo Lake', 'Wapengo',
                                                                                                      ifelse(forModelling$Location == 'Wonboyn Lake', 'Wonboyn', forModelling$Location))))))))))))

fire_impact <- read_excel("fire_impact.xlsx")

forModelling <- forModelling %>% 
  left_join(fire_impact, by = c("Location" = "CATCHMENTN"))

forModelling$`Overall impact` <- ifelse(forModelling$CollectionDate < '2020-03-01', 'zero', forModelling$`Overall impact`)
forModelling$overall_fire_impact <- as.factor(forModelling$`Overall impact`)

forModelling <- forModelling %>%
  mutate(latitude = case_when(
    Location == 'Georges River' ~ -34.02245,
    Location == 'Camden Haven' ~ -31.64478,
    Location == 'Hastings River' ~ -31.40406,
    Location == 'Hawkesbury River' ~ -33.5443,
    Location == 'Manning River' ~ -31.89088,
    Location == 'Pambula' ~ -36.96522,
    Location == 'Port Stephens' ~ -32.7196,
    Location == 'Shoalhaven' ~ -34.9118,
    Location == 'Wagonga' ~ -36.22161,
    Location == 'Wallis Lake' ~ -32.18268,
    Location == 'Wapengo' ~ -36.60182,
    Location == 'Wonboyn' ~ -37.24121
  ))


forModelling$Location <- forcats::fct_reorder(forModelling$Location, forModelling$latitude, .desc = TRUE)
forModelling$overall_fire_impact <- relevel(forModelling$overall_fire_impact, ref = 'zero')

forModelling$overall_fire_impact_2 <- factor(ifelse(forModelling$CollectionDate < '2020-03-01', 'zero',
                                                    ifelse(forModelling$Location == 'Camden Haven', 'med',
                                                           ifelse(forModelling$Location == 'Wallis Lake', 'high',
                                                                  ifelse(forModelling$Location == 'Wagonga', 'med', as.character(forModelling$overall_fire_impact))))))


covariates <- read_csv('data_for_modelling.csv')

forModelling <- left_join(forModelling, covariates, by = c("CollectionDate" = "Date", "Location"))

forModelling$Pseudo_Total_Scaled <- forModelling$Pseudo_Total/max(forModelling$Pseudo_Total)
forModelling$Alexandrium_Total_Scaled <- forModelling$Alexandrium_Total/max(forModelling$Alexandrium_Total)
forModelling$Dinophysis_Total_Scaled <- forModelling$Dinophysis_Total/max(forModelling$Dinophysis_Total)

fit1 <- brm(bf(Pseudo_Total_Scaled ~ 1 +
                 month_of_year +
                 s(rainfall72) +
                 s(solar_exposure) +
                 s(salinity72) + 
                 s(depth72) +
                 s(temp72) +
                 overall_fire_impact_2 +
                 (1 | Location)
),
family = hurdle_lognormal(),
data = forModelling,
prior = c(prior('normal(0, 1)', class = Intercept),
          prior('normal(0, 1)', class = b),
          prior('normal(0, 1)', class = sd),
          prior('normal(0, 1)', class = sds)
          #prior('normal(0, 1)', class = Intercept, dpar = "hu"),
          #prior('normal(0, 1)', class = b, dpar = "hu")
),
chains = 4, iter = 2000)

fit1

plot(conditional_effects(fit1))

forModelling %>%
  filter(CollectionDate >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>% 
  select(month_of_year, rainfall72, solar_exposure, salinity72, depth72, temp72, overall_fire_impact_2, Location) %>% 
  filter(!is.na(rainfall72)) %>%
  posterior_predict(fit1, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              filter(!is.na(rainfall72)) %>%
              select(Location, overall_fire_impact_2, Pseudo_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(CollectionDate >= '2020-03-01') %>%
                          filter(!is.na(rainfall72)) %>%
                          select(Location, overall_fire_impact_2, Pseudo_Total_Scaled) %>%
                          mutate(overall_fire_impact_2 = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2, -Pseudo_Total_Scaled)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2)) %>%
  mutate(type = relevel(factor(ifelse(name == "Pseudo_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = log(value*26016000)) %>%
  ggplot() + 
  geom_density(aes(x = value, fill = type)) +
  xlab("Log Pseudo Counts") +
  facet_grid(rows = vars(`Bushfire Impact`), labeller = label_both) + 
  #xlim(0, 100000) +
  theme_cowplot() + 
  theme(strip.background = element_blank()) + 
  labs(fill='') 

### Calculate number of days HABs is above the action standards
### 50000 cells/L

forModelling %>%
  filter(CollectionDate >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>% 
  select(month_of_year, rainfall72, solar_exposure, salinity72, depth72, temp72, overall_fire_impact_2, Location) %>% 
  filter(!is.na(rainfall72)) %>%
  posterior_predict(fit1, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              filter(!is.na(rainfall72)) %>%
              select(CollectionDate, Location, overall_fire_impact_2, Pseudo_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(CollectionDate >= '2020-03-01') %>%
                          filter(!is.na(rainfall72)) %>%
                          select(CollectionDate, Location, overall_fire_impact_2, Pseudo_Total_Scaled) %>%
                          mutate(overall_fire_impact_2 = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-CollectionDate, -Location, -overall_fire_impact_2, -Pseudo_Total_Scaled)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-CollectionDate, -Location, -overall_fire_impact_2)) %>%
  mutate(type = relevel(factor(ifelse(name == "Pseudo_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = value*26016000) %>%
  filter(name == 'value') %>%
  mutate(above_detection_limit = ifelse(value >= 50000, 1, 0)) %>%
  group_by(CollectionDate, Location, `Bushfire Impact`) %>%
  summarise(mean_above_detection_limit = mean(above_detection_limit)) %>% arrange(Location, CollectionDate) %>% 
  ggplot(aes(x = `Bushfire Impact`, y = mean_above_detection_limit, col = factor(`Bushfire Impact`))) + 
  geom_point() + 
  facet_grid(~Location) + 
  ylab("Probability of observing counts above the detection limit") +
  theme_classic()

fit2 <- brm(Alexandrium_Total_Scaled ~ 1 +
              month_of_year +
              s(rainfall72) +
              s(solar_exposure) +
              s(salinity72) + 
              s(depth72) +
              s(temp72) +
              overall_fire_impact_2 +
              (1 | Location),
            family = hurdle_lognormal(),
            data = forModelling,
            prior = c(prior('normal(0, 1)', class = Intercept),
                      prior('normal(0, 1)', class = b),
                      prior('normal(0, 1)', class = sd),
                      prior('normal(0, 1)', class = sds)
            ),
            chains = 4, iter = 2000)

fit2

plot(conditional_effects(fit2))

forModelling %>%
  filter(CollectionDate >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>% 
  select(month_of_year, rainfall72, solar_exposure, salinity72, depth72, temp72, overall_fire_impact_2, Location) %>% 
  filter(!is.na(rainfall72)) %>%
  posterior_predict(fit2, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              filter(!is.na(rainfall72)) %>%
              select(Location, overall_fire_impact_2, Alexandrium_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(CollectionDate >= '2020-03-01') %>%
                          filter(!is.na(rainfall72)) %>%
                          select(Location, overall_fire_impact_2, Alexandrium_Total_Scaled) %>%
                          mutate(overall_fire_impact_2 = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2, -Alexandrium_Total_Scaled)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2)) %>%
  mutate(type = relevel(factor(ifelse(name == "Alexandrium_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = log(value*8350)) %>%
  ggplot() + 
  geom_density(aes(x = value, fill = type)) +
  xlab("Log Alexandrium Counts") +
  facet_grid(rows = vars(`Bushfire Impact`), labeller = label_both) + 
  #xlim(0, 100000) +
  theme_cowplot() + 
  theme(strip.background = element_blank()) + 
  labs(fill='') 


### Calculate number of days HABs is above the action standards
### 200 cells/L

forModelling %>%
  filter(CollectionDate >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>% 
  select(month_of_year, rainfall72, solar_exposure, salinity72, depth72, temp72, overall_fire_impact_2, Location) %>% 
  filter(!is.na(rainfall72)) %>%
  posterior_predict(fit2, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              filter(!is.na(rainfall72)) %>%
              select(CollectionDate, Location, overall_fire_impact_2, Alexandrium_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(CollectionDate >= '2020-03-01') %>%
                          filter(!is.na(rainfall72)) %>%
                          select(CollectionDate, Location, overall_fire_impact_2, Alexandrium_Total_Scaled) %>%
                          mutate(overall_fire_impact_2 = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-CollectionDate, -Location, -overall_fire_impact_2, -Alexandrium_Total_Scaled)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-CollectionDate, -Location, -overall_fire_impact_2)) %>%
  mutate(type = relevel(factor(ifelse(name == "Alexandrium_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = value*8350) %>%
  filter(name == 'value') %>%
  mutate(above_detection_limit = ifelse(value >= 200, 1, 0)) %>%
  group_by(CollectionDate, Location, `Bushfire Impact`) %>%
  summarise(mean_above_detection_limit = mean(above_detection_limit)) %>% arrange(Location, CollectionDate) %>% 
  ggplot(aes(x = `Bushfire Impact`, y = mean_above_detection_limit, col = factor(`Bushfire Impact`))) + 
  geom_point() + 
  facet_grid(~Location) + 
  ylab("Probability of observing counts above the detection limit") +
  theme_classic()


fit3 <- brm(Dinophysis_Total_Scaled ~ 1 +
              month_of_year +
              s(rainfall72) +
              s(solar_exposure) +
              s(salinity72) + 
              s(depth72) +
              s(temp72) +
              overall_fire_impact_2 + 
              (1 | Location),
            family = hurdle_lognormal(),
            data = forModelling,
            prior = c(prior('normal(0, 1)', class = Intercept),
                      prior('normal(0, 1)', class = b),
                      prior('normal(0, 1)', class = sd),
                      prior('normal(0, 1)', class = sds)
            ),
            chains = 4, iter = 2000)

fit3

plot(conditional_effects(fit3))

forModelling %>%
  filter(CollectionDate >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>% 
  select(month_of_year, rainfall72, solar_exposure, salinity72, depth72, temp72, overall_fire_impact_2, Location) %>% 
  filter(!is.na(rainfall72)) %>%
  posterior_predict(fit3, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              filter(!is.na(rainfall72)) %>%
              select(Location, overall_fire_impact_2, Dinophysis_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(CollectionDate >= '2020-03-01') %>%
                          filter(!is.na(rainfall72)) %>%
                          select(Location, overall_fire_impact_2, Dinophysis_Total_Scaled) %>%
                          mutate(overall_fire_impact_2 = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2, -Dinophysis_Total_Scaled)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2)) %>%
  mutate(type = relevel(factor(ifelse(name == "Dinophysis_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = log(value*33000)) %>%
  ggplot() + 
  geom_density(aes(x = value, fill = type)) +
  xlab("Log Dinophysis Counts") +
  facet_grid(rows = vars(`Bushfire Impact`), labeller = label_both) + 
  #xlim(0, 100000) +
  theme_cowplot() + 
  theme(strip.background = element_blank()) + 
  labs(fill='') 

forModelling %>%
  filter(CollectionDate >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>% 
  select(month_of_year, rainfall72, solar_exposure, salinity72, depth72, temp72, overall_fire_impact_2, Location) %>% 
  filter(!is.na(rainfall72)) %>%
  posterior_predict(fit3, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              filter(!is.na(rainfall72)) %>%
              select(Location, overall_fire_impact_2, Dinophysis_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(CollectionDate >= '2020-03-01') %>%
                          filter(!is.na(rainfall72)) %>%
                          select(Location, overall_fire_impact_2, Dinophysis_Total_Scaled) %>%
                          mutate(overall_fire_impact_2 = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2, -Dinophysis_Total_Scaled)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2)) %>%
  mutate(type = relevel(factor(ifelse(name == "Dinophysis_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = log(value*33000)) %>%
  filter(!is.infinite(value)) %>%
  
  group_by(type, overall_fire_impact_2) %>%
  median_qi(exp(value))

### Calculate number of days HABs is above the action standards
### 500 cells/L

forModelling %>%
  filter(CollectionDate >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>% 
  select(month_of_year, rainfall72, solar_exposure, salinity72, depth72, temp72, overall_fire_impact_2, Location) %>% 
  filter(!is.na(rainfall72)) %>%
  posterior_predict(fit3, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              filter(!is.na(rainfall72)) %>%
              select(CollectionDate, Location, overall_fire_impact_2, Dinophysis_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(CollectionDate >= '2020-03-01') %>%
                          filter(!is.na(rainfall72)) %>%
                          select(CollectionDate, Location, overall_fire_impact_2, Dinophysis_Total_Scaled) %>%
                          mutate(overall_fire_impact_2 = 'zero')
              ) 
  ) %>%
  pivot_longer(c(-CollectionDate, -Location, -overall_fire_impact_2, -Dinophysis_Total_Scaled)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-CollectionDate, -Location, -overall_fire_impact_2)) %>%
  mutate(type = relevel(factor(ifelse(name == "Dinophysis_Total_Scaled", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = value*33000) %>%
  filter(name == 'value') %>%
  mutate(above_detection_limit = ifelse(value >= 200, 1, 0)) %>%
  group_by(CollectionDate, Location, `Bushfire Impact`) %>%
  summarise(mean_above_detection_limit = mean(above_detection_limit)) %>% arrange(Location, CollectionDate) %>% 
  ggplot(aes(x = `Bushfire Impact`, y = mean_above_detection_limit, col = factor(`Bushfire Impact`))) + 
  geom_point() + 
  facet_grid(~Location) + 
  ylab("Probability of observing counts above the detection limit") +
  theme_classic()

### ccdf plots

# pseudo
predictions <- forModelling %>%
  filter(CollectionDate >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>% 
  select(month_of_year, rainfall72, solar_exposure, salinity72, depth72, temp72, overall_fire_impact_2, Location) %>% 
  filter(!is.na(rainfall72)) %>%
  mutate(id = row_number()) %>%
  posterior_predict(fit1, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              filter(!is.na(rainfall72)) %>%
              select(Location, overall_fire_impact_2, Pseudo_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(CollectionDate >= '2020-03-01') %>%
                          filter(!is.na(rainfall72)) %>%
                          select(Location, overall_fire_impact_2, Pseudo_Total_Scaled) %>%
                          mutate(overall_fire_impact_2 = 'zero') 
              ) %>%
              mutate(id = row_number())
  ) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2, -Pseudo_Total_Scaled, -id)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = value*26016000) %>%
  group_by(id, overall_fire_impact_2) %>%
  median_qi(value)

high <- predictions %>%
  filter(overall_fire_impact_2 == 'high') %>%
  arrange(value)

n <- dim(high)[1]
high$ccdf <- seq(n,1,-1)/n

med <- predictions %>%
  filter(overall_fire_impact_2 == 'med') %>%
  arrange(value)

n <- dim(med)[1]
med$ccdf <- seq(n,1,-1)/n

low <- predictions %>%
  filter(overall_fire_impact_2 == 'low') %>%
  arrange(value)

n <- dim(low)[1]
low$ccdf <- seq(n,1,-1)/n

zero <- predictions %>%
  filter(overall_fire_impact_2 == 'zero') %>%
  arrange(value)

n <- dim(zero)[1]
zero$ccdf <- seq(n,1,-1)/n

high %>%
  bind_rows(med) %>%
  bind_rows(low) %>%
  bind_rows(zero) %>%
  ggplot() + 
  geom_line(aes(value, ccdf, col = overall_fire_impact_2, linetype = overall_fire_impact_2), linewidth = 1) +
  scale_colour_manual(values = c("#B31529", "#E69F00", "#337538", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dashed")) +
  labs(y = "Probability of seeing counts at least this high", x= expression(paste("Count of ", italic("Pseudo-nitzschia spp."))), col = "Bushfire Impact") +
  theme_cowplot() + 
  theme(legend.position = 'none',
        text = element_text(size = 32),
        axis.text = element_text(size = 20))

# alexandrium

predictions <- forModelling %>%
  filter(CollectionDate >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>% 
  select(month_of_year, rainfall72, solar_exposure, salinity72, depth72, temp72, overall_fire_impact_2, Location) %>% 
  filter(!is.na(rainfall72)) %>%
  mutate(id = row_number()) %>%
  posterior_predict(fit2, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              filter(!is.na(rainfall72)) %>%
              select(Location, overall_fire_impact_2, Alexandrium_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(CollectionDate >= '2020-03-01') %>%
                          filter(!is.na(rainfall72)) %>%
                          select(Location, overall_fire_impact_2, Alexandrium_Total_Scaled) %>%
                          mutate(overall_fire_impact_2 = 'zero') 
              ) %>%
              mutate(id = row_number())
  ) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2, -Alexandrium_Total_Scaled, -id)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = value*8350) %>%
  filter(value > 0) %>%
  group_by(id, overall_fire_impact_2) %>%
  median_qi(value)

high <- predictions %>%
  filter(overall_fire_impact_2 == 'high') %>%
  arrange(value)

n <- dim(high)[1]
high$ccdf <- seq(n,1,-1)/n

med <- predictions %>%
  filter(overall_fire_impact_2 == 'med') %>%
  arrange(value)

n <- dim(med)[1]
med$ccdf <- seq(n,1,-1)/n

low <- predictions %>%
  filter(overall_fire_impact_2 == 'low') %>%
  arrange(value)

n <- dim(low)[1]
low$ccdf <- seq(n,1,-1)/n

zero <- predictions %>%
  filter(overall_fire_impact_2 == 'zero') %>%
  arrange(value)

n <- dim(zero)[1]
zero$ccdf <- seq(n,1,-1)/n

high %>%
  bind_rows(med) %>%
  bind_rows(low) %>%
  bind_rows(zero) %>%
  ggplot() + 
  geom_line(aes(value, ccdf, col = overall_fire_impact_2, linetype = overall_fire_impact_2), size = 1) +
  xlim(0, NA) +
  scale_colour_manual(values = c("#B31529", "#E69F00", "#337538", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dashed")) +
  labs(y = "Probability of seeing counts at least this high", x= expression(paste("Count of ", italic("Alexandrium spp."))), col = "Bushfire Impact") +
  theme_cowplot() + 
  theme(legend.position = 'none',
        text = element_text(size = 32),
        axis.text = element_text(size = 20))

# dino

predictions <- forModelling %>%
  filter(CollectionDate >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>% 
  select(month_of_year, rainfall72, solar_exposure, salinity72, depth72, temp72, overall_fire_impact_2, Location) %>% 
  filter(!is.na(rainfall72)) %>%
  mutate(id = row_number()) %>%
  posterior_predict(fit3, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(CollectionDate >= '2020-03-01') %>%
              filter(!is.na(rainfall72)) %>%
              select(Location, overall_fire_impact_2, Dinophysis_Total_Scaled) %>%
              bind_rows(forModelling %>%
                          filter(CollectionDate >= '2020-03-01') %>%
                          filter(!is.na(rainfall72)) %>%
                          select(Location, overall_fire_impact_2, Dinophysis_Total_Scaled) %>%
                          mutate(overall_fire_impact_2 = 'zero') 
              ) %>%
              mutate(id = row_number())
  ) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2, -Dinophysis_Total_Scaled, -id)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = value*33000) %>%
  filter(value > 0) %>%
  group_by(id, overall_fire_impact_2) %>%
  median_qi(value)

high <- predictions %>%
  filter(overall_fire_impact_2 == 'high') %>%
  arrange(value)

n <- dim(high)[1]
high$ccdf <- seq(n,1,-1)/n

med <- predictions %>%
  filter(overall_fire_impact_2 == 'med') %>%
  arrange(value)

n <- dim(med)[1]
med$ccdf <- seq(n,1,-1)/n

low <- predictions %>%
  filter(overall_fire_impact_2 == 'low') %>%
  arrange(value)

n <- dim(low)[1]
low$ccdf <- seq(n,1,-1)/n

zero <- predictions %>%
  filter(overall_fire_impact_2 == 'zero') %>%
  arrange(value)

n <- dim(zero)[1]
zero$ccdf <- seq(n,1,-1)/n

high %>%
  bind_rows(med) %>%
  bind_rows(low) %>%
  bind_rows(zero) %>%
  ggplot() + 
  geom_line(aes(value, ccdf, col = overall_fire_impact_2, linetype = overall_fire_impact_2), size = 1) +
  scale_colour_manual(values = c("#B31529", "#E69F00", "#337538", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dashed")) +
  labs(y = "Probability of seeing dinophysis counts at least this high", x= "Count", col = "Bushfire Impact") +
  theme_cowplot() + 
  labs(y = "Probability of seeing counts at least this high", x= expression(paste("Count of ", italic("Dinophysis spp."))), col = "Bushfire Impact") +
  theme_cowplot() + 
  theme(legend.position = 'none',
        text = element_text(size = 32),
        axis.text = element_text(size = 20))
