
library(tidyverse)
library(cowplot)
library(brms)
library(tidybayes)
library(lubridate)
library(readxl)

options(mc.cores = parallel::detectCores())

forModelling <- read_csv("data_for_modelling.csv")
forModelling$time <- interval(min(forModelling$Date), forModelling$Date) %/% weeks(1)
forModelling$month_of_year <- factor(month(forModelling$Date))

fire_impact <- read_excel("fire_impact.xlsx")

forModelling <- forModelling %>% 
  left_join(fire_impact, by = c("Location" = "CATCHMENTN"))

forModelling$`Overall impact` <- ifelse(forModelling$Date < '2020-03-01', 'zero', forModelling$`Overall impact`)
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
    Location == 'Wonboyn River' ~ -37.24121
  ))

forModelling$Location <- forcats::fct_reorder(forModelling$Location, forModelling$latitude, .desc = TRUE)
forModelling$overall_fire_impact <- relevel(forModelling$overall_fire_impact, ref = 'zero')

forModelling$overall_fire_impact_2 <- factor(ifelse(forModelling$Date < '2020-03-01', 'zero',
                                                    ifelse(forModelling$Location == 'Camden Haven', 'med',
                                                           ifelse(forModelling$Location == 'Wallis Lake', 'high',
                                                                  ifelse(forModelling$Location == 'Wagonga', 'med', as.character(forModelling$overall_fire_impact))))))

fit <- brm(log10Phytoplankton ~ 1 +
             month_of_year +
             s(rainfall72) +
             s(solar_exposure) +
             s(salinity72) + 
             s(depth72) +
             s(temp72) +
             overall_fire_impact_2 +
             (1 | Location),
           family = student(),
           data = forModelling,
           prior = c(prior('normal(0, 1)', class = Intercept),
                     prior('normal(0, 1)', class = b),
                     prior('normal(0, 1)', class = sd),
                     prior('normal(0, 1)', class = sds)),
           chains = 4, iter = 2000)

fit

### Plotting

predictions <- forModelling %>%
  filter(Date >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(Date >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>%
  posterior_predict(fit, newdata = .) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= '2020-03-01') %>%
              select(Location, overall_fire_impact_2, log10Phytoplankton) %>%
              bind_rows(forModelling %>%
                          filter(Date >= '2020-03-01') %>%
                          select(Location, overall_fire_impact_2, log10Phytoplankton) %>%
                          mutate(overall_fire_impact_2 = 'zero'))) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2, -log10Phytoplankton)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2)) %>%
  mutate(type = relevel(factor(ifelse(name == "log10Phytoplankton", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  ))

predictions$type <- relevel(predictions$type, ref = 'Predicted')

predictions %>%
  ggplot() + 
  geom_density(aes(x = value, fill = `Bushfire Impact`, alpha = type)) +
  xlab("Log10 Phytoplankton Counts") +
  facet_grid(rows = vars(`Bushfire Impact`), labeller = label_both) + 
  scale_fill_manual(values = c("#B31529", "#E69F00", "#337538", "#FFFFFF")) +
  scale_alpha_manual(values = c(1.0, 0.5)) +
  xlim(5, 8) +
  theme_cowplot() + 
  theme(strip.background = element_blank(),
        legend.position = "none",
        text = element_text(size=32),
        axis.text = element_text(size = 24),
        strip.text.y = element_blank()) + 
  labs(fill='') 

### Numerical summary 

forModelling %>%
  filter(Date >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(Date >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>%
  posterior_predict(fit, newdata = .) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= '2020-03-01') %>%
              select(Location, overall_fire_impact_2, log10Phytoplankton) %>%
              bind_rows(forModelling %>%
                          filter(Date >= '2020-03-01') %>%
                          select(Location, overall_fire_impact_2, log10Phytoplankton) %>%
                          mutate(overall_fire_impact_2 = 'zero'))) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2, -log10Phytoplankton)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2)) %>%
  mutate(type = relevel(factor(ifelse(name == "log10Phytoplankton", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  group_by(type, `Bushfire Impact`) %>%
  median_qi(10^value)

### Plot summary per location

predictions_per_locations <- forModelling %>%
  filter(Date >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(Date >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>%
  posterior_predict(fit, newdata = .) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= '2020-03-01') %>%
              select(Location, overall_fire_impact_2, log10Phytoplankton) %>%
              bind_rows(forModelling %>%
                          filter(Date >= '2020-03-01') %>%
                          select(Location, overall_fire_impact_2, log10Phytoplankton) %>%
                          mutate(overall_fire_impact_2 = 'zero'))) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2, -log10Phytoplankton)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2)) %>%
  mutate(type = relevel(factor(ifelse(name == "log10Phytoplankton", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  ))
  

predictions_per_locations$overall_fire_impact_2 <- fct_relevel(predictions_per_locations$overall_fire_impact_2, c('high', 'med', 'low', 'zero'))


predictions_per_locations %>%
  filter(type == 'Predicted') %>%
  ggplot() + 
  geom_density(aes(x = value, fill = overall_fire_impact_2)) +
  xlab("Log10 Phytoplankton Counts") +
  facet_wrap(~Location) + 
  xlim(5, 8) +
  scale_fill_manual(values = c("#337538", "#E69F00", "#B31529", "#FFFFFF")) +
  theme_cowplot() + 
  theme(strip.background = element_blank(),
        legend.position =  'none',
        text = element_text(size = 20)) + 
  labs(fill='') 

zero_predictions <- predictions_per_locations %>%
  filter(type == 'Predicted') %>%
  filter(overall_fire_impact_2 == 'zero') %>%
  filter(Location == 'Wallis Lake')

non_zero_predictions <- predictions_per_locations %>%
  filter(type == 'Predicted') %>%
  filter(overall_fire_impact_2 != 'zero') %>%
  filter(Location == 'Wallis Lake')

zero_predictions %>%
  bind_cols(non_zero_predictions, by = "Location") %>%
  ggplot() + 
  geom_density(aes(x = `value...10` - `value...4`, fill = `overall_fire_impact_2...8`)) +
  geom_vline(xintercept = 0) +
  xlab("Difference in Log10 Phytoplankton Counts") +
  facet_wrap(~`Location...1`) + 
  scale_fill_manual(values = c("#B31529", "#FFFFFF")) +
  theme_cowplot() + 
  theme(strip.background = element_blank(),
        legend.position =  'none',
        text = element_text(size = 20)) + 
  labs(fill='') 

## ccdf 

predictions <- forModelling %>%
  filter(Date >= '2020-03-01') %>%
  bind_rows(forModelling %>%
              filter(Date >= '2020-03-01') %>%
              mutate(overall_fire_impact_2 = 'zero')) %>%
  mutate(id = row_number()) %>%
  posterior_predict(fit, newdata = .) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= '2020-03-01') %>%
              select(Location, overall_fire_impact_2, log10Phytoplankton) %>%
              bind_rows(forModelling %>%
                          filter(Date >= '2020-03-01') %>%
                          select(Location, overall_fire_impact_2, log10Phytoplankton) %>%
                          mutate(overall_fire_impact_2 = 'zero')) %>%
              mutate(id = row_number())) %>%
  pivot_longer(c(-Location, -overall_fire_impact_2, -log10Phytoplankton, -id)) %>%
  mutate(overall_fire_impact_2 = factor(overall_fire_impact_2, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact_2 == 'high' ~ 'High',
    overall_fire_impact_2 == 'med' ~ 'Medium',
    overall_fire_impact_2 == 'low' ~ 'Low',
    overall_fire_impact_2 == 'zero' ~ 'Zero'
  )) %>%
  mutate(value = 10^value) %>%
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
  labs(y = "Probability of seeing counts at least this high", x= "Phytoplankton Count", col = "Bushfire Impact") +
  theme_cowplot() + 
  theme(legend.position = 'none',
        text = element_text(size = 32),
        axis.text = element_text(size = 24))

