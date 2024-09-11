
library(tidyverse)
library(cowplot)
library(brms)
library(tidybayes)
library(lubridate)
library(readxl)

options(mc.cores = parallel::detectCores())

df <- read_excel("Chile_Dataset.xlsx")
fire_intensity <- read_excel("class_areas_all_fires.xlsx")

df$overall_fire_impact <- factor(ifelse(df$Date >= "2020-04-07" & df$Date <= "2020-06-07", "low", "zero"), levels = c("low","zero"))

colnames(df) <- c("Date", "rainfall", "sun_exposure", "temp", "depth", "total_phytoplankton", "fluroescence", "fire_intensity", "alexandrium", "dinophysis", "pesudo", "overall_fire_impact")

df <- df %>%
  filter(!is.na(total_phytoplankton))

df$log10Phytoplankton <- log10(df$total_phytoplankton)

df$month_of_year <- month(df$Date)

df$sun_exposure <- as.numeric(df$sun_exposure)

df$Pseudo_Total_Scaled <- df$pesudo/max(df$pesudo)
df$Alexandrium_Total_Scaled <- df$alexandrium/max(df$alexandrium)
df$Dinophysis_Total_Scaled <- df$dinophysis/max(df$dinophysis)

fit1 <- brm(bf(Pseudo_Total_Scaled ~ 1 +
                 month_of_year +
                 rainfall +
                 s(sun_exposure) +
                 s(depth) +
                 s(temp) +
                 overall_fire_impact
),
family = hurdle_lognormal(),
data = df,
prior = c(prior('normal(0, 1)', class = Intercept),
          prior('normal(0, 1)', class = b),
          prior('normal(0, 1)', class = sds)
          #prior('normal(0, 1)', class = Intercept, dpar = "hu"),
          #prior('normal(0, 1)', class = b, dpar = "hu")
),
chains = 4, iter = 2000)

fit1

df %>%
  filter(Date >= "2020-04-07") %>%
  filter(Date <= "2020-06-07") %>%
  bind_rows(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit1, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              select(overall_fire_impact, Pseudo_Total_Scaled) %>%
              bind_rows(df %>%
                          filter(Date >= "2020-04-07") %>%
                          filter(Date <= "2020-06-07") %>%
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
  mutate(value = log(value*123000)) %>%
  ggplot() + 
  geom_density(aes(x = value, fill = type)) +
  xlab("Log Pseudo Counts") +
  facet_grid(rows = vars(`Bushfire Impact`), labeller = label_both) + 
  theme_cowplot() + 
  theme(strip.background = element_blank()) + 
  labs(fill='') 


### Calculate number of days HABs is above the action standards
### 50000 cells/L

df %>%
  filter(Date >= "2020-04-07") %>%
  filter(Date <= "2020-06-07") %>%
  bind_rows(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit1, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              select(overall_fire_impact, Pseudo_Total_Scaled) %>%
              bind_rows(df %>%
                          filter(Date >= "2020-04-07") %>%
                          filter(Date <= "2020-06-07") %>%
                          select(Date, overall_fire_impact, Pseudo_Total_Scaled) %>%
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
  mutate(value = log(value*123000)) %>%
  filter(name == 'value') %>%
  mutate(above_detection_limit = ifelse(value >= 50000, 1, 0)) %>%
  group_by(Date, `Bushfire Impact`) %>%
  summarise(mean_above_detection_limit = mean(above_detection_limit)) %>% arrange(Date) %>% 
  ggplot(aes(x = `Bushfire Impact`, y = mean_above_detection_limit, col = factor(`Bushfire Impact`))) + 
  geom_point() + 
  ylab("Probability of observing counts above the detection limit") +
  theme_classic()

fit2 <- brm(Alexandrium_Total_Scaled ~ 1 +
              month_of_year +
              rainfall +
              s(sun_exposure) +
              s(depth) +
              s(temp) +
              overall_fire_impact,
            family = hurdle_lognormal(),
            data = df,
            prior = c(prior('normal(0, 1)', class = Intercept),
                      prior('normal(0, 1)', class = b),
                      prior('normal(0, 1)', class = sds)
            ),
            chains = 4, iter = 2000)

fit2

plot(conditional_effects(fit2))

df %>%
  filter(Date >= "2020-04-07") %>%
  filter(Date <= "2020-06-07") %>%
  bind_rows(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit2, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              select(overall_fire_impact, Alexandrium_Total_Scaled) %>%
              bind_rows(df %>%
                          filter(Date >= "2020-04-07") %>%
                          filter(Date <= "2020-06-07") %>%
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
  mutate(value = log(value*100)) %>%
  ggplot() + 
  geom_density(aes(x = value, fill = type)) +
  xlab("Log Alexandrium Counts") +
  facet_grid(rows = vars(`Bushfire Impact`), labeller = label_both) + 
  theme_cowplot() + 
  theme(strip.background = element_blank()) + 
  labs(fill='') 


### Calculate number of days HABs is above the action standards
### 200 cells/L

df %>%
  filter(Date >= "2020-04-07") %>%
  filter(Date <= "2020-06-07") %>%
  bind_rows(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit2, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              select(overall_fire_impact, Alexandrium_Total_Scaled) %>%
              bind_rows(df %>%
                          filter(Date >= "2020-04-07") %>%
                          filter(Date <= "2020-06-07") %>%
                          select(Date, overall_fire_impact, Alexandrium_Total_Scaled) %>%
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
  mutate(value = log(value*100)) %>%
  filter(name == 'value') %>%
  mutate(above_detection_limit = ifelse(value >= 200, 1, 0)) %>%
  group_by(Date, `Bushfire Impact`) %>%
  summarise(mean_above_detection_limit = mean(above_detection_limit)) %>% arrange(Date) %>% 
  ggplot(aes(x = `Bushfire Impact`, y = mean_above_detection_limit, col = factor(`Bushfire Impact`))) + 
  geom_point() + 
  ylab("Probability of observing counts above the detection limit") +
  theme_classic()


fit3 <- brm(Dinophysis_Total_Scaled ~ 1 +
              month_of_year +
              rainfall +
              s(sun_exposure) +
              s(depth) +
              s(temp) +
              overall_fire_impact,,
            family = hurdle_lognormal(),
            data = df,
            prior = c(prior('normal(0, 1)', class = Intercept),
                      prior('normal(0, 1)', class = b),
                      prior('normal(0, 1)', class = sds)
            ),
            chains = 4, iter = 2000)

fit3

plot(conditional_effects(fit3))

df %>%
  filter(Date >= "2020-04-07") %>%
  filter(Date <= "2020-06-07") %>%
  bind_rows(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit3, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              select(overall_fire_impact, Dinophysis_Total_Scaled) %>%
              bind_rows(df %>%
                          filter(Date >= "2020-04-07") %>%
                          filter(Date <= "2020-06-07") %>%
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
  mutate(value = log(value*700)) %>%
  ggplot() + 
  geom_density(aes(x = value, fill = type)) +
  xlab("Log Dino Counts") +
  facet_grid(rows = vars(`Bushfire Impact`), labeller = label_both) + 
  theme_cowplot() + 
  theme(strip.background = element_blank()) + 
  labs(fill='') 


### Calculate number of days HABs is above the action standards
### 50000 cells/L

df %>%
  filter(Date >= "2020-04-07") %>%
  filter(Date <= "2020-06-07") %>%
  bind_rows(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              mutate(overall_fire_impact = 'zero')) %>% 
  posterior_predict(fit3, newdata = ., allow_new_levels = TRUE) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(df %>%
              filter(Date >= "2020-04-07") %>%
              filter(Date <= "2020-06-07") %>%
              select(overall_fire_impact, Dinophysis_Total_Scaled) %>%
              bind_rows(df %>%
                          filter(Date >= "2020-04-07") %>%
                          filter(Date <= "2020-06-07") %>%
                          select(Date, overall_fire_impact, Dinophysis_Total_Scaled) %>%
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
  mutate(value = log(value*700)) %>%
  filter(name == 'value') %>%
  mutate(above_detection_limit = ifelse(value >= 200, 1, 0)) %>%
  group_by(Date, `Bushfire Impact`) %>%
  summarise(mean_above_detection_limit = mean(above_detection_limit)) %>% arrange(Date) %>% 
  ggplot(aes(x = `Bushfire Impact`, y = mean_above_detection_limit, col = factor(`Bushfire Impact`))) + 
  geom_point() + 
  ylab("Probability of observing counts above the detection limit") +
  theme_classic()
