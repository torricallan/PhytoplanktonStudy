
library(tidyverse)
library(cowplot)
library(brms)
library(tidybayes)
library(lubridate)
library(readxl)

options(mc.cores = parallel::detectCores())

df <- read_excel("Slovenia_Dataset.xlsx")
fire_intensity <- read_excel("class_areas_all_fires.xlsx")

df$overall_fire_impact <- factor(ifelse(df$Date >= "2022-08-01" & df$Date <= "2022-10-01", "low", "zero"), levels = c("low","zero"))

colnames(df) <- c("Date", "rainfall", "sun_exposure", "temp", "depth", "total_phytoplankton", "chlorophyll", "alexandrium", "dinophysis", "pesudo", "overall_fire_impact")

df <- df %>%
  filter(!is.na(total_phytoplankton))

#15-07-2022 to 31-07-2022

df$log10Phytoplankton <- log10(df$total_phytoplankton)

df$month_of_year <- month(df$Date)

df$sun_exposure <- as.numeric(df$sun_exposure)

fit <- brm(log10Phytoplankton ~ 1 +
             month_of_year +
             rainfall +
             s(sun_exposure) +
             s(depth) +
             s(temp) +
             overall_fire_impact,
           family = student(),
           data = df,
           prior = c(prior('normal(0, 1)', class = Intercept),
                     prior('normal(0, 1)', class = b),
                     prior('normal(0, 1)', class = sds)
           ),
           chains = 4, iter = 2000)

fit

forModelling <- df

forModelling %>%
  filter(Date >= "2022-08-01") %>%
  filter(Date <= "2022-10-01") %>%
  bind_rows(forModelling %>%
              filter(Date >= "2022-08-01") %>%
              filter(Date <= "2022-10-01") %>%
              mutate(overall_fire_impact = 'zero')) %>%
  posterior_predict(fit, newdata = .) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= "2022-08-01") %>%
              filter(Date <= "2022-10-01") %>%
              select(overall_fire_impact, log10Phytoplankton) %>%
              bind_rows(forModelling %>%
                          filter(Date >= "2022-08-01") %>%
                          filter(Date <= "2022-10-01") %>%
                          select(overall_fire_impact, log10Phytoplankton) %>%
                          mutate(overall_fire_impact = 'zero'))) %>%
  pivot_longer(c(-overall_fire_impact, -log10Phytoplankton)) %>%
  mutate(overall_fire_impact = factor(overall_fire_impact, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-overall_fire_impact)) %>%
  mutate(type = relevel(factor(ifelse(name == "log10Phytoplankton", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact == 'high' ~ 'High',
    overall_fire_impact == 'med' ~ 'Medium',
    overall_fire_impact == 'low' ~ 'Low',
    overall_fire_impact == 'zero' ~ 'Zero'
  )) %>%
  ggplot() + 
  geom_density(aes(x = value, fill = type)) +
  xlab("Log10 Phytoplankton Counts") +
  facet_grid(rows = vars(`Bushfire Impact`), labeller = label_both) + 
  theme_cowplot() + 
  theme(strip.background = element_blank()) + 
  labs(fill='') 


forModelling %>%
  filter(Date >= "2022-08-01") %>%
  filter(Date <= "2022-10-01") %>%
  bind_rows(forModelling %>%
              filter(Date >= "2022-08-01") %>%
              filter(Date <= "2022-10-01") %>%
              mutate(overall_fire_impact = 'zero')) %>%
  posterior_predict(fit, newdata = .) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= "2022-08-01") %>%
              filter(Date <= "2022-10-01") %>%
              select(overall_fire_impact, log10Phytoplankton) %>%
              bind_rows(forModelling %>%
                          filter(Date >= "2022-08-01") %>%
                          filter(Date <= "2022-10-01") %>%
                          select(overall_fire_impact, log10Phytoplankton) %>%
                          mutate(overall_fire_impact = 'zero'))) %>%
  pivot_longer(c(-overall_fire_impact, -log10Phytoplankton)) %>%
  mutate(overall_fire_impact = factor(overall_fire_impact, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>%
  pivot_longer(c(-overall_fire_impact)) %>%
  mutate(type = relevel(factor(ifelse(name == "log10Phytoplankton", "Observed", "Predicted"), levels = c("Observed", "Predicted")), ref = "Predicted")) %>%
  mutate(`Bushfire Impact` = case_when(
    overall_fire_impact == 'high' ~ 'High',
    overall_fire_impact == 'med' ~ 'Medium',
    overall_fire_impact == 'low' ~ 'Low',
    overall_fire_impact == 'zero' ~ 'Zero'
  )) %>%
  group_by(type, `Bushfire Impact`) %>%
  median_qi(10^value)

### Box plot

forModelling %>%
  filter(Date >= "2022-08-01") %>%
  filter(Date <= "2022-10-01") %>%
  bind_rows(forModelling %>%
              filter(Date >= "2022-08-01") %>%
              filter(Date <= "2022-10-01") %>%
              mutate(overall_fire_impact = 'zero')) %>%
  posterior_predict(fit, newdata = .) %>%
  t() %>%
  as_tibble() %>% 
  bind_cols(forModelling %>%
              filter(Date >= "2022-08-01") %>%
              filter(Date <= "2022-10-01") %>%
              select(overall_fire_impact, log10Phytoplankton) %>%
              bind_rows(forModelling %>%
                          filter(Date >= "2022-08-01") %>%
                          filter(Date <= "2022-10-01") %>%
                          select(overall_fire_impact, log10Phytoplankton) %>%
                          mutate(overall_fire_impact = 'zero'))) %>%
  pivot_longer(c(-overall_fire_impact, -log10Phytoplankton)) %>%
  mutate(overall_fire_impact = factor(overall_fire_impact, levels = c("high", "med", "low", "zero"))) %>%
  select(-name) %>% 
  group_by(overall_fire_impact) %>%
  median_qi(value) %>%
  mutate(`Bushfire Impact` = ifelse(overall_fire_impact == "low", "Low", "Zero")) %>%
  ggplot(aes(x = `Bushfire Impact`, y = value, ymin = `.lower`, ymax = `.upper`, fill = factor(`Bushfire Impact`), col = factor(`Bushfire Impact`))) + 
  geom_bar(stat = "identity") +
  geom_errorbar(width=0.5, colour = "black") + 
  scale_fill_manual(values = c("#337538", "white")) +
  scale_colour_manual(values = c("white", "black")) +
  ylab("Log10 Phytoplankton") + 
  xlab("") +
  theme_classic() + 
  theme(legend.position = 'none',
        axis.text = element_text(size = 32),
        text = element_text(size = 32))
