library(tidyverse)
library(mgcv)

# Get data ----
load("~/LCBC/Projects/Cross_projects/MOAS/data/MOAS.RData")

dat <- MOAS %>%
  MOAS::filter_site() %>%
  MOAS::filter_trainingexposed(grepl("post", NCP_Condition)) %>%
  select(CrossProject_ID, Age, IQ_Matrix_Raw,
         MRI_aseg_left_hippocampus,
         MRI_aseg_right_hippocampus) %>%
  mutate(
    Hippocampus = MRI_aseg_left_hippocampus + MRI_aseg_right_hippocampus
  ) %>%
  select(-MRI_aseg_right_hippocampus, -MRI_aseg_left_hippocampus) %>%
  na.omit() %>%
  arrange(CrossProject_ID, Age) %>%
  group_by(CrossProject_ID) %>%
  mutate(
    TPs = n(),
    BL_Age = round(min(Age), 1),
    Matrix = mean(IQ_Matrix_Raw, na.rm=TRUE),
    Interval = Age - BL_Age
  ) %>%
  ungroup()

# Get distributions and deviations ----
fit <- gamm(Hippocampus ~ s(Age) + s(Age, by=Matrix),
            data = dat, random = list(CrossProject_ID =~ 1))
plot(fit$gam, select = 2, ylim = c(-20,50))

# Save random effects
random_effects <- ranef(fit$lme)$CrossProject_ID$`(Intercept)`

# Standard deviation of noise
sd_epsilon <- sd(residuals(fit$lme))

# BL age distribution + Matrix
bl_age_distribution <- dat %>%
  distinct(BL_Age, Matrix)
# pull(BL_Age)

# Interval distributions
interval_distributions <- dat %>%
  group_by(CrossProject_ID) %>%
  summarise(ints = list(Interval)) %>%
  pull(ints)

# Simulate data ----
# Unique IDs
n <- 1000

sim_data <- tibble(
  ID = seq(1, n, by = 1),
  random_intercept = sample(random_effects, size = n)
) %>%
  bind_cols(sample_n(bl_age_distribution, n)) %>%
  mutate(
    Interval = interval_distributions[sample(1:length(interval_distributions), size = n)]
  ) %>%
  unnest(cols = Interval) %>%
  mutate(Age = BL_Age + Interval) %>%
  mutate(
    Hippocampus = predict(fit$gam, newdata = .) + random_intercept + rnorm(nrow(.), sd = sd_epsilon),
    Hippocampus = as.numeric(Hippocampus),
    Matrix = Matrix + rnorm(nrow(.), mean = 0, sd = sd(bl_age_distribution$Matrix)/4),
    Age_group = factor(cut(Age, seq(0, 100, by=10))),
    group = case_when(Age < 15 ~ "Development", Age > 70 ~ "Ageing", TRUE ~ "Adulthood" ),
    group = factor(group, levels=c("Development", "Adulthood", "Ageing")),
    ID = factor(ID)
  )

# Plot it ----
sim_data %>% 
  ggplot(aes(x=Age, y=Hippocampus))+
  geom_point() + 
  geom_line(aes(group=ID)) + 
  geom_smooth()

sim_data %>% 
  ggplot(aes(x=Age, y=Matrix))+
  geom_point() + 
  geom_line(aes(group=ID)) + 
  geom_smooth()

# Save it ----
save(sim_data, file = here::here("data/sim_data.rda"))
