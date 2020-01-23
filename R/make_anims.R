library(tidyverse)
library(gganimate)

source(paste0(here::here(), "/R/synth-data.R"))
load(paste0(here::here("data/"), "sim_data.rda"))
sim_data <- sim_data %>% 
  ungroup() %>% 
  mutate(
    nn = row_number(),
    Hippocampus = as.numeric(Hippocampus),
    group = case_when(Age < 15 ~ "Development", Age > 70 ~ "Ageing", TRUE ~ "Adulthood" )
  )

# Cohort anim ----
states <- list(
  T1 = c("(0,10]", "(80,90]"),
  T2 = c("(0,10]","(30,40]", "(80,90]"),
  T3 = c("(0,10]","(20,30]","(30,40]", "(80,90]"),
  T4 = c("(0,10]","(10,20]","(20,30]","(30,40]", "(80,90]"),
  T5 = c("(0,10]","(10,20]","(20,30]","(30,40]","(40,50]", "(80,90]"),
  T6 = c("(0,10]","(10,20]","(20,30]","(30,40]","(40,50]", "(50,60]", "(80,90]"),
  T7 = c("(0,10]","(10,20]","(20,30]","(30,40]","(40,50]", "(50,60]", "(60,70]", "(80,90]"),
  T8 = unique(sim_data$Age_group)
)

j <- lapply(states, 
            function(x) sim_data %>% 
              filter(Age_group %in% x)) %>% 
  bind_rows(.id="appear2") %>% 
  mutate(appear2 = parse_number(appear2))

jm <- j %>% 
  group_by(appear2, Age_group) %>% 
  group_means(Age, Hippocampus)

p <- ggplot(j, aes(x=Age, y=Hippocampus))  +
  geom_point(alpha=.2, size=.5, colour="grey40", aes(group=nn)) + 
  geom_errorbar(data=jm, inherit.aes = FALSE, 
                aes(x=Age_Mean, width=4,
                    ymin = Hippocampus_Mean - Hippocampus_SE,
                    ymax = Hippocampus_Mean + Hippocampus_SE)) +
  geom_point(data=jm, inherit.aes = FALSE, 
             aes(x=Age_Mean, y = Hippocampus_Mean, 
                 colour=Age_group)) +
  geom_line(data=jm, inherit.aes = FALSE, 
            aes(x=Age_Mean, y = Hippocampus_Mean))

p2 <- p +
  transition_time(appear2) +
  enter_grow()
p3 <- animate(p2, nframes = 700, end_pause = 50, start_pause = 35, duration = 90, width = 16, height = 6.5, units = "in", res=100)

anim_save(filename = "cohort.gif", path = "docs/anims")
