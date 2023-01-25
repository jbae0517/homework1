## title: "Homework Assignment 1"
## author: "Jennifer Bae"
## date: "2023-01-20"



if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales)

#load dataset and set workspace 
full.ma.data <- readRDS("/Users/jenniferbae/Downloads/ECON 470 R Coding/homework1/data/output/full_ma_data.rds")
contract.services.area <- readRDS("/Users/jenniferbae/Downloads/ECON 470 R Coding/homework1/data/output/contract_service_area.rds")
ma.penetration.data <- readRDS("/Users/jenniferbae/Downloads/ECON 470 R Coding/homework1/data/output/ma_penetration.rds")
plan.premiums <- readRDS("/Users/jenniferbae/Downloads/ECON 470 R Coding/homework1/data/output/plan_premiums.rds")


# 1. How many observations exist in the current dataset?

tot.obs <- as.numeric(count(full.ma.data %>% ungroup()))
tot.obs

# 2. How many different plan_types are there?

unique_type <- unique(full.ma.data$plan_type)
unique_type

# 3. Provide a table of the count of plans under each plan type in each year. 

library(knitr)

plan.type.year1 <- full.ma.data %>%
  group_by(plan_type, year) %>%
  count() %>%
  arrange (year, -n) %>%
  filter (plan_type!= "NA")
plan.type.year1 <- pivot_wider(plan.type.year1, names_from = "year", values_from = "n", names_prefix = "Count_")
view(plan.type.year1)



# 4. Remove all special needs plan (SNP), employer group plans (eghp), and all "800-series" plans. 

final.plans <- full.ma.data %>%
  filter(snp == "No" & eghp == "No" & (planid <800 | planid >= 900))

plan.type.year2 <- final.plans %>% 
  group_by(plan_type, year) %>%
  count () %>%
  arrange (year, -n)
plan.type.year2 <- pivot_wider(plan.type.year2, names_from = "year", values_from = "n", names_prefix = "Count_")
view(plan.type.year2)

 

# 5. Provide a graph showing the average number of Medicare Advantage enrollees per county from 2008 to 2015. 

library(ggplot2)



final.data <- full.ma.data %>%
  inner_join(contract.services.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!is.na(avg_enrollment))



fig.avg.enrollment <- final.data %>%
  group_by(fips, year) %>%
  select(fips, year, avg_enrollment) %>%
  summarize(all_enroll = sum(avg_enrollment)) %>%
  ggplot(aes(x=as.factor(year), y = all_enroll)) +


  stat_summary(fun.y="mean", geom = "bar") +
  labs(
    x= "Year",
    y= "People",
    title =""
    ) +scale_y_continuous(labels=comma) +
  theme_bw()

fig.avg.enrollment





save.image("Hwk1_workspace.Rdata")


