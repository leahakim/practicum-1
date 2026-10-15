#Topic: Practicum 1 
#Author: Leah Kim 
#Date: 10/8/2026
#Output: Replicate Figure 1 and Figure 2

#Load all libraries 
library(tidyverse)
library(haven)
library(lattice)
library(ggplot2)
library(grid)
library(srvyr)

#Set working directory and load data 
setwd('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Assignments /Practicum 1')
cces <- read_dta('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/cumulative_2006-2024.dta')

#Select the variables needed 
cces$year #year variable 
cces$pid7 #strong, weak, and leaners 
cces$vv_turnout_gvm #turnout for general election 

unique(cces_sub$year)
#Make new dataset sub for all variables needed 
cces_sub <- cces |> select(year, pid7, vv_turnout_gvm, weight)

table(cces$vv_turnout_gvm)
#Figure 1 
cces_sub$pid7 #strong, weak, and leaners 
#Manipulate the data 
#1. Drop the Don't Know's 
cces_sub <- cces_sub %>%
  mutate(pid7 = if_else(pid7 %in% 8:9, NA_real_, pid7))
#Now proportions for the rest of the identifiers 
cces_sub <- cces_sub %>%
  mutate(
    strong = case_when(
      pid7 %in% c(1,7) ~ 1, 
      pid7 %in% 2:6 ~ 0,
    ),
    weak= case_when(
      pid7 %in% c(2,6) ~ 1, 
      pid7 %in% c(1, 3, 4, 5, 7) ~ 0,
    ),
    leaners = case_when(
      pid7 %in% c(3,5) ~ 1, 
      pid7 %in% c(1, 2, 4, 6, 7) ~ 0, 
    ), 
    indeps = case_when(
      pid7 == 4 ~ 1, 
      pid7 %in% c(1, 2, 3, 5, 6, 7) ~ 0,
    )
  )

#For Figure 2, need to combine voting turnout variables so it's 1 and 0
cces_sub <- cces_sub |>
  mutate(voted = case_when(
    vv_turnout_gvm == 1 ~ 1, 
    vv_turnout_gvm %in% 2:3 ~ 0
  ))

#Declare the survey and collapse the data 
cces_svy <- cces_sub %>%
  mutate(identifiers = case_when(
    pid7 %in% c(1, 2, 6, 7) ~ 1, 
    pid7 %in% c(3, 4, 5) ~ 0
  )) %>% 
  as_survey_design(weights = weight)

#now for figure 1, with the survey-weighted means 
figure1 <- cces_svy %>% 
  group_by(year) %>%
  summarise(
    strong = survey_mean(strong, na.rm = TRUE, vartype = NULL),
    weak = survey_mean(weak, na.rm = TRUE, vartype = NULL),
    leaners = survey_mean(leaners, na.rm = TRUE, vartype = NULL),
    indeps = survey_mean(indeps, na.rm = TRUE, vartype = NULL)
  )

#now for figure 2
figure2 <- cces_svy %>%
  filter(!is.na(voted)) %>%
  group_by(year, voted) %>%
  summarise(
    identifiers = survey_mean(identifiers, na.rm = TRUE, vartype = NULL)
  )

#now it's time to graph everything 
#Figure 1(a)
plot(x=0, type = "n", 
     xlim = c(2006, 2024), xaxt = "n", 
     ylim = c(0, 0.6), yaxt = "n", 
     xlab = "", ylab = "", 
     main = "Bartels 1a")
axis(1, at = seq(2006, 2024, 2))
axis(2, at = seq(0, 0.6, 0.1))

lines(figure1$year, figure1$strong, type = "o", col= "black", lty = 1, pch=16)
lines(figure1$year, figure1$weak, type = "o", col="black", lty=2, pch=1 )

legend("topright", legend = c("Strong", "Weak"), 
       lty = c(1,2), pch = c(16, 1), bty = "n")

#Figure 1(b)
plot(x=0, type = "n", 
     xlim = c(2006, 2024), xaxt = "n", 
     ylim = c(0, 0.6), yaxt = "n", 
     xlab = "", ylab = "", 
     main = "Bartels 1b")
axis(1, at = seq(2006, 2024, 2))
axis(2, at = seq(0, 0.6, 0.1))

lines(figure1$year, figure1$indeps, type = "o", col= "black", lty = 1, pch=16)
lines(figure1$year, figure1$leaners, type = "o", col="black", lty=2, pch=1 )

legend("topright", legend = c("Pure Independents", "Leaners"), 
       lty = c(1,2), pch = c(16, 1), bty = "n")

#Figure 2: Voters vs. nonvoters 
voters <- subset(figure2, voted == 1)
nonvoters <- subset(figure2, voted == 0)

plot(x=0, type="n",
     xlim= c(2006, 2024), xaxt = "n", 
     ylim = c(0, 1), yaxt = "n", 
     xlab = "", ylab = "", 
     main = "Bartels 2")
axis(1, at = seq(2006, 2024, 2))
axis(2, at = seq(0, 1, 0.1))

lines(voters$year, voters$identifiers, type="o", col="black",lty=1,pch=16)
lines(nonvoters$year, nonvoters$identifiers, type="o", col="black", lty=2, pch=1)

legend("topleft", legend = c("Voters", "Nonvoters"),
       lty = c(1,2), pch = c(16, 1), bty = "n")