install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)

setwd("/Users/maboca/Desktop/trolleyconsent")

d <- read.csv("trolley_consent_2_data.csv")

#View(d)

names(d)
means1 <- grep("means_1", names(d))
means2 <- grep("meansConQ_1", names(d))
cons1 <- grep("cons_1", names(d)) #how do i get rid of all of the sidecons ones
cons1s <- grep("side", names(d[cons1]))
cons1 <- cons1[-c(cons1s)]
cons2 <- grep("consConQ_1", names(d))
cons2s <- grep("side", names(d[cons2]))
cons2 <- cons2[-c(cons2s)]
side1 <- grep("side_1", names(d))
side2 <- grep("sideConQ_1", names(d)) 
sidecons1 <- grep("sidecons_1", names(d))
sidecons2 <- grep("sideconsConQ_1", names(d))
time <- grep("Submit", names(d)) 

#View (d)
dp <- d[, c(9, time, means1, means2, cons1, cons2, side1, side2, sidecons1, sidecons2)] #what does 8 mean oh it means ID
#View(dp)
dp$totaltime <- rowSums(dp[,2:29], na.rm = TRUE)
#View(dp)
dp <- dp %>% select(-c(2:29)) %>% filter(dp$totaltime > 80) %>%#there seems to be an error here and i think it's because total time has no value  
  gather(question, response, -c(1,58), na.rm = TRUE) %>% ##what does the 57 mean here
  mutate(dv = case_when( #dv is a new set of data with these two conditions
    str_detect(question, "ConQ")~"consent",
    str_detect(question, "1")~"acceptability"
  ), 
  scenario = case_when(
    str_detect(question, "MSH")~"scenario1",
    str_detect(question, "NS")~"scenario2",
    str_detect(question, "MR")~"scenario3",
    str_detect(question, "MFT")~"scenario4",
    str_detect(question, "NP")~"scenario5",
    str_detect(question, "SS")~"scenario6",
    str_detect(question, "BB")~"scenario7",
  ),
  scenario = factor(scenario, levels = c("scenario1", "scenario2", "scenario3", ##what does factor mean again - these are distinct things
                                         "scenario4", "scenario5", "scenario6",
                                         "scenario7")),
  condition = case_when(
    str_detect(question, "sidecons")~"side effect consent",
    str_detect(question, "means")~"means",
    str_detect(question, "side")~"side effect", #this could be a problem
    str_detect(question, "cons")~"consent"
  ))

View(dp)

d.sum <- dp %>% group_by(dv, 
                         #scenario, 
                         condition) %>% #add scenario for grouping by scenario
  summarise(mean = mean(response, na.rm = TRUE), 
            n = length(response), 
            sd = sd(response),
            se = sd/(sqrt(n))
  )

fig1 <- d.sum %>% filter(dv != "consent") %>%  #these are all of the resuls of acceptability by scenario
  #filter(scenario == "scenario1" | scenario == "scenario2" | scenario == "scenario3"
   #      | scenario == "scenario4" | scenario == "scenario5" | scenario == "scenario6" | scenario == "scenario7") %>%
  ggplot(aes(y = mean, x = condition, fill = condition)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = "dodge") +
  #facet_wrap(~scenario) +
  theme_bw()

#t.test(dp$response[dp$scenario == "scenario1" & dp$condition == "means" & dp$dv == "acceptability"], 
      # dp$response[dp$scenario == "scenario1" & dp$condition == "side effect" & dp$dv == "acceptability"])

fig2 <- d.sum %>% filter(dv == "consent") %>% 
  ggplot(aes(y = mean, x = condition, fill = condition)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = "dodge") +
  #facet_wrap(~scenario) +
  theme_bw()

dpw <- dp %>% select(-question) %>% 
  #filter(condition != "side effect") %>% 
  spread(dv, response)
View(dpw)

fig3 <- dpw %>% #filter(dv == "consent") %>% 
  ggplot(aes(y = acceptability, x = consent, color = scenario)) + 
  geom_point(stat = "identity") +
  geom_smooth(method = lm, alpha = 0.1) + 
  #facet_wrap(~scenario) + #potentially comment this line out
  theme_bw()

cor.test(x = dpw$consent[dpw$scenario == "scenario1"], y = dpw$acceptability[dpw$scenario == "scenario1"])
cor.test(x = dpw$consent[dpw$scenario == "scenario2"], y = dpw$acceptability[dpw$scenario == "scenario2"])
cor.test(x = dpw$consent[dpw$scenario == "scenario3"], y = dpw$acceptability[dpw$scenario == "scenario3"])
cor.test(x = dpw$consent[dpw$scenario == "scenario4"], y = dpw$acceptability[dpw$scenario == "scenario4"])
cor.test(x = dpw$consent[dpw$scenario == "scenario5"], y = dpw$acceptability[dpw$scenario == "scenario5"])
cor.test(x = dpw$consent[dpw$scenario == "scenario6"], y = dpw$acceptability[dpw$scenario == "scenario6"])
cor.test(x = dpw$consent[dpw$scenario == "scenario7"], y = dpw$acceptability[dpw$scenario == "scenario7"])


d.overall <- dp %>% group_by(dv, condition) %>% 
  summarise(mean = mean(response))

View(d.sum)
