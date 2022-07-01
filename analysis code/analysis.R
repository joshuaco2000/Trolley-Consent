#install.packages("tidyverse")
library(tidyverse)


setwd("/Users/maboca/Desktop/trolleyconsent")

d <- read.csv("trolley_consent_officialdata.csv")

View(d)

names(d)
time <- grep("Submit", names(d))
means1 <- grep("means_1", names(d))
means2 <- grep("means_2", names(d))
cons1 <- grep("cons_1", names(d))
cons2 <- grep("cons_2", names(d))
side1 <- grep("side_1", names(d))
dp <- d[, c(8, time, means1, means2, cons1, cons2, side1)]
dp$totaltime <- rowSums(dp[,2:12])
dp <- dp %>% select(-c(2:12)) %>% filter(totaltime > 120) %>% 
  gather(question, response, -c(1, 57), na.rm = TRUE) %>%
  mutate(dv = case_when(
    str_detect(question, "1")~"acceptability", 
    str_detect(question, "2")~"consent"
  ), 
  scenario = case_when(
    str_detect(question, "MSH")~"scenario1",
    str_detect(question, "NS")~"scenario2",
    str_detect(question, "MR")~"scenario3",
    str_detect(question, "BW")~"scenario4",
    str_detect(question, "MFT")~"scenario5",
    str_detect(question, "NP")~"scenario6",
    str_detect(question, "SS")~"scenario7",
    str_detect(question, "MSU")~"scenario8",
    str_detect(question, "CBM")~"scenario9",
    str_detect(question, "BB")~"scenario10",
    str_detect(question, "VT")~"scenario11"
  ),
  scenario = factor(scenario, levels = c("scenario1", "scenario2", "scenario3",
                                         "scenario4", "scenario5", "scenario6",
                                         "scenario7", "scenario8", "scenario9",
                                         "scenario10", "scenario11")),
  condition = case_when(
    str_detect(question, "cons")~"consent",
    str_detect(question, "means")~"means",
    str_detect(question, "side")~"side effect"
  ))

d.sum <- dp %>% group_by(dv,
                        # scenario, 
                         condition) %>% #add scenario for grouping by scenario
  summarise(mean = mean(response), 
            n = length(response), 
            sd = sd(response),
            se = sd/(sqrt(n))
  )

fig1 <- d.sum %>% filter(dv != "consent") %>%  #these are all of the results 
#  filter(scenario == "scenario2" | scenario == "scenario3" | scenario == "scenario5"
 #        | scenario == "scenario6" | scenario == "scenario10") %>%
  ggplot(aes(y = mean, x = condition, fill = condition)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = "dodge") +
  #facet_wrap(~scenario) +
  theme_bw()

d.sum4 <- dp %>% filter(scenario == "scenario1" | scenario == "scenario2" | scenario == "scenario3" | scenario == "scenario4" | scenario == "scenario5"
                        | scenario == "scenario6" | scenario == "scenario7" | scenario == "scenario10") %>%
                    group_by(dv,
                         #scenario, 
                         condition) %>% #add scenario for grouping by scenario
  summarise(mean = mean(response), 
            n = length(response), 
            sd = sd(response),
            se = sd/(sqrt(n))
  )

fig4 <- d.sum4 %>% filter(dv != "consent") %>%  #this is the section with only the scenarios that replicated means vs side effect
  ggplot(aes(y = mean, x = condition, fill = condition)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = "dodge") +
  #facet_wrap(~scenario) +
  theme_bw()

t.test(dp$response[dp$scenario == "scenario10" & dp$condition == "consent" & dp$dv == "acceptability"], 
       dp$response[dp$scenario == "scenario10" & dp$condition == "side effect" & dp$dv == "acceptability"])

fig2 <- d.sum %>% filter(dv == "consent") %>% 
  ggplot(aes(y = mean, x = condition, fill = condition)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = "dodge") +
  #facet_wrap(~scenario) +
  theme_bw()

dpw <- dp %>% select(-question) %>% filter(condition != "side effect") %>% spread(dv, response)
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
cor.test(x = dpw$consent[dpw$scenario == "scenario8"], y = dpw$acceptability[dpw$scenario == "scenario8"])
cor.test(x = dpw$consent[dpw$scenario == "scenario9"], y = dpw$acceptability[dpw$scenario == "scenario9"])
cor.test(x = dpw$consent[dpw$scenario == "scenario10"], y = dpw$acceptability[dpw$scenario == "scenario10"])
cor.test(x = dpw$consent[dpw$scenario == "scenario11"], y = dpw$acceptability[dpw$scenario == "scenario11"])
#we found a significant correlation in 10/11 scenarios ranging from (p= .... to ... and r = .... to ...)

d.overall <- dp %>% group_by(dv, condition) %>% 
  summarise(mean = mean(response))

View(d.sum)
