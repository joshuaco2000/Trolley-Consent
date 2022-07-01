#install.packages("tidyverse")
#install.packages("dplyr")
library(tidyverse)

setwd("/Users/maboca/Desktop/trolleyconsent")

d <- read.csv("trolley_consent_officialdata.csv")
View(d)

#Data Cleaning
time <- grep("Submit", names(d)) #storing column names with time information
means1 <- grep("means_1", names(d)) #storing column names with acceptability values for means cases
means2 <- grep("means_2", names(d)) #storing column names with perceived consent values for means cases
cons1 <- grep("cons_1", names(d)) #storing column names with acceptability values for consensual means cases
cons2 <- grep("cons_2", names(d)) #storing column names with perceived consent values for consensual means cases
side1 <- grep("side_1", names(d)) #storign column names with acceptability values for side effect cases
dp <- d[, c(7, time, means1, means2, cons1, cons2, side1)] #filtering for relevant columns

#filter for minimum time being 120 seconds
dp$totaltime <- rowSums(dp[,2:12]) #sums up time taken on each page to create a total time value for each participant
dp <- dp %>% select(-c(2:12)) %>% #gets rid of individual page times after creating the sum times under totaltime column
  filter(totaltime > 120) %>% #removing participants that took less than 120 seconds
  
  #sort data by scenario + which response is which
  gather(question, response, -c(1, 57), na.rm = TRUE)%>% #reorganizes data into question and response columns
  mutate(dv = case_when(
    str_detect(question, "1")~"acceptability", #detects which responses are acceptability responses
    str_detect(question, "2")~"consent" #detects which responses are consent responses
  ), 
  #detect scenario by naming conventions in data
  scenario = case_when(
    str_detect(question, "MSH")~"scenario1", #Mine Shaft = Scenario 1
    str_detect(question, "NS")~"scenario2", #Night Shift = Scenario 2
    str_detect(question, "MR")~"scenario3", #Modified Rowboat = Scenario 3
    str_detect(question, "BW")~"scenario4", #Bike Week = Scenario 4
    str_detect(question, "MFT")~"scenario5", #Modified Footbridge/Trolley = Scenario 5
    str_detect(question, "NP")~"scenario6", #Nuclear Reactor = Scenario 6
    str_detect(question, "SS")~"scenario7", #Space Station = Scenario 7
    str_detect(question, "MSU")~"scenario8", #Modified Submarine = Scenario 8
    str_detect(question, "CBM")~"scenario9", #Crying Baby Modified = Scenario 9
    str_detect(question, "BB")~"scenario10", #Burning Building = Scenario 10
    str_detect(question, "VT")~"scenario11" #Vaccine Test = Scenario 11
  ),
  scenario = factor(scenario, levels = c("scenario1", "scenario2", "scenario3", #re-orders scenarios so they appear in order
                                         "scenario4", "scenario5", "scenario6",
                                         "scenario7", "scenario8", "scenario9",
                                         "scenario10", "scenario11")),
  #detect which condition is which
  condition = case_when(
    str_detect(question, "cons")~"consent", #cons abbreviation in column name indicates consensual means condition
    str_detect(question, "means")~"means", #means abbreviation in column name indicates means condition
    str_detect(question, "side")~"side effect" #side abbreviation in column name indicates side effect condition
  ))

#FIGURE 1 = acceptability ratings across all scenarios
d.sum <- dp %>% group_by(dv,condition) %>% #group data by condition
  summarise(mean = mean(response), #mean response across participants will be graphed  
            n = length(response), 
            sd = sd(response),
            se = sd/(sqrt(n))
  )
fig1 <- d.sum %>% filter(dv != "consent") %>% #dependent variable is acceptability (not consent)
  ggplot(aes(y = mean, x = condition, fill = condition)) + #bar graph with mean acceptability response by condition
  geom_bar(stat = "identity", position = "dodge") +  #so bars are graphed next to each other instead of superimposed
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = "dodge") + #error bar is within standard error
  theme_bw()

# #FIGURE 4 = figure 1 but only the scenarios that replicated means vs side effect
# #d.sum4 <- dp %>% filter(scenario == "scenario1" | scenario == "scenario2" | scenario == "scenario3" | scenario == "scenario4" | scenario == "scenario5"
#  #                       | scenario == "scenario6" | scenario == "scenario7" | scenario == "scenario10") %>% #isolate the scenarios where the means vs side effect distinction was replicated
#   #group_by(dv, condition) %>% #group acceptability responses by condition
#   #summarise(mean = mean(response), 
#    #         n = length(response), 
#             sd = sd(response),
#             se = sd/(sqrt(n))
#   )
# fig4 <- d.sum4 %>% filter(dv != "consent") %>%  #dependent variable is acceptability response (not consent)
#   ggplot(aes(y = mean, x = condition, fill = condition)) + #bar graph with mean acceptability response across all scenarios on y axis
#   geom_bar(stat = "identity", position = "dodge") +  
#   geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = "dodge") + 
#   theme_bw()

#this is to test whether the difference in acceptability responses between the consensual means and side effect conditions is statistically significant
t.test(dp$response[dp$scenario == "scenario10" & dp$condition == "consent" & dp$dv == "acceptability"], 
       dp$response[dp$scenario == "scenario10" & dp$condition == "side effect" & dp$dv == "acceptability"])
#go back and copy and paste this
#FIGURE 2 = shows whether consent was properly manipulated for each scenario
fig2 <- d.sum %>% filter(dv == "consent") %>% #filter for the consent response
  ggplot(aes(y = mean, x = condition, fill = condition)) + #bar graph with average consent response on y axis
  geom_bar(stat = "identity", position = "dodge") +  
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = "dodge") + 
  theme_bw()

#FIGURE 3 = correlational graph between consent and acceptability
dpw <- dp %>% select(-question) %>% filter(condition != "side effect") %>% spread(dv, response) #filter out side effect condition because a perceived consent response was not collected for it 

fig3 <- dpw %>% #filter(dv == "consent") %>% 
  ggplot(aes(y = acceptability, x = consent, color = scenario)) +  #line graph with average acceptability response on y axis
  geom_point(stat = "identity") +
  geom_smooth(method = lm, alpha = 0.1) + 
  theme_bw()

#CORRELATION TESTS for each scenario to measure significance and strength of correlation between consent and acceptability
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

d.overall <- dp %>% group_by(dv, condition) %>% 
  summarise(mean = mean(response))

###########################################################################################
############################STUDY 2 ANALYSIS###############################################

setwd("/Users/maboca/Desktop/trolleyconsent")

d <- read.csv("trolley_consent_2_data.csv")

names(d)
means1 <- grep("means_1", names(d)) #storing column names with means acceptability response
means2 <- grep("meansConQ_1", names(d)) #storing column names with means perceived consent response
cons1 <- grep("cons_1", names(d)) #storing column names with consensual acceptability response
cons1s <- grep("side", names(d[cons1])) #stores column names with consensual side effect acceptability responses
cons1 <- cons1[-c(cons1s)] #isolates consensual means acceptability responses from consensual side effect acceptability responses
cons2 <- grep("consConQ_1", names(d)) #stores column names with consensual perceived consent responses
cons2s <- grep("side", names(d[cons2])) #stores column names with consensual side effect consent responses
cons2 <- cons2[-c(cons2s)] #isolates consensual means consent responses from consensual side effect consent responses
side1 <- grep("side_1", names(d))
side2 <- grep("sideConQ_1", names(d)) 
sidecons1 <- grep("sidecons_1", names(d))
sidecons2 <- grep("sideconsConQ_1", names(d))
time <- grep("Submit", names(d)) 

dp <- d[, c(9, time, means1, means2, cons1, cons2, side1, side2, sidecons1, sidecons2)] 
dp$totaltime <- rowSums(dp[,2:29], na.rm = TRUE)
dp <- dp %>% select(-c(2:29)) %>% filter(dp$totaltime > 80) %>%
  gather(question, response, -c(1,58), na.rm = TRUE) %>% 
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
  scenario = factor(scenario, levels = c("scenario1", "scenario2", "scenario3",
                                         "scenario4", "scenario5", "scenario6",
                                         "scenario7")),
  condition = case_when(
    str_detect(question, "sidecons")~"side effect consent",
    str_detect(question, "means")~"means",
    str_detect(question, "side")~"side effect",
    str_detect(question, "cons")~"consent"
  ))

View(dp)

#FIGURE 1 = acceptability ratings across scenarios
d.sum <- dp %>% group_by(dv, 
                         condition) %>% 
  summarise(mean = mean(response, na.rm = TRUE), 
            n = length(response), 
            sd = sd(response),
            se = sd/(sqrt(n))
  )

fig1 <- d.sum %>% filter(dv != "consent") %>%  #these are all of the resuls of acceptability by scenario
  ggplot(aes(y = mean, x = condition, fill = condition)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = "dodge") +
  theme_bw()

#t.test(dp$response[dp$scenario == "scenario1" & dp$condition == "means" & dp$dv == "acceptability"], 
# dp$response[dp$scenario == "scenario1" & dp$condition == "side effect" & dp$dv == "acceptability"])

#FIGURE 2 = shows whether consent was properly manipulated for each scenario
fig2 <- d.sum %>% filter(dv == "consent") %>% 
  ggplot(aes(y = mean, x = condition, fill = condition)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = "dodge") +
  theme_bw()

#FIGURE 3 = correlational graph between consent and acceptability
dpw <- dp %>% select(-question) %>% 
  #filter(condition != "side effect") %>% 
  spread(dv, response)

fig3 <- dpw %>% #filter(dv == "consent") %>% 
  ggplot(aes(y = acceptability, x = consent, color = scenario)) + 
  geom_point(stat = "identity") +
  geom_smooth(method = lm, alpha = 0.1) + 
  theme_bw()

#CORRELATION TESTS for each scenario to measure significance
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