#install.packages("tidyverse")
#install.packages("readstata13")
library(dplyr)
library(readstata13)

# Fowler (2016), QJPS, "What Explains Incumbent Success?":
# The paper and the replication data can be found under: https://www.nowpublishers.com/article/Details/QJPS-15108
# The replication data is stored in a zip file under Supplementary Information.
# We now have the .dta file already on Github (so no need to download it from the journal's site).
mydata <- readstata13::read.dta13("./data.dta")

# Have a look at the data:
mydata %>% View()

# Mutate: Generate a new variable and store it in mydata:
mydata <- mydata %>% dplyr::mutate(percvote = voteshare * 100)
# Check if the new variable "percvote" is now there:
names(mydata)

# Select: Select certain variables in your data:
mydata %>% dplyr::select(percvote)
# Use as_tibble() or head() to get a more compact view of the selected data:
mydata %>% tibble::as_tibble() %>% dplyr::select(percvote)
mydata %>% dplyr::select(percvote) %>% head()
# Or a range of variables:
mydata %>% tibble::as_tibble() %>% dplyr::select(setting:year)

# Filter: Filter out observations of a certain value in your data:
# For example, get all percentage voteshares above 50%:
mydata %>% dplyr::filter(percvote > 50)
# We could now also generate a subset of mydata with observations, which all fulfill this criterium
# That is in this data: A subset with Democratic candidates who received above 50% of the votes in US elections:
topcan <- mydata %>% dplyr::filter(percvote > 50)
# We still have all variables from mydata in the new subset "topcan", but reduced number of observations:
names(topcan)
# Another example: Get all observations from senate and gubernatorial elections:
mydata %>% dplyr::filter(setting %in% c("gov", "senate")) 

# Summarise: Aggregate/summarise your data:
# For example, counting the number of observations in topcan:
topcan %>% dplyr::summarise(n())
# Use group_by to count the observations within a specific group of topcan
# For example within each type of election:
topcan %>% dplyr::group_by(setting) %>% dplyr::summarise(obs = n())
# Or calculate the mean voteshare within each type of election:
topcan %>% dplyr::group_by(setting) %>% dplyr::summarise(mean(percvote))

# Arrange: Sorts your data:
# For example, alphabetically for elections, then for states and then descending in years within states:
topcan <- topcan %>% dplyr::arrange(setting, state, -year)
# Have another look at the data, now sorted:
topcan %>% View()

# Some more generic examples:
c(1,2,3) %>% .[1]
seq(1,9) %>% matrix(., ncol = 3, byrow = T)
seq(1,9) %>% matrix(., ncol = 3, byrow = T) %>% .[2, ]

# Exercises:
# [1] Generate a subset containing only US gubernatorial elections with narrow election outcomes (i.e. a 3% winning margin in this exercise).
# Find out how many observations it contains and check the minimum and maximum level of candidate voteshares in this subset.
gubdata <- mydata %>% dplyr::filter(setting %in% c("gov"), percvote < 53 & percvote > 47) 
gubdata %>% dplyr::summarise(n())
gubdata %>% dplyr::summarise(max(voteshare))
gubdata %>% dplyr::summarise(min(voteshare))

# [2] Within this subset, generate a dummy variable for states, which were historically members of the Confederacy (check or scrape them from Wikipedia if you are not sure).
# Calculate the average vote share of Democratic candidates within those states. 
# Calculate the share of elections where a Democratic incumbent runs (mydata$incumbency = 1) in the Confederate states
# vs the share of elections where a Republican incumbent runs (mydata$incumbency = -1).
# install.packages("stringr")
# install.packages("rvest")
library(stringr)
library(rvest)
url <- read_html("https://en.wikipedia.org/wiki/Confederate_States_of_America")
states <- rvest::html_nodes(url, "tr:nth-child(40) td")
states <- rvest::html_text(states)
states <- unlist(stringr::str_split(states, "\n"))[4:17]
states <- stringr::str_trim(states)
states <- matrix(cbind(states,rep("", 14)), nrow = 14, ncol = 2)
states[,2] <- c("AL", "AZ", "AR", "FL", "GA", "LA", "MS", "NM", "NC", "SC", "TN", "TX", "VA", "WV")
gubdata <- gubdata %>% dplyr::mutate(confstate = ifelse(state %in% states[, 2], 1, 0))
gubdata %>% dplyr::summarise(mean(voteshare))
denom <- gubdata %>% dplyr::group_by(confstate) %>% dplyr::summarise(n()) %>% .[2, 2]
numD <- gubdata %>% dplyr::filter(confstate == 1) %>% dplyr::group_by(incumbency) %>% dplyr::summarise(n()) %>% .[3, 2]
numR <- gubdata %>% dplyr::filter(confstate == 1) %>% dplyr::group_by(incumbency) %>% dplyr::summarise(n()) %>% .[1, 2]
# Share of Democratic incumbents running in Confederate states:
numD/denom
# Share of Republican incumbents running in Confederate states:
numR/denom

# [3] Sort the subset such that it first lists within Confederate states the states alphabetically, increasing in election years,
# and then within non-Confederate states. Are there some gubernatorial elections with more than one Democratic candidate? Find out.
gubdata <- gubdata %>% dplyr::arrange(-confstate, state, year)

# [4] Within the subset, are there states where the Democratic party is always (i.e. in all years) incumbent? If yes, which ones?
gubdata %>% dplyr::filter(confstate == 1) %>% dplyr::group_by(state, incumbency) %>% dplyr::summarise(n()) %>% View()
# None of the Confederate states has only Democratic incumbents running in all years.

# [5] Within the subset, generate a variable that stores the previous electoral result for the Democratic party in each state. Tip: use the lag() function within mutate().
gubdata <- gubdata %>% dplyr::group_by(state) %>% dplyr::mutate(prevvs = lag(voteshare))

# [6*] For more advanced students - a less dplyr-oriented exercise: Can you find an incumbency advantage for gubernatorial elections?
# Fowler (2016) does this first in a very simple manner by running a linear regression of the Democratic candidate
# voteshare on incumbency status.
pooling <- lm(voteshare ~ incumbency, data = gubdata)
summary(pooling)
# In a second step, he controls for state-decade effects.
#install.packages("plm")
library(plm)
pdata <- pdata.frame(gubdata, index = c("setting_unit_decade"))
fereg <- plm::plm(voteshare ~ incumbency, data = pdata, model = "within")
summary(fereg)
# At last, he estimates the local average treatment effect of incumbency status on the Democratic voteshare in a
# regression discontinuity design (during reading week, on Mon, 5 Nov, we will have an entire session on running an RD design).
# Install RD package by MatthieuStigler (2014) following Lee (2008):
#install.packages("devtools")
library(devtools)
#install_github(repo = "RDDtools", username = "MatthieuStigler", subdir = "RDDtools")
library(RDDtools)

