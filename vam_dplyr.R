install.packages("tidyverse")
install.packages("readstata13")
library(dplyr)
library(readstata13)

# Fowler (2016), QJPS, "What Explains Incumbent Success?":
# The paper and the replication data can be found under: https://www.nowpublishers.com/article/Details/QJPS-15108
# The replication data is stored in a zip file under Supplementary Information.
# Download the zip file and open it. 
# Open a new R project (in RStudio: File>New Project...>Existing Directory, choose the folder you saved the unzipped folder in).
# Within your R project, open a new R script, save it under a name (e.g. "vam1") and copy-paste this code into it.
mydata <- read.dta13("./qjps_15108_supp/data.dta")

# Have a look at the data:
mydata %>% View()

# Mutate: Generate a new variable and store it in mydata:
mydata <- mydata %>% mutate(percvote = voteshare*100)
# Check if the new variable "percvote" is now there:
names(mydata)

# Select: Select certain variables in your data:
mydata %>% select(percvote)
# Use as_tibble() or head() to get a more compact view of the selected data:
mydata %>% as_tibble() %>% select(percvote)
mydata %>% select(percvote) %>% head()
# Or a range of variables:
mydata %>% as_tibble() %>% select(setting:year)

# Filter: Filter out observations of a certain value in your data:
# For example, get all percentage voteshares above 50%:
mydata %>% filter(percvote > 50)
# We could now also generate a subset of mydata with observations, which all fulfill this criterium
# That is in this data: A subset with Democratic candidates who received above 50% of the votes in US elections:
topcan <- mydata %>% filter(percvote > 50)
# We still have all variables from mydata in the new subset "topcan", but reduced number of observations:
names(topcan)

# Summarise: Aggregate/summarise your data:
# For example, counting the number of observations in topcan:
topcan %>% summarise(n())
# Use group_by to count the observations within a specific group of topcan
# For example within each type of election:
topcan %>% group_by(setting) %>% summarise(obs=n())
# Or calculate the mean voteshare within each type of election:
topcan %>% group_by(setting) %>% summarise(mean(percvote))

# Arrange: Sorts your data:
# For example, alphabetically for elections, then for states and then descending in years within states:
topcan <- topcan %>% arrange(setting, state, -year)
# Have another look at the data, now sorted:
topcan %>% View()

# More generic examples:
# %>% is a pipe
# %in%
# df %<>% filter(...)
c(1,2,3) %>% .[1]
seq(1,9) %>% matrix(.,ncol=3, byrow=T) %>% .[,1]
subset()

# Exercises:
# [1] Generate a subset containing only US gubernatorial elections with narrow election outcomes (i.e. a 3% winning margin in this exercise).
# Find out how many observations it contains and check the minimum and maximum level of candidate voteshares in this subset.

# [2] Within this subset, generate a dummy variable for states, which were historically members of the Confederacy (check Wikipedia if you are not sure).
# Calculate the average vote share of Democratic candidates within those states. 
# Calculate the share of elections where a Democratic incumbent runs (mydata$incumbency = 1) in the Confederate states
# vs the share of elections where a Republican incumbent runs (mydata$incumbency = -1).

# [3] Sort the subset such that it first lists within Confederate states the states alphabetically, increasing in election years,
# and then within non-Confederate states. Are there some gubernatorial elections with more than one Democratic candidate? Find out.

# [4] Within the subset, are there states where the Democratic party is always (i.e. in all years) incumbent? If yes, which ones?

# [5*] For more advanced students: Can you find an incumbency advantage for gubernatorial elections?
# Fowler (2016) does this first in a very simple manner by running a linear regression of the Democratic candidate
# voteshare on incumbency status.
# In a second step, he controls for state-decade effects.
# At last, he estimates the local average treatment effect of incumbency status on the Democratic voteshare in a
# regression discontinuity design.

