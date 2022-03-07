# HW1A
# Jeffrey Williams
# Date: Jan 15, 2022
# Prof. Robinson


# Step 2: Clearing Workspace
rm(list=ls())
gc()

# Step 3: Invoking the libraries
library(tidyverse)
library(usmap)

# Step 4: Accessing COVID CSV data and assigning the variables to represent data
url<-"https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
nytcovcounty <- read_csv(url)
url2<-"https://usafactsstatic.blob.core.windows.net/public/data/covid-19/ covid_county_population_usafacts.csv"
pop2019 <- read_csv("~/Downloads/covid_county_population_usafacts.csv")


# Step 5: Conversion of FIPS from a word to a number string
nytcovcounty$countyFIPS <- as.numeric(nytcovcounty$fips)

# Steps 6-12: Plotting US map with county boundaries to reflect data
plot_usmap(regions="counties", include=c("MD","VA","PA","WV","NC"),
           data=countypop, values="pop_2015")

# Step 13: Creating a new plot
plot_usmap(regions="counties", include=c("MD","VA","PA","WV","NC"),
           data=countypop, values="pop_2015") + theme(legend.position="right")

plot_usmap(regions="counties", include=c("MD","VA","PA","WV","NC"),
           data=countypop, values="pop_2015") + theme(legend.position="bottom") +
  scale_fill_continuous(name="Population", label=scales::comma)

# Step 16: Options for the legend position on the plot include the right, bottom,
# or top of the graph. You can also choose to not have one ("none"), or you
# can choose a two-element numeric vector.

# Pre-step 17: Example merging data frames
a <- c(10,20,30,40)
b <- c("y","n","n","y")
s <- c("P","Q","R","Q")
df1 <- data.frame(a,b,s)
View(df1)

s <- c("P","Q","R")
r <- c("Papa","Quebec","Romeo")
df2 <-data.frame(s,r)
View(df2)

df3 <- left_join(df1,df2,by=c("s"))
View(df3)
View(pop2019)


# Step 17: Merging data
mdata <- left_join(nytcovcounty,pop2019,by=c('countyFIPS'))

# Step 20: Determining COVID rate per thousand in each county
mdata$perThou <- 1000 * mdata$cases / mdata$population

# Step 21: Saving a slice of most recent COVID data
mostRecent <- mutate(mdata) %>% filter(mdata$date==max(mdata$date))

# Step 22: Creating map of state of choice (based on case numbers)
plot_usmap(regions="counties", include=c("VA", "MD"),
           data=nytcovcounty, values="cases") + 
  theme(legend.position="bottom") +
  scale_fill_continuous(name="Cases", label=scales::comma) +
  labs(title="COVID-19 Cases in Virginia")

# Step 23: Creating map of state of choice (based on death)
plot_usmap(regions="counties", include=c("VA", "MD"),
           data=nytcovcounty, values="deaths") + 
  theme(legend.position="bottom") +
  scale_fill_continuous(name="Deaths", label=scales::comma) +
  labs(title="COVID-19 Deaths in Virginia")

# Step 24: Creating map of state of choice (based on COVID rate per thousand)
plot_usmap(regions="counties", include=c("VA", "MD"),
           data=nytcovcounty, values="cases") + 
  theme(legend.position="bottom") +
  scale_fill_continuous(name="Cases Per Thousand", label=scales::comma) +
  labs(title="COVID-19 Deaths in Virginia")

# DATA MUTATION: Mutating the data of state
VA <- mutate(mdata) %>% filter(State=="VA")
VA2 <- mutate(VA) %>% filter(VA$date==max(VA$date))

VA2 <- VA2[order(-VA2$cases),]
View(VA2)

# Step 26-27: Identify and list top 5 counties in Virginia
top5 <- c("Fairfax", "Prince William", "Virginia Beach city", "Chesterfield", "Loudoun")
View(VA)

# Step 28: Making a copy of original data
VA4 <- VA
View(VA4)

# Step 29: Getting rid of rows and reducing data
VA4$deaths = NULL
VA4$perThou = NULL
VA4$abbr = NULL
VA4$population = NULL
VA4$state = NULL
VA4$fips = NULL
VA4$countyFIPS = NULL
VA4$`County Name`= NULL
VA4$State = NULL

# Step 30: Reducing data
VA5 <- VA4 %>% filter(county %in% top5)
View(VA5)

# Steps 31-33: Creating first timeseries plot
VA5 %>% ggplot(aes(x=date, y=cases, color=county)) + geom_smooth() +
  labs(title="Total COVID-19 Cases in Top 5 
       Hot Spot Virginia Counties", x="month")

VA5$logs <- log(VA5$cases)

VA5 %>% ggplot(aes(x=date, y=cases, color=county)) + geom_smooth() +
  labs(title="Total COVID-19 Cases in Top 5 
       Hot Spot Virginia Counties", x="month")

# Certification: No lines of code have been copied/pasted from another source.
# All code has been written by hand.
