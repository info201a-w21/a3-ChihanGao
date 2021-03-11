# Jail Admissions / Discharges Total jail admissions is an estimate of the number of admissions in each jurisdiction or county in a given year. 

# Introduction

# Disaggregation is the topic we focus in this class. We read and study Data Feminism 
# and we try to prevent inequality from happening in the real world. In this practice,
# we will primarily focus on population overtime, prison population of black and white
# citizens in the US, and prison administration over time. Our intention is to find if
# there is inequality exist that treats African American different when it comes to send
# he/she to the jail, while African American takes more part in the population of the United
# States of America.
# Though it will be good to focus on all the states and compare their stats, we will only focus on 
# 5 states for the first part. To find these five states, we calculated the top 5 states with the 
# most population change from 1970 and up to 2018. The states we got are: California, North Carolina
# Georgia, Florida, and Texas. The state with the most population change is California.
# After finishing calculating the top five states, 


library("tidyverse")
library("dplyr")
library("gridExtra")
library("ggplot2")
library("ggmap")
library("mapdata")
library("countrycode")

incarceration <- read.csv("incarceration_trends.csv")

# year, state, total_pop_15to64, white_pop_15to64,black_pop_15to64,
# total_prison_pop, black_prison_pop, white_prison_pop,
# total_prison_adm,  black_prison_adm, white_prison_adm.

# extract rows and columns contain only about population and prison pop
pop_and_prison <- select(incarceration,year, state, county_name, total_pop_15to64, black_pop_15to64, white_pop_15to64, total_prison_pop,  black_prison_pop,  white_prison_pop,total_prison_adm, black_prison_adm,white_prison_adm)

# pop_and_prison is the data we select for analyze, though it looks like we have
# a lot of variables but actually there are only major kinds: year, state, pop_15to64, prison_pop, and prison_adm.

# Next we will be calculating values that are related to these variables including:
# which are the top five states that have the largest population growth since 1970?

# First, we group our state population by year and sum up all the population for each county to represent the 
# state total population, and we do that for each year. Using "state_pop_overtime"  to find the state total pop

# (Note that we use pop from 15 to 64 for our data instead of total pop, as illustrated in the article:
# youth under age 15 and adults over 64 are age groups at very low risk of jail incarceration)

state_pop_overtime <- pop_and_prison %>%
  group_by(year,state) %>%
  summarise(state_total = sum(total_pop_15to64, na.rm = TRUE))

# Next, we want to find the change of population, we will use the most recent number to 
# subtract the oldest data, use "pop_difference"

oldest <- state_pop_overtime %>%
  filter(year == 1970) %>%
  rename(year_1970 = year, state_total_1970 = state_total)

recent<- state_pop_overtime %>%
  filter(year == 2018) %>%
  rename(year_2018 = year,state_total_2018 = state_total)

oldest_recent <- left_join(oldest, recent, by = "state")
  
pop_difference <- oldest_recent["state_total_2018"] - oldest_recent["state_total_1970"]
pop_difference <- rename(pop_difference, difference = state_total_2018)

oldest_recent <- mutate(oldest_recent, pop_difference)

# The next step is to find the states that has the most change of population and
# pull the states name out, using "selected_states"

oldest_recent <- oldest_recent[order(oldest_recent$difference) , ]

# What are the top 5 population difference?

top_5_difference <- top_n(oldest_recent,5,difference)

# What are the top 5 states with the largest population difference?
selected_state <-pull(top_5_difference,state)

# The state with the most population change is California. The top 5 are:
# NC, GA, FL, TX, CA. now we want to focus on  these 5 states and calculate the data, to do that we need to filter out all the 
# data that is related, use "selected_states_data" to extract the data

selected_states_data <- filter(pop_and_prison, state == "CA"| state =="NC"|state =="GA"|state =="FL"|state =="TX")

# Next we find the population overtime in these states
selected_states_pop_overtime <- pop_and_prison%>%
  filter( state == "CA"| state =="NC"|state =="GA"|state =="FL"|state =="TX")%>%
  group_by(year,state) %>%
  summarise(state_total = sum(total_pop_15to64, na.rm = TRUE))


# Population growth of African American and White
# After down calculating the state level data, we want to focus on the two 
# racial groups

black_pop <- selected_states_data %>%
  group_by(year,state) %>%
  summarise(black_total_overtime = sum(black_pop_15to64, na.rm = TRUE))

white_pop<- selected_states_data %>%
  group_by(year,state) %>%
  summarise(white_total_overtime = sum(white_pop_15to64, na.rm = TRUE))

all_pop_overtime <- left_join(black_pop, white_pop)

selected_states_pop_overtime <- left_join(selected_states_pop_overtime, all_pop_overtime)

# Because the values are added up values, which means the most recent value is the 
# result of adding up all the previous values. We will extract the most recent value
most_recent_pop <- filter(selected_states_pop_overtime, year == 2018)

# Overall, what are the mean pop for each race? 
mean_white <- mean(most_recent_pop$white_total_overtime)
mean_black <- mean(most_recent_pop$black_total_overtime)

# Where is the highest population located in the five states? Use max_state
max_state_pop <- filter(most_recent_pop, state_total == max(state_total))
max_state_pop <- pull(max_state_pop, state)

# What is the mean pop overall?
mean_pop <- mean(most_recent_pop$state_total)


# Next, we want to calculate the stat related to prison pop, which is very similar to what we did for 
# total pop

selected_states_prison_overtime <- selected_states_data%>%
  group_by(year,state) %>%
  summarise(state_prison_total = sum(total_prison_pop, na.rm = TRUE))

black_prison_pop_overtime <- selected_states_data  %>%
  group_by(year,state) %>%
  summarise(black_prison_total = sum(black_prison_pop, na.rm = TRUE))

white_prison_pop_overtime <- selected_states_data %>%
  group_by(year,state) %>%
  summarise(white_prison_total = sum(white_prison_pop, na.rm = TRUE))

all_prison_overtime <- left_join(black_prison_pop_overtime, white_prison_pop_overtime)
selected_states_prison_overtime <- left_join(selected_states_prison_overtime,all_prison_overtime)

# We want to extract the most recent values for selected prison
most_recent_prison <- filter(selected_states_prison_overtime, year == 2016)

# What are the mean prison pop for each race?
mean_prison_white <- mean(most_recent_prison$white_prison_total)
mean_prison_black <- mean(most_recent_prison$black_prison_total)

# Where is the highest prison population located in the five states? Use max_state 
max_prison_state <- most_recent_prison %>%
  filter(state_prison_total == max(state_prison_total)) %>%
  pull(state)

# what is the mean prison pop overall ?
mean_prison_pop <- mean(most_recent_prison$state_prison_total)

# Last part before we plot is the prison administration overtime. The calculation is
# a little bit different because we want to use this dataset for the mapping part of our 
# analysis, it is easier to draw a country in a given year than to complicate the map. 

# We choose the whole country as our state of interest and 2016 as the given year

prison_adm <- select(pop_and_prison, year,state,county_name,black_prison_adm,white_prison_adm)
most_recent_ca_adm_black <- prison_adm %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  summarize(black_prison_adm = sum(black_prison_adm, na.rm = T))

most_recent_ca_adm_white <- prison_adm %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  summarize(white_prison_adm = sum(white_prison_adm, na.rm = T))


most_recent_ca_adm_black$state <- state.name[match(most_recent_ca_adm_black$state, state.abb)]
most_recent_ca_adm_black[8,1] <- "district of columbia"
most_recent_ca_adm_black$state <- tolower(most_recent_ca_adm_black$state)
most_recent_ca_adm_black <- most_recent_ca_adm_black[-c(1,12),]
most_recent_ca_adm_black <- rename(most_recent_ca_adm_black, region = state)

most_recent_ca_adm_white$state <- state.name[match(most_recent_ca_adm_white$state, state.abb)]
most_recent_ca_adm_white[8,1] <- "district of columbia"
most_recent_ca_adm_white$state <- tolower(most_recent_ca_adm_white$state)
most_recent_ca_adm_white <- most_recent_ca_adm_white[-c(1,12),]
most_recent_ca_adm_white <- rename(most_recent_ca_adm_white, region = state)

# Questions related to prison admission

# Which state has the highest black prison admission?

highest_black_prison_adm_state <- most_recent_ca_adm_black %>%
  filter(black_prison_adm == max(black_prison_adm)) %>%
  pull(region)

# Which state has the highest white prison admission?

highest_white_prison_adm_state <- most_recent_ca_adm_white %>%
  filter(white_prison_adm == max(white_prison_adm)) %>%
  pull(region)

# What are the corresponding prison admission?

highest_black_prison_adm <- most_recent_ca_adm_black %>%
  filter(black_prison_adm == max(black_prison_adm)) %>%
  pull(black_prison_adm)

highest_white_prison_adm <- most_recent_ca_adm_white %>%
  filter(white_prison_adm == max(white_prison_adm)) %>%
  pull(white_prison_adm)

# Find the african and white american population in texas in 2016, what do you find?

black_pop_2016 <- black_pop %>%
  filter(year == 2016 & state =="TX") %>%
  pull(black_total_overtime)

white_pop_2016 <- white_pop %>%
  filter(year == 2016 & state == "TX")%>%
  pull(white_total_overtime)


ID <- list("selected_state","mean_white","mean_black","mean_prison_black", "mean_prison_white","mean_pop","mean_prison_pop","highest_black_prison_adm","highest_black_prison_adm_state","highest_white_prison_adm","highest_white_prison_adm_state","black_pop_2016","white_pop_2016")
ID <- data.frame(ID)
ID <- t(ID)

summary_info <- list(toString(selected_state),mean_white,mean_black,mean_prison_black, mean_prison_white,mean_pop,mean_prison_pop,highest_black_prison_adm,highest_black_prison_adm_state,highest_white_prison_adm,highest_white_prison_adm_state,black_pop_2016,white_pop_2016)
summary_info <- data.frame(summary_info)
summary_info <- t(summary_info)

summary_info <- data.frame(ID,Obs = unlist(summary_info))
summary_info <- subset(summary_info, select = -c(ID))
write_csv(summary_info,"summary_info.csv")



# Variable comparison code

selected_prison_pop <- left_join(all_prison_overtime, all_pop_overtime)
selected_prison_pop <- transform(selected_prison_pop, black_ratio = black_prison_total / black_total_overtime)
selected_prison_pop <- transform(selected_prison_pop, white_ratio = white_prison_total / white_total_overtime)


# Part 2 Charts

# Trends over time chart 1

p1 <- ggplot(data = selected_states_pop_overtime) +
  geom_point(mapping = aes(x = year, y = state_total,color = state)) +
  geom_line(mapping = aes(x = year, y = state_total,color = state)) +
  xlim(1990, 2018)+
  labs(y = "Total Population", title = "Total Population from 1990 to 2018")+
  scale_color_brewer(palette = "Set1")

p2 <- ggplot(data = selected_states_pop_overtime) +
  geom_line(mapping = aes(x = year, y = black_total_overtime, color = state)) +
  geom_point(mapping = aes(x = year, y = black_total_overtime, color = state)) +
  xlim(1990, 2018) +
  labs(y = "Total Population", title = "Total population of African American 1990 to 2018")+
  scale_color_brewer(palette = "Set1")

p3 <- ggplot(data = selected_states_pop_overtime) +
  geom_line(mapping = aes(x = year, y = white_total_overtime, color = state)) +
  geom_point(mapping = aes(x = year, y = white_total_overtime, color = state)) +
  xlim(1990, 2018) +
  labs(y = "Total Population", title = "Total population of White 1990 to 2018")+
  scale_color_brewer(palette = "Set1")

chart1 <- grid.arrange(p1,p2,p3)


# Variable Comparison Chart 2

p4 <- ggplot(data = selected_prison_pop) +
  geom_point(mapping = aes(x = white_total_overtime, y = white_prison_total)) +
  ylim(1,NA)+
  xlim(1,NA)+
  labs(x = "Total Population", y = "prison population", title = "Total White Prison to Population")+
  scale_color_brewer(palette = "Set3")

p5 <- ggplot(data = selected_prison_pop) +
  geom_point(mapping = aes(x = black_total_overtime, y = black_prison_total)) +
  xlim(1,NA)+
  ylim(1,NA)+
  labs(x = "Total Population", y = "prison population",  title = "Total Black Prison to Population")+
  scale_color_brewer(palette = "Set3")

p6 <- ggplot(data = selected_prison_pop) +
  geom_point(mapping = aes(x = year, y = black_ratio)) +
  xlim(1990, 2016) +
  ylim(0,0.05)+
  labs(x = "year", y = "prison to pop ratio, unit 1", title = "Black Pirson to Population ratio") 

p7 <- ggplot(data = selected_prison_pop) +
  geom_point(mapping = aes(x = year, y = white_ratio)) +
  xlim(1990, 2016) +
  ylim(0,0.05)+
  labs(x = "year", y = "prison to pop ratio, unit 1", title = "White Pirson to Population ratio") 
chart2 <- grid.arrange(p4,p5,p7,p6)


# Part 3 Map

shapefile <- map_data("state")
shapefile <- inner_join(shapefile,most_recent_ca_adm_black)
shapefile <- inner_join(shapefile,most_recent_ca_adm_white)



p8 <- ggplot(data = shapefile) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_prison_adm)) +
  labs(x ="", y ="", title ="African American Prison Admission in US, 2016", fill = "Num. People") +
  theme_minimal()

p9 <- ggplot(data = shapefile) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = white_prison_adm)) +
  labs(x ="", y ="", title ="White Prison Admission in US, 2016", fill = "Num. People") +
  theme_minimal()

map1 <- grid.arrange(p8,p9)


