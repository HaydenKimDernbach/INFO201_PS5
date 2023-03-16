library(tidyverse)
library(gapminder)
library(ggplot2)

data <- read_delim("data/gapminder.csv")

numRows <- nrow(data)
numCols <- ncol(data)

head(data)

# How many countries are in the dataset
length(unique(data$iso3))
length(unique(data$iso2))
length(unique(data$name))

# Trying to find iso2 codes with multiple names
multiNameIso2 <- group_by(data, iso2) %>% 
    select(name) %>% 
    summarise(
        count = n()
    )
unique(multiNameIso2) %>% 
    filter(count == 1)


multiNameIso3 <- group_by(data, name) %>% 
    summarise(
        
        n = length(unique(iso3))
    )
multiNameIso3 %>% 
    filter(n > 1)

# Min and max years
min(data$time, na.rm = TRUE)
max(data$time, na.rm = TRUE)

# Part 2 ------

# Missing CO2 values per year
missingCO2 <- group_by(data, time) %>% 
    select(co2)
sum(is.na(missingCO2))

missingCO2_PC <- group_by(data, time) %>% 
    select(co2_PC)
sum(is.na(missingCO2_PC))

# Plot of CO2 emissions over time

countries <- filter(data, iso3 == "USA" | iso3 == "CHN" | iso3 == "IND" | iso3 == "KOR" | iso3 == "JPN")

ggplot(data = countries) +
    geom_point(mapping = aes(x = time, y = co2, color = iso3))

ggplot(data = countries) +
    geom_point(mapping = aes(x = time, y = co2_PC, color = iso3))

# Compute average CO2 emissions per capita across the continents (assume region is the same as continent).

averageCO22016 <- group_by(data, region) %>% 
    filter(time == 2016) %>% 
    summarise(
        mean = mean(co2_PC, na.rm = TRUE)
    )
averageCO22016

averageCO21960 <- group_by(data, region) %>% 
    filter(time == 1960) %>% 
    summarise(
        mean = mean(co2_PC, na.rm = TRUE)
    )
averageCO21960

averageCO2 <- data.frame(data$region, data$time, data$co2_PC) %>% 
    filter(data.time == 2016 | data.time == 1960) %>% 
    arrange(data.region, data.time)
averageCO2



ggplot(averageCO2, aes(fill = mean, x = region, y = averageCO22016.mean)) + 
    geom_bar(position=position_dodge(), stat="identity")

head(data)
# 8pt) Make a scatterplot of GDP per capita versus life expectancy by country, using data for
# 1960. Make the point size dependent on the country size, and color those according to the
# continent. Feel free to adjust the plot in other ways to make it better.
# Comment what do you see there.
# 2. (4pt) Make a similar plot, but this time use 2019 data only.

df <- data.frame(data)

ggplot(df, aes(x = GPD_PC, y = lifeExpectancy)) + 
    geom_point(aes(size=totalPopulation))
