#This is the complete analysis file

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(ggmap)
library(stringr)
library(lubridate)
library(dplyr)
library(ggpmisc)
library(ggmap)
library(maps)
library(mapdata)
library(useful)
library(factoextra)


#Plot of country vs sightings
plot1 <- ggplot(complete_cleaned, aes(x=country)) + geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Country") + ylab("Number of Sightings") + ggtitle("UFO Sightings by Country") +
    theme(plot.title = element_text(hjust = 0.5))

plot2 <- ggplot(complete_cleaned, aes(x=state)) + geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("State") + ylab("Number of Sightings") + ggtitle("UFO Sightings by U.S. State") +
    theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot1, plot2, nrow = 1)

#Subset data for US, other, and not listed
subsetUS <- subset(complete_cleaned, country == "us")
subsetOther <- subset(complete_cleaned, country == "other")
subsetNL <- subset(complete_cleaned, country == "not listed")
subsetCountry <- subset(complete_cleaned, country != "us" & country != "other" & country != "not listed")

#Put columns in proper classes
subsetUS$datetime <- as.POSIXct(subsetUS$datetime, format = "%m/%d/%Y %H:%M")
subsetOther$datetime <- as.POSIXct(subsetOther$datetime, format = "%m/%d/%Y %H:%M")
subsetNL$datetime <- as.POSIXct(subsetNL$datetime, format = "%m/%d/%Y %H:%M")
subsetCountry$datetime <- as.POSIXct(subsetCountry$datetime, format = "%m/%d/%Y %H:%M")

#Histograms of subplots
plot3 <- ggplot(subsetCountry, aes(x=country)) + geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    xlab("Country") + ylab("Number of Sightings") + ggtitle("UFO Sightings by Country") +
    theme(plot.title = element_text(hjust = 0.5))

plot4 <- ggplot(subsetUS, aes(x=state)) + geom_bar() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    xlab("State") + ylab("Number of Sightings") + ggtitle("UFO Sightings by U.S. State") +
    theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot3, plot4, nrow = 1)


#state, city, and shape counts of sightings
state_counts <- subsetUS %>% 
    filter(city != "") %>% 
    count(state, sort = TRUE) %>% 
    unite("location", -n, sep = "")
city_counts <- subsetUS %>% 
    filter(state != "") %>% 
    count(city, state, sort = TRUE) %>% 
    unite("location", -n, sep = ",")
shape_counts <- subsetUS %>% 
    filter(!shape %in% c("", "unknown", "other", "light")) %>% 
    count(shape, sort = TRUE) %>% 
    unite("location", -n, sep = ",")

#Plot of states with greater than 100 UFO sightings
filter(state_counts, n > 100) %>% 
    mutate(x = factor(location)) %>% 
    ggplot(aes(x=reorder(location, -n), y=n)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    xlab("State") + ylab("Number of Sightings") + ggtitle("States with over 100 UFO Sightings") +
    theme(plot.title = element_text(hjust = 0.5))

#Plot of cities
filter(city_counts, n > 100) %>% 
    mutate(x = factor(location)) %>% 
    ggplot(aes(x=reorder(location, -n), y=n)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    xlab("City") + ylab("Number of Sightings") + ggtitle("Cities with over 100 UFO Sightings") +
    theme(plot.title = element_text(hjust = 0.5))

#Just for interest, the plot of top countries with sightings
top20 <- row.names(as.data.frame(summary(subsetCountry$country, max = 20)))
subsetCountry$country <- as.character(subsetCountry$country)
subsetCountry$top <- ifelse(
    subsetCountry$country %in% top20,
    subsetCountry$country,
    ""
)
subsetCountry$top <- as.factor(subsetCountry$top)
ggplot(subsetCountry, aes(x=fct_rev(reorder(country, country, FUN=length)))) +
    stat_count() + 
    theme_bw() + xlab("Country") + ylab("Number of Sightings") + ggtitle("Top 15 Countries Outside U.S. for UFO Sightings") + theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim("ca", "gb", "au", "mx", "in", "ie", "pr", "nz", "za", "de", "nl", "es", "fr", "br", "ph") +
    scale_x_discrete(name ="Country", 
                     limits=c("ca", "gb", "au", "mx", "in", "ie", "pr", "nz", "za", "de", "nl", "es", "fr", "br", "ph"),
                     labels=c("Canada", "Great Britain", "Australia", "Mexico", "India", "Ireland", "Puerto Rico", "New Zealand", "South Africa", "Germany", "Netherlands", "Spain", "France", "Brazil", "Philippines"))

#Analysis of sightings over time
subsetUS <- subsetUS %>% 
    mutate(day = day(datetime),
           month = month(datetime),
           year = year(datetime),
           hour = hour(datetime))
sightingsPlot <- subsetUS %>% 
    mutate(datetime = floor_date(datetime, unit = "1 year")) %>% 
    group_by(datetime) %>% 
    summarize(cts = n()) %>% 
    ggplot(aes(datetime, cts)) +
    geom_line(size = 1) +
    scale_x_datetime(date_breaks = "10 years", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
    xlab("Date") + ylab("Number of Sightings") + ggtitle("U.S. UFO Sightings Since 1906") +
    theme(plot.title = element_text(hjust = 0.5))
sightingsPlot

#Fitting a linear model to the data
my.formula <- y ~ x
sightingsPlot + geom_smooth(method = "lm", formula = my.formula) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE)

#Plotting from 1989
subsetYear <- subset(subsetUS, year >= "1989")
subsetYear <- subset(subsetYear, year <= "2013")
sightingsPlotYear <- subsetYear %>% 
    mutate(datetime = floor_date(datetime, unit = "1 year")) %>% 
    group_by(datetime) %>% 
    summarize(cts = n()) %>% 
    ggplot(aes(datetime, cts)) +
    geom_line(size = 1) +
    scale_x_datetime(date_breaks = "5 years", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
    xlab("Date") + ylab("Number of Sightings") + ggtitle("U.S. UFO Sightings from 1989 - 2013") +
    theme(plot.title = element_text(hjust = 0.5))
sightingsPlotYear

#Fitting a linear model to the data
my.formula <- y ~ x
sightingsPlotYear + geom_smooth(method = "lm", formula = my.formula) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..rr.label.., sep = "~~~")), 
                 parse = TRUE)

subsetTest <- data.frame(table(subsetYear$year))

ggplot(data = subsetTest, aes(x = Var1, y = Freq, group = 1)) +
           geom_line() +
    geom_smooth(method = "lm")

fit1989 <- lm(Freq ~ Var1, data = subsetTest)
summary(fit1989)

a <- c(1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013)
b <- c(236,232,217,232,281,397,1284,823,1216,1710,2747,2605,2897,2899,3470,3813,3730,3397,4022,4600,4209,4125,5044,7187,6893)
regression <- lm(b~a)
summary(regression)

#Plotting the seasonal effect
subsetUS <- subsetUS %>% 
    mutate(day = day(datetime),
           month = month(datetime),
           year = year(datetime),
           hour = hour(datetime))

subsetUS %>% 
    mutate(month = factor(month),
           day = factor(day)) %>% 
    filter(between(year, 1989, 2013)) %>% 
    group_by(year, month) %>% 
    summarize(cts = n()) %>% 
    ggplot(aes(month, cts, group = year)) +
    geom_line() + ylab("Number of Sightings") +
    facet_wrap(~ year, ncol = 5, scales = "free_y") +
    labs(title = "Are UFO Sightings in the U.S. Seasonal?") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12") +
    scale_x_discrete(name ="Month", 
                     limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                     labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))

#Plotting the daily effect
subsetUS <- na.omit(subsetUS)
subsetUS %>% 
    group_by(day, hour) %>% 
    summarize(cts = n()) %>% 
    ggplot(aes(hour, cts, group = day)) +
    geom_line() + ylab("Number of Sightings") +
    facet_wrap(~ day) +
    labs(title = "What Time and Day of the Month do most UFO Sightings Occur?") +
    theme(plot.title = element_text(hjust = 0.5))

#Calculating the probability of seeing a UFO at noon
subsetUS %>% 
    count(hour) %>% 
    mutate(freq = n/sum(n),
           cum_prob = cumsum(freq)) %>% 
    slice(11:13)
#Plot the probability of seeing a UFO at noon
subsetUS %>%
    count(hour)%>%
    mutate(freq = n/sum(n),
           cum_prob = cumsum(freq))%>%
    ggplot(aes(hour, cum_prob))+
    geom_area(alpha = .5)+
    geom_vline(xintercept = 12, color='black')+
    geom_hline(yintercept = 0.279, color='blue')+
    geom_hline(yintercept = 0.018, color='red') +
    labs(title="Estimated Cumulative Probability of a UFO Sighting by Hour in the Day") +
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = 5, y = 0.9, label = "Only about a 1.8% of all sightings\n occur between noon and 1:00 pm") +
    annotate("text", x = 5, y = 0.7, label = "Cumulatively, only about a 28% of all sightings\n occur between midnight and noon")
    
    

#Plot of most common shapes
filter(shape_counts, n > 100) %>% 
    mutate(x = factor(location)) %>% 
    ggplot(aes(x=reorder(location, -n), y=n)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    xlab("UFO Shape") + ylab("Number of Sightings") + ggtitle("Most Common UFO Shape Descriptions") +
    theme(plot.title = element_text(hjust = 0.5))

#Correlation plot between shape of UFO and time of day observed
shapes_day <- subsetUS %>% 
    group_by(hour = hour(datetime), shape) %>% 
    summarize(count = n())
ggplot(shapes_day, aes(x = hour, y = shape, size = count)) +
    geom_point() +
    xlab("Hour in the Day") + ylab("Shape") + ggtitle("UFO Shape Descriptions as a function of Hour in the Day") +
    theme(plot.title = element_text(hjust = 0.5))
    
#Correlation plot between shape of UFO and state where it was observed
shapes_state <- subsetUS %>% 
    group_by(state, shape) %>% 
    summarize(count = n())
ggplot(shapes_state, aes(x = state, y = shape, size = count)) +
    geom_point() +
    xlab("Hour in the Day") + ylab("Shape") + ggtitle("UFO Shape Descriptions as a function of State") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#Simple USA map outline
usa <- map_data("usa")
usaOutline <- ggplot() + 
    geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "red") + 
    coord_fixed(1.3)

#Get subset into proper class for plotting
subsetUS$latitude <- as.numeric(as.character(subsetUS$latitude))

#New subset to drop all zero lat long values and drop HI and AK
subsetContUS <- subset(subsetUS, state != "ak" & state != "hi" & latitude != "0" & longitude != "0")

#Outline of US map with data points
usaOutline
usaOutline + geom_point(data = subsetContUS, aes(x = longitude, y = latitude), color = "red", size = 1) +
    labs(title="UFO Sighting Locations within the U.S.") + theme(plot.title = element_text(hjust = 0.5))

#Make subset of just mountain states
subsetMountain <- read.csv("~/School/Classes/DSC 520/Final Project/Final Project/subsetMountain.csv", stringsAsFactors=FALSE)

#Extract only lat and long columns for cluster analysis
clusterMountain <- subsetMountain %>% 
    select(latitude, longitude)


set.seed(456)

#Establish cluster regions
cluster3Mt <- kmeans(x=clusterMountain, centers = 3)
cluster5Mt <- kmeans(x=clusterMountain, centers = 5)
cluster10Mt <- kmeans(x=clusterMountain, centers = 10)
cluster15Mt <- kmeans(x=clusterMountain, centers = 15)


#Plot cluster regions
p3Mt <- fviz_cluster(cluster3Mt, geom = "point", data = clusterMountain) + ggtitle("k=3 Cluster Results")
p5Mt <- fviz_cluster(cluster5Mt, geom = "point", data = clusterMountain) + ggtitle("k=5 Cluster Results")
p10Mt <- fviz_cluster(cluster10Mt, geom = "point", data = clusterMountain) + ggtitle("k=10 Cluster Results")
p15Mt <- fviz_cluster(cluster15Mt, geom = "point", data = clusterMountain) + ggtitle("k=15 Cluster Results")

rownames()
usaOutline
p3Mt + 
    geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "red") + 
    coord_fixed(1.3) +
    coord_flip() #This will do an overlay, but the axes are all messed up
p5Mt + coord_flip()
p10Mt + coord_flip()
p15Mt + coord_flip()


#Determining the correct number of clusters
wssMt <- (nrow(clusterMountain)-1)*sum(apply(clusterMountain,2,var))
nclusters = 15
for(i in 2: nclusters){
    wssMt[i]<-sum(kmeans(clusterMountain, centers = i, nstart = 25)$withinss)
    
}
scree_dataMt <- data.frame(wss = wssMt, clusters = 1:nclusters)
head(scree_dataMt)

#Plot out scree data to find elbow
ggplot(data = scree_dataMt) + 
    geom_line(aes(x = clusters, y = wss)) +
    geom_point(aes(x = clusters, y = wss), size = 3) +
    scale_x_continuous(breaks = 1:nclusters) +
    theme_classic()
#k=5?

#Silhouette method for determining value of k
fviz_nbclust(clusterMountain, kmeans,
             method = "silhouette")
#k=6

#adding a custom legend to the plot that shows which state is within each cluster
p5Mt + coord_flip() + scale_fill_discrete(name="State", labels = c("1: NV", "2: AZ", "3: UT", "4: ID, MT", "5: CO, NM, WY"))







