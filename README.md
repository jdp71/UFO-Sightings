# Analysis of UFO Sightings  

## Introduction  
Do you believe in UFOs? Well you are not alone. Many studies indicate that anywhere between one-third to nearly one-half of all American’s believe in UFOs. So let’s look at some data and see how UFO sightings are trending both in the U.S. and around the world.  

## Data Sources Used  
The data were obtained from Kaggle and were geolocated and time standardized from the National UFO Reporting Center.  The data have 88,875 rows and 11 columns.  Each row represents a reported sighting from 1906-2014.  The data can be downloaded [here](https://www.kaggle.com/NUFORC/ufo-sightings).  

## Technologies Used  
* R Studio 1.1.447  

## Required Packages  
```r
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(ggmap)
library(stringr)
library(lubridate)
library(dplyr)
library(ggmisc)
library(forcats)
library(maps)
library(mapdata)
library(useful)
library(factoextra)
```  

## Analysis Methods Used  
* Data cleaning  
* Exploratory Data Analysis (EDA)  
* Correlation anlaysis  
* Geographical anlaysis 
* Linear regression modeling  

## Model Deployment  
I was able to develop a model that allowed for predictions beyond the year 2014.  The <img src="https://latex.codecogs.com/svg.latex?\Large$;R^2" /> value for this model was 0.93, which indicates a very good fit to the data.  The model was able to make the following predictions:  

| Year | Predicted Number of UFO Sightings |  
| --- | :---: |  
| 2014 | 6212 |  
| 2015 | 6479 |  
| 2016 | 6747 |  
| 2017 | 7015 |  
| 2018 | 7282 |  
| 2019 | 7550 |  

## Summary of Results  
Most UFO sightings within the United States occur in California. In fact, California has over two-times as many sightings as the next state, Washington. Another interesting note is that the top five UFO sighting locations within the U.S. (California, Washington, Florida, Texas, and New York) are states that have a coastline.  Additionally, there are a definite increase in UFO sightings in the months of July and January.  A possible reason for these spikes is due to fireworks on July 4th and January 1st.  On these days, people are looking up at the sky and are more likely to see objects they report as being UFOs.  Finally, as the model above indicated, UFO observations are indeed increasing.
