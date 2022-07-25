ad_df <- read.csv("C:/moringa/GROUP WORK/advertising.csv")
View(advertising)

# Preview dataset
head(ad_df)

```{r}
# Finding the Shape of the dataset
dim(ad_df)
```

```{r}
# Finding the datatypes of the data
str(ad_df)

## Data cleaning
```{r}
# checking for missing Data
colSums(is.na(ad_df))
```
There is no missing values in the dataset. 
```{r}
# Check for duplicated data in the ad_Df
ad_df1 <- ad_df[duplicated(ad_df),]
ad_df1
There are no duplicated records in the dataset

```{r}
str(ad_df)
boxplot(ad_df$Daily.Time.Spent.on.Site, main = 'Daily Time Spent on-site')
boxplot(ad_df$Age, main = 'Age Boxplot')
boxplot(ad_df$Area.Income, main = 'Area Income Boxplot')
boxplot(ad_df$Daily.Internet.Usage, main = 'Daily Internet usage boxplot')
```
From the boxplots, only the Area_income column has outliers. 
```{r}
#Print out the outliers 
boxplot(ad_df$Area.Income, main = 'Area Income Boxplot')$out
```
There are outliers that do not look like they are in the extreme. There are areas where poverty is prevelant in such areas the total income could be that small.


str (ad_df)
```

```{r}
ad_df[['Timestamp']] <- as.POSIXct(ad_df[['Timestamp']],
                                   format = "%Y-%m-%d %H:%M:%S")
str(ad_df)
```
The timestamp column is now in the correct dtype

## Univariate Data Analysis
###Numerical Columns
```{r}
summary(ad_df)
```


#### Age 
```{r}
# Mean 
mean.age <- mean(ad_df$Age)
mean.age
```


```{r}
#median 
median.age <- median (ad_df$Age)
median.age
```


```{r}
# Function to get the mode. 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
```


```{r}
mode.age <- getmode(ad_df$age)
mode.age
```

####Area income 
```{r}
mean.areaincome <- mean(ad_df$Area.Income)
mean.areaincome
```


```{r}
median.areaincome <- median(ad_df$Area.Income)
median.areaincome
```


```{r}
mode.areaincome <- getmode(ad_df$Area.Income)
mode.areaincome
```
```{r}
hist(ad_df$Area.Income,
     main="Histogram for Area Income", 
     xlab="Area income", 
     border="blue", 
     col="steelblue",)
```

#### Daily.Internet.Usage

```{r}
mean.daily.internet <- mean(ad_df$Daily.Internet.Usage)
mean.daily.internet
```


```{r}
median.daily.internet <- median(ad_df$Daily.Internet.Usage)
median.daily.internet
```


```{r}
mode.daily.internet <- getmode(ad_df$Daily.Internet.Usage)
mode.daily.internet
```
```{r}
hist(ad_df$Daily.Internet.Usage, 
     main = 'Daily Intenet Usage',
     xlab="Daily internet Usage (mins)", 
     border="blue", 
     col="steelblue")
```

#### Daily time spent on site

```{r}
mean.dtsos <- mean(ad_df$Daily.Time.Spent.on.Site)
mean.dtsos
```



```{r}
median.dtsos <- median(ad_df$Daily.Time.Spent.on.Site)
median.dtsos
```


```{r}
mode.dtsos <- getmode(ad_df$Daily.Time.Spent.on.Site)
mode.dtsos
```
#### Clicked.on.Ad
```{r}
uniq_clickers <- unique(ad_df$Clicked.on.Ad, )
length(uniq_clickers)
```
There are two categories of the people who clicked on ads 
Let us plot the frequency of each
```{r}
clickers <- ad_df$Clicked.on.Ad
clickers_frequency <- table (clickers)
barplot(clickers_frequency, col = "steelblue")
```
There are 500 people who clicked on ads and another 500 did not click on the ads. 

### Categorical Columns
####Ad.Topic.line
```{r}
uniq_topic <- unique(ad_df$Ad.Topic.Line, )
length(uniq_topic)
```
There are 1000 unique topic lines meaning it would be impossible to get a good visualization.

#### City 
```{r}
uniq_city <- unique(ad_df$City, )
length(uniq_city)
```
There are 969 unique cities hence it would also be impossible to get a good visualization

#### Country 
```{r}
uniq_country <- unique(ad_df$Country)
length(uniq_country)
```
There are 237 unique countries. 

```{r}
install.packages(sf)
library(sf)
library(raster)
library(dplyr)
library(spData)
#library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2)
```
```{r}
Country <- ad_df$Country
countyfreq <- table(Country)
```


```{r}
tm_shape(world) +
  tm_polygons() 

```

```{r}
tm_shape(world) +
  tm_fill() +
  tm_borders()

```

#### Gender 
```{r}
male <- ad_df$Male
male_freq <- table(male)
barplot(male_freq, main= 'Gender Distribution', xlab="Gender",
        ylab="Number of people",
        border="red",
        col="steelblue")
```

###Overall Summary
```{r}
summary(ad_df)
```
```{r}
library(lubridate)
ad_df$Month_Yr <- format(as.Date(ad_df$Timestamp), "%Y-%m")
head(ad_df)
```


## Bivariate Analysis

```{r}
ggplot(data = ad_df, mapping = aes(x = Area.Income)) + 
  geom_freqpoly(mapping = aes(colour = Clicked.on.Ad), binwidth = 2000)
```
In areas where the income lies between 60,000 and & 70,000 there is a higher number of people clicking the ads
#### Correlation

```{r}
#creating with only interger columns
numerical_df = ad_df[c("Daily.Time.Spent.on.Site", "Age", "Area.Income","Daily.Internet.Usage" ,"Male", "Clicked.on.Ad" )]
head(numerical_df)
```


```{r}
correlation = cor(numerical_df)
correlation
```

```{r}
library("PerformanceAnalytics")
library(corrplot)
```

```{r}
# Correlation Matrix
corrplot(correlation, method = 'number')
```

```{r}
chart.Correlation(numerical_df, histogram = TRUE, pch = 19, )
```
The chart correlations gives a clear summary on the Bivariate analysis of the dataframe.  


```{r}