---
title: "R Notebook - Sugar Consumption in the United States, 1961 - 2004"
output: html_notebook

###Summary:  Sugar consumption per person, per day has increased from 1980 to 2004.  By 2004 the United States had the highest consumption of sugar of all countries measured.
---

```{r}

library(tidyr)
library(dplyr)
library(plyr)
library(reshape2)
library(gridExtra)
library(rio)
install_formats()


convert("sugar.xlsx","sugar3.csv")
df <- read.csv("sugar3.csv", header=T, fill = TRUE)
colnames(df)[1] <- "country"
df[is.na(df)] <- 0

gather(df, "year","n", na.rm = T)
xx <- gather(df, "year","n", na.rm = T,-country)
xx$year <- gsub("X","",xx$year)

ggplot()+  geom_boxplot(aes(x=year,y=n),data=xx)+ scale_x_discrete (breaks = seq(1960, 2004, 2)) 
summary(xx$n)



# *** US Only, vertical Years
ggplot(data=subset(xx,country=="United States"), aes(y=factor(n), x=year)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(x="Year", y="Grams of Sugar") + 
  ggtitle("Sugar Consumption Per Person, Per Day, in the United States, 1961-2004") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

```

```{r}


# Top 20 Countries in 2004
df2004 <- subset(xx, year == "2004.0")
top40a <- df2004$country [order (df2004$n, decreasing = TRUE)]
ggplot (data = subset (df2004, country %in% top40a [1 : 20]), 
        aes (n, country)) +
  geom_point (aes (color = factor (n)), size = 4) +
  labs(x="Grams of Sugar per Day", y="Country") + 
  ggtitle("Top 20 Countries in Sugar Consumption Per Person, Per Day in 2004")
```
United States has the highest consumption rate of sugar in 2004




```{r}
# Top 40 Countries in 1980
df1980 <- subset(xx, year == "1980.0")
top40 <- df1980$country [order (df1980$n, decreasing = TRUE)]
ggplot (data = subset (df1980, country %in% top40 [1 : 20]), 
        aes (n, country)) +
  geom_point (aes (color = factor (n)), size = 4) +
  labs(x="Grams of Sugar per Day", y="Country") + 
  ggtitle("Top 20 Countries in Sugar Consumption Per Person, Per Day in 1980") 
```

```{r}
# Top 40 Countries in 1962
df1962 <- subset(xx, year == "1962.0")
top40c <- df1962$country [order (df1962$n, decreasing = TRUE)]
ggplot (data = subset (df1962, country %in% top40 [1 : 20]), 
              aes (n, country)) +
  geom_point (aes (color = factor (n)), size = 4) +
  labs(x="Grams of Sugar per Day", y="Country") + 
  ggtitle("Top 20 Countries in Sugar Consumption Per Person, Per Day in 1962") 

```