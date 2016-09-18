setwd("~/Documents/Kaggle/housingprices")
library(neuralnet)
library(magrittr)
library(ggplot2)

# Load in data
train.df <- read.csv("train.csv", stringsAsFactors = FALSE)
test.df  <- read.csv("test.csv", stringsAsFactors = FALSE)

# Check data for outliers
# here all housing square footage above 4,000 are removed
train.df$GrLivArea[c(524, 692, 1299, 1183)] # outliers
salevGr <- ggplot(data=train.df, aes(x=SalePrice, y= GrLivArea))+
  geom_point()

# remove outliers
train.df <- train.df[-c(524, 692, 1299, 1183), ]
train.df$Id

# test plots/ random test code
salevCon <- ggplot(data=train.df, aes(x=train.df$Neighborhood, y= SalePrice))+
  geom_point()
salevCon

lm(data=train.df, SalePrice ~ LotArea) %>% summary(.)

head(test.df$GarageArea)
pairs.df <- train.normal[, c("GrLivArea", "YearBuilt", "LotArea")]
pairs(pairs.df)
# --------------------------------
# cat: ExterQual, BsmtQual, Foundation, Exterior1st
# bin: central air, paved drive, cars
# numeric int: total rooms above grade, yearbuilt
# numeric cont: garagearea, overallquality, grliv area SF, totalbsmsSF 





