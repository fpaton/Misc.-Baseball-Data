setwd("~/Documents/STAT/440/HousingPirces")
library(neuralnet)
library(magrittr)
library(ggplot2)

# Load in data
train.df <- read.csv("train.csv")
test.df  <- read.csv("test.csv")

# Check data for outliers
# here all housing square footage above 4,000 are removed
train.df$GrLivArea[c(524, 692, 1299, 1183)] # outliers

salevGr <- ggplot(data=train.df, aes(x=SalePrice, y= GrLivArea))+
                  geom_point()
# remove outliers
train.df <- train.df[-c(524, 692, 1299, 1183), ]
m <- nls(SalePrice~YearBuilt, data=train.df)

salevCon <- ggplot(data=train.df, aes(x=SalePrice, y= BldgType))+
  geom_point()
salevCon
