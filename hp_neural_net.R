# Variables to use
# overallqual +grlivarea,

library(dplyr)

# normalized = (x-min(x))/(max(x)-min(x))

train.df.sub <- train.df[,c("Id", "SalePrice","GrLivArea", "GarageArea", "LotArea", "OverallQual",
                            "YearBuilt","Exterior1st","MasVnrArea", "ExterQual", "Foundation",
                            "BsmtQual", "TotalBsmtSF", "TotRmsAbvGrd", "PavedDrive", "WoodDeckSF", "CentralAir" )]

test.df.sub <- test.df[, c("GrLivArea", "GarageArea", "LotArea", "OverallQual",
                           "YearBuilt","Exterior1st","MasVnrArea", "ExterQual", "Foundation",
                           "BsmtQual", "TotalBsmtSF", "TotRmsAbvGrd", "PavedDrive", "WoodDeckSF", "CentralAir" )]

# Normailize and Recode categorical/ binary variables:
# Binary: PavedDrive: >> recoded into {-1, 1}
train.df.sub$PavedDrive[train.df.sub$PavedDrive=="Y"] <- -1
train.df.sub$PavedDrive[train.df.sub$PavedDrive=="N"] <-  1
train.df.sub$PavedDrive[train.df.sub$PavedDrive=="P"] <-  0
train.df.sub$PavedDrive <- as.numeric(train.df.sub$PavedDrive)

test.df.sub$PavedDrive[test.df.sub$PavedDrive=="Y"] <- -1
test.df.sub$PavedDrive[test.df.sub$PavedDrive=="N"] <-  1
test.df.sub$PavedDrive[test.df.sub$PavedDrive=="P"] <-  0
test.df.sub$PavedDrive <- as.numeric(test.df.sub$PavedDrive)
# Central Air
train.df.sub$CentralAir[train.df.sub$CentralAir=="Y"] <- -.5
train.df.sub$CentralAir[train.df.sub$CentralAir=="N"] <-  .5
train.df.sub$CentralAir <- as.numeric(train.df.sub$CentralAir)

test.df.sub$CentralAir[test.df.sub$CentralAir=="Y"] <-  -.5
test.df.sub$CentralAir[test.df.sub$CentralAir=="N"] <-  .5
test.df.sub$CentralAir <- as.numeric(test.df.sub$CentralAir)

# Normalize numeric columns

# Train
numerSub.train <- sapply(train.df.sub, is.numeric) # train
train.df.sub.num <- train.df.sub[, numerSub.train] # train
max.train <- apply(train.df.sub.num, 2, max, na.rm = TRUE)
min.train <- apply(train.df.sub.num, 2, min, na.rm=TRUE)
train.normal <- scale(train.df.sub.num, center= min.train, scale= max.train - min.train) %>%
  as.data.frame(.)
# ------------------

# Test:
numerSub.test <- sapply(test.df.sub, is.numeric) # test
test.df.sub.num <- test.df.sub[, numerSub.test]  # test
max.test <- apply(test.df.sub.num, 2, max, na.rm=TRUE)
min.test <- apply(test.df.sub.num, 2, min, na.rm=TRUE)
test.normal <- scale(test.df.sub.num, center= min.test, scale= max.test - min.test) %>%
  as.data.frame(.)
# ------------------

# test {-1, 1} for paved driveway:

train.normal$CentralAir <- train.df.sub$CentralAir

# neural net
model.0 <- SalePrice ~ GrLivArea + OverallQual + YearBuilt + GarageArea#  1.09

sum(is.na(train.df.sub$))

nn.0 <- neuralnet(model.0, data=train.normal, linear.output = TRUE, hidden = c(4, 3, 2),
                      threshold =.01)
plot(nn.0)

final.test <- test.normal[, c("GrLivArea", "OverallQual", "YearBuilt", 
                              "LotArea", "GarageArea") ]

final.test$GarageArea[1117] <- 0
nn.0.prediction <- neuralnet::compute(nn.0, final.test)


nn.0.prediction$net.result
sum(is.na(nn.0.prediction$net.result))

names(train.normal)

