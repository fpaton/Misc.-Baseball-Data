# Combine neighborhood data frames and numeric:

# neibourhood effect

# Train n df:
train.nei.df <- train.df$Neighborhood %>% as.factor() %>% as.data.frame()
contrast.nei.train<- model.matrix(~ . +0, data=train.nei.df, contrasts.arg = lapply(train.nei.df, contrasts, 
                                                                                    contrasts=FALSE)) %>% 
  as.data.frame()

# ______________

# Test n df:
test.nei.df <- test.df$Neighborhood %>% as.factor() %>% as.data.frame()
contrast.nei.test<- model.matrix(~ . +0, data=test.nei.df, contrasts.arg = lapply(test.nei.df, contrasts, 
                                                                                  contrasts=FALSE)) %>% 
  as.data.frame()

# ---------------------------

# merge identifier:

train.normal$Id <- train.df$Id
contrast.nei.train$Id <- train.df$Id

final.train <- merge(train.normal, contrast.nei.train, by="Id")
###

# linear model string:
names.neigh <- names(contrast.nei.train)
train.names <- paste(names.neigh, collapse = " + ")
model.n <- paste("SalePrice ~ ", train.names, sep="") 

model.1 <- SalePrice ~ .Blmngtn + .Blueste + .BrDale + .BrkSide + .ClearCr + .CollgCr + .Crawfor + .Edwards + 
  .Gilbert + .IDOTRR + .MeadowV + .Mitchel + .NAmes + .NoRidge + .NPkVill + .NridgHt + .NWAmes + 
  .OldTown + .Sawyer + .SawyerW + .Somerst + .StoneBr + .SWISU + .Timber + .Veenker + GrLivArea + OverallQual +
  YearBuilt

model.0 <- SalePrice ~ GrLivArea + OverallQual + YearBuilt + GarageArea

nn.final <- neuralnet(model.1, data= final.train, linear.output= TRUE, hidden=c(18, 6, 2), threshold = .01)

plot(nn.final)

nn.final$result.matrix

# Compute predictions

final.test <- test.normal[, c("GrLivArea", "OverallQual", "YearBuilt") ]
final.test$Id <- test.df$Id 
contrast.nei.test$Id <- test.df$Id
final.test <- merge( final.test, contrast.nei.test, by="Id")
final.test <- final.test[,-1 ]
pred.nn.1 <- neuralnet::compute(nn.final, final.test)

# Outpute file: 
pr.nn <- pred.nn.1$net.result*(max(train.df$SalePrice)
                                     -min(train.df$SalePrice))+min(train.df$SalePrice)

submission_matrix <- data.frame(cbind(test.df$Id, pr.nn))
colnames(submission_matrix) = c('Id', 'SalePrice')
submission_matrix$SalePrice <- round(submission_matrix$SalePrice)
submission_matrix$SalePrice <- pmax(100, submission_matrix$SalePrice)

# Write submission file
write.csv(submission_matrix, file='submission_file_latest.csv', row.names = FALSE)





