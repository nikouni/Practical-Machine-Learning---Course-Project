valid_cols <- function(data) {
  num_cols <- ncol(data)
  num_rows <- nrow(data)
  res <- vector("numeric")
  for (i in 1:(num_cols)) {
    if (i <= 5 && i != 2 ) #remove col X and timestamps
      next
    if(sum(is.na(data[,i])) < num_rows )
      res <- c(res,i)
  }  
  
  res
}

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

set.seed(1234)
source("project.R")

training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

vcols <- valid_cols(testing)
training <- training[,vcols]
testing <- testing[,vcols]
testing <- testing[,1:(ncol(testing)-1)]
myControl <- trainControl(method = "oob", number = 4, verboseIter = TRUE)
model <- train(classe ~ . , method = "rf" , data = training, trControl = myControl)

pred <- predict(model,newdata = testing)
pml_write_files(pred)
