# Load the data
library(randomForest)
A<-read.csv("pml-training.csv", na.strings = c("NA",""," "))

# preprocess- remove columns with NA or counterproductive effect
DropColumn=logical(length=length(A))
for (idx in 1:length(A)) {
    if (sum(is.na(A[,idx]))/dim(A)[1] >0.5) {
        DropColumn[idx] <- TRUE
    }
    if (idx <= 7) DropColumn[idx] <- TRUE
}

B <- subset(A, subset=A$new_window == "no", select=!DropColumn)

# Do random forest on B
set.seed(1)
ForestObject <- randomForest(classe ~ ., data=B, ntree=100)

print(ForestObject$err.rate[100,1])

T<-read.csv("pml-testing.csv", na.strings = c("NA",""," "))
predictions<-predict(ForestObject,T)
print(predictions)