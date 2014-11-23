setwd("/Users/sublett/Documents/DataAnalysis/MachineLearning/Project")
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(data.table))
trainDT<-fread("pml-training.csv")
testDT<-fread("pml-testing.csv")

getDuplicates<-function(data.frame) {
	originalNames <- names(data.frame)
	number.of.duplicates <- length(originalNames) -	length(unique(originalNames))
	names.that.are.duplicates <- names( table(originalNames)[table(originalNames)>1] )
	if(length(names.that.are.duplicates)==0)
	 	noDup<-c("There are no duplicate names.")
	else 
		names.that.are.duplicates
}

testDup<-getDuplicates(testDT)
trainDup<-getDuplicates(trainDT)

# Fixes a mispelling in the column names.
origColNames<-names(trainDT)
fixedColNames<-gsub('picth','pitch',origColNames)
setnames(trainDT,origColNames,fixedColNames)

origColNames<-names(testDT)
fixedColNames<-gsub('picth','pitch',origColNames)
setnames(testDT,origColNames,fixedColNames)

# The last column of each dataset differs from the other dataset.
#  	This is simply a check to make sure the remainder is properly named
#		with each other.

checkToBeSame<-identical(names(trainDT[,-160,with=FALSE]),names(testDT[,-160,with=FALSE]))

count<-0
remVector<-c(rep(NA,100))
for(i in 1:160) {
		elementDT<-testDT[[i]][1]
		if( is.na(elementDT) ) {
			for(j in 1:20) {
				if(!is.na(elementDT[j]) ) break	
			}
			remVector[count+1] <- -i
			count<-count+1
		}
		else
		if( elementDT=='' ) {
			for(j in 1:20) {
				if(!elementDT=='') break
			}
			remVector[count+1] <- -i
		 	count<-count+1
		}
}
newTrainDT<-trainDT[,remVector,with=FALSE]
newTestDT<-testDT[,remVector,with=FALSE]

# Remove unneeded variables in the training set
#	and change the response variable in the trainDT
#   set to a factor.

delCol<-c(-1,-2,-3,-4,-5,-6,-7)
newTrainDT<-newTrainDT[,delCol,with=FALSE]
newTestDT<-newTestDT[,delCol,with=FALSE]
newTrainDT$classe<-as.factor(newTrainDT$classe)
checkToBeSame<-identical(names(newTrainDT[,-53,with=FALSE]),names(newTestDT[,-53,with=FALSE]))

# The following corrects 5 entries in the 5373rd event.
#	There are many outliers in this set of data.  Most of
#	them have an indeterminate origin.  These,however,
#	are so far from the comparable entries in other events
#	that one must assume they are a mistake of some sort.
# 	Therefore, values were imputed to address those mistakes.
#	The subject variables are approximately normally  distributed,
#   thus the use of rnorm to generate imputed values.

set.seed(91788)
newTrainDT$gyros_forearm_y[5373]<-rnorm(1,mean(newTrainDT$gyros_forearm_y[-5373]),sd(newTrainDT$gyros_forearm_y[-5373]))
newTrainDT$gyros_forearm_z[5373]<-rnorm(1,mean(newTrainDT$gyros_forearm_z[-5373]),sd(newTrainDT$gyros_forearm_z[-5373]))
newTrainDT$gyros_dumbbell_x[5373]<-rnorm(1,mean(newTrainDT$gyros_dumbbell_x[-5373]),sd(newTrainDT$gyros_dumbbell_x[-5373]))
newTrainDT$gyros_dumbbell_y[5373]<-rnorm(1,mean(newTrainDT$gyros_dumbbell_y[-5373]),sd(newTrainDT$gyros_dumbbell_y[-5373]))
newTrainDT$gyros_dumbbell_z[5373]<-rnorm(1,mean(newTrainDT$gyros_dumbbell_z[-5373]),sd(newTrainDT$gyros_dumbbell_z[-5373])) 

# Now we can start analyzing the data.

suppressPackageStartupMessages(library(randomForest))

# Partition the training data to provide a training subset and
# 	an evaluation subset.

indexPartition<-createDataPartition(y=newTrainDT$classe, p=0.895, list=FALSE)
trainingSet<-newTrainDT[as.vector(indexPartition),]
evalSet<-newTrainDT[-as.vector(indexPartition),]

# Look at correlations

corMatrix<-abs(cor(newTrainDT[,-53,with=FALSE]))
diag(corMatrix)<-0
highCorPairs<-which(corMatrix > 0.8, arr.ind=TRUE)

rfFit<-randomForest(classe ~ .,data=trainingSet, ntree=500)
garbageCollect<-gc()

# Use the eval set to check the forest fit

answersEval<-predict(rfFit,evalSet)
confusionMatrix(answersEval,evalSet$classe)
table(answersEval,evalSet$classe)
png(file="plot1.png",width=640, height=480, bg="transparent")
varImpPlot(rfFit,n.var=25,pch=19,col="grey60",main="Initial Fit, Bicep Dumbbell Curl")
d.off<-dev.off()

# The following code reduces the number of variables in the training
#	set to 13 based on the above plot.  This is somewhat subjective,but
# 	it looks as if these top variables are causing most of the variance.

secondTrainSetColNums<-c(order(-importance(rfFit))[1:13],53)
secondTrainSet<-trainingSet[,secondTrainSetColNums,with=FALSE]
secondEvalSet<-evalSet[,secondTrainSetColNums,with=FALSE]
rfFitTruncated<-randomForest(classe ~ .,data=secondTrainSet, ntree=500)

png(file="plot2.png", width=640, height=480, bg="transparent")
varImpPlot(rfFitTruncated,n.var=13,pch=19,col="grey60",main="Final Fit, Bicep Dumbbell Curl")
d.off<-dev.off()

answersTwo<-predict(rfFitTruncated,secondEvalSet)
confusionMatrix(answersTwo,evalSet$classe)
table(answersTwo,evalSet$classe)

answersSubmit<-predict(rfFitTruncated, newTestDT[,-53,with=FALSE])

 pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


          
          
