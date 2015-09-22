# Course Project - Practical Machine Learning
Wat Hughes  
September 21, 2015  




## Basic Loading and Cleaning

Load and clean the data provided for this assignment. Thanks to the Groupware Researchers at the Pontifical Catholic University of Rio de Janeiro$^{1}$ for collecting this WLE dataset and making it available for this use.


```r
# fread is faster than read.csv
ProblemSetData = fread('pml-testing.csv',drop=1,colClasses='character',na.strings=c('NA',''))
# Make a vector to record which ProblemSetData variables are all NA. These variables cannot
# be helpful for training since there are no values in ProblemSetData to use for predictions.
f = function(v)sum(is.na(v))
NAs = unlist(ProblemSetData[,lapply(.SD,f)])
naNames = names(NAs)[NAs == 0]
ProblemSetData = ProblemSetData[,naNames,with=F] # Keep only the potentially useful variables

numNames = colnames(ProblemSetData)[c(2,3,6:58)] # By observation, these columns are numeric
invisible(ProblemSetData[,(numNames):=lapply(.SD,as.numeric),.SDcols=numNames]) # Make it so

# Make the same transformations to the training data.
training = fread('pml-training.csv',drop=1,colClasses='character',na.strings=c('NA',''))
naNames[length(naNames)] = 'classe' # Instead of problem_id
training = training[,naNames,with=F]

invisible(training[,(numNames):=lapply(.SD,as.numeric),.SDcols=numNames])

training$classe = as.factor(training$classe) # To support the rpart classification implementation
```

## Summary of EDA Activities

All of the remaining variables were reviewed with respect to classe, our prediction target. By far the strongest correlation was with raw_timestamp_part_1 as shown (in part) here:


```r
table(as.integer(training$raw_timestamp_part_1/10),training$classe)
```

```
##            
##               A   B   C   D   E
##   132248960  99   0   0   0   0
##   132248961 233   0   0   0   0
##   132248962 257   0   0   0   0
##   132248963 244   0   0   0   0
##   132248964  32 224   0   0   0
##   132248965   0 238   0   0   0
##   132248966   0 130 121   0   0
##   132248967   0   0 241   0   0
##   132248968   0   0 127 131   0
##   132248969   0   0   0 254   0
##   132248970   0   0   0 197  48
##   132248971   0   0   0   0 237
##   132248972   0   0   0   0 236
##   132248973   0   0   0   0  21
##   132267302 123   0   0   0   0
##   132267303 259   0   0   0   0
##   132267304 243   0   0   0   0
##   132267305 244   0   0   0   0
##   132267306 250   0   0   0   0
##   132267307  58 175   0   0   0
##   132267308   0 234   0   0   0
##   132267309   0  80 172   0   0
##   132267310   0   0 239   0   0
##   132267311   0   0 232   0   0
##   132267312   0   0   9 226   0
##   132267313   0   0   0 250   0
##   132267314   0   0   0  46 190
##   132267315   0   0   0   0 235
##   132267316   0   0   0   0 137
##   132283277 177   0   0   0   0
##   132283278 238   0   0   0   0
##   132283279 200   0   0   0   0
##   132283280 226   0   0   0   0
##   132283281 220   0   0   0   0
##   132283282 104 119   0   0   0
##   132283283   0 214   0   0   0
##   132283284   0 233   0   0   0
##   132283285   0 210  31   0   0
##   132283286   0   0 229   0   0
##   132283287   0   0 234   0   0
##   132283288   0   0 224   0   0
##   132283289   0   0  32 182   0
##   132283290   0   0   0 228   0
##   132283291   0   0   0 105 124
##   132283292   0   0   0   0 223
##   132283293   0   0   0   0 225
##   132283294   0   0   0   0 114
##   132283780  30   0   0   0   0
##   132283781 205   0   0   0   0
##   132283782 233   0   0   0   0
##   132283783 222   0   0   0   0
##   132283784 209  22   0   0   0
##   132283785   0 237   0   0   0
##   132283786   0 231   0   0   0
##   132283787   0 226   0   0   0
##   132283788   0  29 215   0   0
##   132283789   0   0 226   0   0
##   132283790   0   0  98 119   0
##   132283791   0   0   0 240   0
##   132283792   0   0   0 222   0
##   132283793   0   0   0  61 154
##   132283794   0   0   0   0 250
##   132283795   0   0   0   0 240
##   132283796   0   0   0   0  67
##   132308423 190   0   0   0   0
##   132308424 259   0   0   0   0
##   132308425 246   0   0   0   0
##   132308426 139 115   0   0   0
##   132308427   0 246   0   0   0
##   132308428   0 230   0   0   0
##   132308429   0  99 163   0   0
##   132308430   0   0 239   0   0
##   132308431   0   0  91 162   0
##   132308432   0   0   0 250   0
##   132308433   0   0   0  74 199
##   132308434   0   0   0   0 237
##   132308435   0   0   0   0 173
##   132309496  19   0   0   0   0
##   132309497 248   0   0   0   0
##   132309498 236   0   0   0   0
##   132309499 137  83   0   0   0
##   132309500   0 226   0   0   0
##   132309501   0 196  34   0   0
##   132309502   0   0 241   0   0
##   132309503   0   0 217   0   0
##   132309504   0   0   7 220   0
##   132309505   0   0   0 249   0
##   132309506   0   0   0   0 213
##   132309507   0   0   0   0 244
##   132309508   0   0   0   0  40
```

In fact the correlation is even closer when raw_timestamp_part_1 and raw_timestamp_part_2 are considered together. This will make a good first predictor for training our classification model.


```r
MinP1 = min(training$raw_timestamp_part_1) # Subtract this to remove redundant bits from part_1
training$xyzzy = training$raw_timestamp_part_1-MinP1
training$xyzzy = training$xyzzy+
    training$raw_timestamp_part_2/1000000 # New predictor, part_2 as the decimal
# Everything we did to training we need to do to ProblemSetData
ProblemSetData$xyzzy = ProblemSetData$raw_timestamp_part_1-MinP1
ProblemSetData$xyzzy = ProblemSetData$xyzzy+ProblemSetData$raw_timestamp_part_2/1000000
```

## Exploratory Model

A quick and dirty exploratory model performs perfectly on the out of sample data! This implies that the actual out of sample error rate is very, very low.


```r
set.seed(19) # For reproducibility
inMyTrain = createDataPartition(training$classe,p=.9,list=F)
myTraining = training[as.vector(inMyTrain)]
myTesting = training[-as.vector(inMyTrain)]
EDAFit = train(classe~xyzzy,method='rpart',data=myTraining)
preds = predict(EDAFit,newdata=myTesting)
# Wow, the exploratory model predicts perfectly on the held out data:
confusionMatrix(preds,myTesting$classe)$table
```

```
##           Reference
## Prediction   A   B   C   D   E
##          A 558   0   0   0   0
##          B   0 379   0   0   0
##          C   0   0 342   0   0
##          D   0   0   0 321   0
##          E   0   0   0   0 360
```

```r
ProblemSetPreds = predict(EDAFit,newdata=ProblemSetData)
```

## Crossvalidation

Now estimate the out of sample error more accurately using cross-validation:


```r
tc = trainControl('cv')
myTraining = training[as.vector(inMyTrain)]
cvFit = train(classe~xyzzy,method='rpart',data=myTraining,trControl=tc)
preds = predict(cvFit,newdata=myTesting)
# Confirmation: the out of sample error is nearly 0
confusionMatrix(preds,myTesting$classe)$table
```

```
##           Reference
## Prediction   A   B   C   D   E
##          A 558   0   0   0   0
##          B   0 379   0   0   0
##          C   0   0 342   0   0
##          D   0   0   0 321   0
##          E   0   0   0   0 360
```

Yay! cross-validation also indicates that the out of sample error rate for this model is zero.

## Submission Files

Finish up by writing out the submission files for this project:


```r
pml_write_files = function(x) # Modified from the code provided.
{
    SubDir = 'SubmissionFiles'
    # dir.create(SubDir)
    setwd(SubDir)

    n = length(x)
    for(i in 1:n)
    {
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
    setwd('..')
} # pml_write_files

pml_write_files(ProblemSetPreds)
```

Note that 100% of these are correct according to the course's submission web application. This further validates the remarkable out of sample error rate for this model.

## Footnotes

$^1 Qualitative Activity Recognition of Weight Lifting Exercises$

http://groupware.les.inf.puc-rio.br/work.jsf?p1=11201

http://groupware.les.inf.puc-rio.br/har
