# Practical Machine Learning
Antreas Antoniou  
1 Αυγούστου 2016  


```r
#We load the libraries and the data
library(caret);library(rattle);library(rpart.plot);library(rpart)
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```
## Rattle: A free graphical interface for data mining with R.
## Version 4.1.0 Copyright (c) 2006-2015 Togaware Pty Ltd.
## Type 'rattle()' to shake, rattle, and roll your data.
```

```
## Loading required package: rpart
```

```r
trn<-read.csv("pml-training.csv")
test<-read.csv("pml-testing.csv")
#We exclude the names and id
trn2<-subset(trn,select=-c(1:2))
#Then we cut the training data to 2 portions and create training and testing data
set.seed(99)
inTrain<- createDataPartition(y=trn2$classe,p=.6,list=FALSE)

training<-trn2[inTrain,]
testing<-trn2[-inTrain,]
dim(training); dim(testing)
```

```
## [1] 11776   158
```

```
## [1] 7846  158
```

```r
#Near Zero Variances
dtNZV<-nearZeroVar(training,saveMetrics=TRUE)
#we remove NZV
training<-training[,!dtNZV$nzv]

dim(training)
```

```
## [1] 11776   103
```


```r
#We remove variables that have more than 75% NAs
obs<-nrow(training)
vars<-ncol(training)

training2 <- training

for(i in 1:vars) {
        if( (sum(is.na( training[, i]) ) / obs ) >= .75 ){
                 for(j in 1:ncol(training2)) {
                         if ( names(training[i]) == names(training2[j]) ) {
                                training2 <- training2[ , -j]
                                break
                        }
                }
        }
}
dim(training2)
```

```
## [1] 11776    57
```

```r
training<-training2
```


```r
#We coerce the classes to match

test_<-test

for (i in 1:((length(training)-1) )) {
        class(test_[,names(training[i])])<-class(training[,i])
}       
```


```r
#We finally fit the model

fit<-rpart(classe~.,training)

print(fit,digits=4)
```

```
## n= 11776 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 11776 8428 A (0.28 0.19 0.17 0.16 0.18)  
##    2) cvtd_timestamp=02/12/2011 13:32,02/12/2011 13:33,02/12/2011 13:34,02/12/2011 14:56,02/12/2011 14:57,05/12/2011 11:23,05/12/2011 11:24,05/12/2011 14:22,05/12/2011 14:23,28/11/2011 14:13,28/11/2011 14:14,30/11/2011 17:10,30/11/2011 17:11 7483 4135 A (0.45 0.3 0.2 0.047 0)  
##      4) cvtd_timestamp=02/12/2011 13:32,02/12/2011 13:33,02/12/2011 14:56,05/12/2011 11:23,05/12/2011 14:22,28/11/2011 14:13,30/11/2011 17:10 2344  193 A (0.92 0.082 0 0 0)  
##        8) raw_timestamp_part_1< 1.323e+09 1719    0 A (1 0 0 0 0) *
##        9) raw_timestamp_part_1>=1.323e+09 625  193 A (0.69 0.31 0 0 0)  
##         18) cvtd_timestamp=02/12/2011 14:56,05/12/2011 11:23,05/12/2011 14:22 427    0 A (1 0 0 0 0) *
##         19) cvtd_timestamp=02/12/2011 13:33 198    5 B (0.025 0.97 0 0 0) *
##      5) cvtd_timestamp=02/12/2011 13:34,02/12/2011 14:57,05/12/2011 11:24,05/12/2011 14:23,28/11/2011 14:14,30/11/2011 17:11 5139 3072 B (0.23 0.4 0.3 0.069 0)  
##       10) magnet_dumbbell_z< 8.5 2584 1417 A (0.45 0.42 0.13 0.0027 0)  
##         20) raw_timestamp_part_1< 1.323e+09 2305 1138 A (0.51 0.47 0.02 0.003 0)  
##           40) magnet_dumbbell_x< -455.5 1238  310 A (0.75 0.21 0.03 0.0057 0)  
##             80) raw_timestamp_part_1< 1.323e+09 1052  124 A (0.88 0.095 0.016 0.0067 0) *
##             81) raw_timestamp_part_1>=1.323e+09 186   20 B (0 0.89 0.11 0 0) *
##           41) magnet_dumbbell_x>=-455.5 1067  248 B (0.22 0.77 0.0084 0 0)  
##             82) num_window< 68.5 177    0 A (1 0 0 0 0) *
##             83) num_window>=68.5 890   71 B (0.07 0.92 0.01 0 0) *
##         21) raw_timestamp_part_1>=1.323e+09 279    0 C (0 0 1 0 0) *
##       11) magnet_dumbbell_z>=8.5 2555 1360 C (0.012 0.38 0.47 0.14 0)  
##         22) magnet_dumbbell_x>=-462.5 763  185 B (0.0066 0.76 0.12 0.12 0) *
##         23) magnet_dumbbell_x< -462.5 1792  687 C (0.014 0.23 0.62 0.14 0)  
##           46) pitch_belt< -43.25 153   16 B (0 0.9 0.1 0 0) *
##           47) pitch_belt>=-43.25 1639  550 C (0.015 0.16 0.66 0.16 0)  
##             94) magnet_belt_z>=-447 1546  457 C (0.016 0.17 0.7 0.11 0) *
##             95) magnet_belt_z< -447 93    0 D (0 0 0 1 0) *
##    3) cvtd_timestamp=02/12/2011 13:35,02/12/2011 14:58,02/12/2011 14:59,05/12/2011 11:25,05/12/2011 14:24,28/11/2011 14:15,30/11/2011 17:12 4293 2128 E (0 0.0044 0.12 0.37 0.5)  
##      6) roll_belt< 125.5 3222 1654 D (0 0.0059 0.17 0.49 0.34)  
##       12) roll_dumbbell< -66.03 641  144 C (0 0.0078 0.78 0.086 0.13) *
##       13) roll_dumbbell>=-66.03 2581 1068 D (0 0.0054 0.014 0.59 0.39)  
##         26) accel_forearm_x< -100.5 1468  263 D (0 0.0095 0.012 0.82 0.16)  
##           52) magnet_belt_y>=578.5 1319  135 D (0 0.011 0.014 0.9 0.078) *
##           53) magnet_belt_y< 578.5 149   21 E (0 0 0 0.14 0.86) *
##         27) accel_forearm_x>=-100.5 1113  327 E (0 0 0.017 0.28 0.71)  
##           54) accel_dumbbell_y< 48.5 439  177 D (0 0 0.041 0.6 0.36) *
##           55) accel_dumbbell_y>=48.5 674   47 E (0 0 0.0015 0.068 0.93) *
##      7) roll_belt>=125.5 1071    7 E (0 0 0 0.0065 0.99) *
```


```r
fancyRpartPlot(fit)
```

![](practical_machine_learning_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



```r
#We test our algorithm
# in sample error
predictions<-predict(fit,training,type="class")
confusionMatrix(predictions,training$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 3251  100   17    7    0
##          B   72 1893  135   90    0
##          C   25  272 1865  220   84
##          D    0   14   36 1539  262
##          E    0    0    1   74 1819
## 
## Overall Statistics
##                                           
##                Accuracy : 0.8803          
##                  95% CI : (0.8744, 0.8862)
##     No Information Rate : 0.2843          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.8487          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9710   0.8306   0.9080   0.7974   0.8402
## Specificity            0.9853   0.9687   0.9382   0.9683   0.9922
## Pos Pred Value         0.9633   0.8644   0.7563   0.8314   0.9604
## Neg Pred Value         0.9885   0.9597   0.9797   0.9606   0.9650
## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2761   0.1608   0.1584   0.1307   0.1545
## Detection Prevalence   0.2866   0.1860   0.2094   0.1572   0.1608
## Balanced Accuracy      0.9782   0.8997   0.9231   0.8829   0.9162
```

```r
#out of sample error
predictions1<-predict(fit,testing,type="class")
confusionMatrix(predictions1,testing$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2149   59    6    3    0
##          B   73 1285  100   63    0
##          C   10  165 1241  134   56
##          D    0    9   20 1002  163
##          E    0    0    1   84 1223
## 
## Overall Statistics
##                                          
##                Accuracy : 0.8794         
##                  95% CI : (0.872, 0.8866)
##     No Information Rate : 0.2845         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.8475         
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9628   0.8465   0.9072   0.7792   0.8481
## Specificity            0.9879   0.9627   0.9437   0.9707   0.9867
## Pos Pred Value         0.9693   0.8448   0.7727   0.8392   0.9350
## Neg Pred Value         0.9853   0.9632   0.9796   0.9573   0.9665
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2739   0.1638   0.1582   0.1277   0.1559
## Detection Prevalence   0.2826   0.1939   0.2047   0.1522   0.1667
## Balanced Accuracy      0.9754   0.9046   0.9254   0.8749   0.9174
```




```r
#We find the predictions for the "real" data

predictions_final<-predict(fit,test_,type="class")
predictions_final
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  C  A  A  E  D  C  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```


