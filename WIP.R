library(data.table)
library(caret)
library(rpart)

# training = fread('pml-training.csv')
str(training)
# Classes ‘data.table’ and 'data.frame':	19622 obs. of  160 variables:
#  $ V1                      : chr  "1" "2" "3" "4" ... # by observation row number so drop it
#  $ user_name               : chr  "carlitos" "carlitos" "carlitos" "carlitos" ...
#  $ raw_timestamp_part_1    : int  1323084231 1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
#  $ raw_timestamp_part_2    : int  788290 808298 820366 120339 196328 304277 368296 440390 484323 484434 ...
#  $ cvtd_timestamp          : chr  "05/12/2011 11:23" "05/12/2011 11:23" "05/12/2011 11:23" "05/12/2011 11:23" ...
#  $ new_window              : chr  "no" "no" "no" "no" ...
#  $ num_window              : int  11 11 11 12 12 12 12 12 12 12 ...
#  $ roll_belt               : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
summary(training)
training$V1

# testing = fread('pml-testing.csv')
str(testing)
# Classes ‘data.table’ and 'data.frame':	20 obs. of  160 variables:
#  $ V1                      : chr  "1" "2" "3" "4" ...
#  $ user_name               : chr  "pedro" "jeremy" "jeremy" "adelmo" ...
#  $ raw_timestamp_part_1    : int  1323095002 1322673067 1322673075 1322832789 1322489635 1322673149 1322673128 1322673076 1323084240 1322837822 ...
#  $ raw_timestamp_part_2    : int  868349 778725 342967 560311 814776 510661 766645 54671 916313 384285 ...
#  $ cvtd_timestamp          : chr  "05/12/2011 14:23" "30/11/2011 17:11" "30/11/2011 17:11" "02/12/2011 13:33" ...
#  $ new_window              : chr  "no" "no" "no" "no" ...
#  $ num_window              : int  74 431 439 194 235 504 485 440 323 664 ...
#  $ roll_belt               : num  123 1.02 0.87 125 1.35 -5.92 1.2 0.43 0.93 114 ...
summary(testing)
testing$V1

# testing = fread('pml-testing.csv',drop=1,stringsAsFactors=T)
training = fread('pml-training.csv',drop=1)
testing = fread('pml-testing.csv',drop=1)
str(testing)
# Classes ‘data.table’ and 'data.frame':	20 obs. of  159 variables:
#  $ user_name               : chr  "pedro" "jeremy" "jeremy" "adelmo" ...
#  $ raw_timestamp_part_1    : int  1323095002 1322673067 1322673075 1322832789 1322489635 1322673149 1322673128 1322673076 1323084240 1322837822 ...
#  $ raw_timestamp_part_2    : int  868349 778725 342967 560311 814776 510661 766645 54671 916313 384285 ...
#  $ cvtd_timestamp          : chr  "05/12/2011 14:23" "30/11/2011 17:11" "30/11/2011 17:11" "02/12/2011 13:33" ...
#  $ new_window              : chr  "no" "no" "no" "no" ...
#  $ num_window              : int  74 431 439 194 235 504 485 440 323 664 ...
#  $ roll_belt               : num  123 1.02 0.87 125 1.35 -5.92 1.2 0.43 0.93 114 .summary(testing)
summary(testing)

table(training$user_name)
#   adelmo carlitos  charles   eurico   jeremy    pedro
#     3892     3112     3536     3070     3402     2610

table(training$classe)
#    A    B    C    D    E
# 5580 3797 3422 3216 3607

table(training$user_name,training$classe)
#               A    B    C    D    E
#   adelmo   1165  776  750  515  686
#   carlitos  834  690  493  486  609
#   charles   899  745  539  642  711
#   eurico    865  592  489  582  542
#   jeremy   1177  489  652  522  562
#   pedro     640  505  499  469  497

table(training$cvtd_timestamp,training$classe)
#                      A   B   C   D   E
#   02/12/2011 13:32 177   0   0   0   0
#   02/12/2011 13:33 988 333   0   0   0
#   02/12/2011 13:34   0 443 750 182   0
#   02/12/2011 13:35   0   0   0 333 686
#   02/12/2011 14:56 235   0   0   0   0
#   02/12/2011 14:57 664 716   0   0   0
#   02/12/2011 14:58   0  29 539 642 154
#   02/12/2011 14:59   0   0   0   0 557
#   05/12/2011 11:23 190   0   0   0   0
#   05/12/2011 11:24 644 690 163   0   0
#   05/12/2011 11:25   0   0 330 486 609
#   05/12/2011 14:22 267   0   0   0   0
#   05/12/2011 14:23 373 505 492   0   0
#   05/12/2011 14:24   0   0   7 469 497
#   28/11/2011 14:13 833   0   0   0   0
#   28/11/2011 14:14  32 592 489 385   0
#   28/11/2011 14:15   0   0   0 197 542
#   30/11/2011 17:10 869   0   0   0   0
#   30/11/2011 17:11 308 489 643   0   0
#   30/11/2011 17:12   0   0   9 522 562

table(training$cvtd_timestamp,training$user_name)
#                    adelmo carlitos charles eurico jeremy pedro
#   02/12/2011 13:32    177        0       0      0      0     0
#   02/12/2011 13:33   1321        0       0      0      0     0
#   02/12/2011 13:34   1375        0       0      0      0     0
#   02/12/2011 13:35   1019        0       0      0      0     0
#   02/12/2011 14:56      0        0     235      0      0     0
#   02/12/2011 14:57      0        0    1380      0      0     0
#   02/12/2011 14:58      0        0    1364      0      0     0
#   02/12/2011 14:59      0        0     557      0      0     0
#   05/12/2011 11:23      0      190       0      0      0     0
#   05/12/2011 11:24      0     1497       0      0      0     0
#   05/12/2011 11:25      0     1425       0      0      0     0
#   05/12/2011 14:22      0        0       0      0      0   267
#   05/12/2011 14:23      0        0       0      0      0  1370
#   05/12/2011 14:24      0        0       0      0      0   973
#   28/11/2011 14:13      0        0       0    833      0     0
#   28/11/2011 14:14      0        0       0   1498      0     0
#   28/11/2011 14:15      0        0       0    739      0     0
#   30/11/2011 17:10      0        0       0      0    869     0
#   30/11/2011 17:11      0        0       0      0   1440     0
#   30/11/2011 17:12      0        0       0      0   1093     0

testing$cvtd_timestamp
#  [1] "05/12/2011 14:23" "30/11/2011 17:11" "30/11/2011 17:11" "02/12/2011 13:33" "28/11/2011 14:13"
#  [6] "30/11/2011 17:12" "30/11/2011 17:12" "30/11/2011 17:11" "05/12/2011 11:24" "02/12/2011 14:57"
# [11] "05/12/2011 11:24" "30/11/2011 17:11" "28/11/2011 14:14" "30/11/2011 17:10" "30/11/2011 17:12"
# [16] "28/11/2011 14:15" "05/12/2011 14:22" "05/12/2011 11:24" "05/12/2011 14:23" "28/11/2011 14:14"

table(training$raw_timestamp_part_1,training$classe)

# user_name, raw_timestampe_part_1, ...part_2, cvtd_timestamp, new_window
#colclasses = c(NULL, 'character','integer','integer','character','character')
# colclasses = c(colclasses, rep('numeric',160-length(colclasses))) # Damn "" for NA in some fields
colclasses = c(colclasses, rep('character',160-length(colclasses)))
# testing = fread('pml-testing.csv',drop=1,colClasses=colclasses)
testing = fread('pml-testing.csv',drop=1,colClasses='character',na.strings=c("NA",""))

# testing$amplitude_yaw_forearm # ick, instead of NA is "", overriding colClasses


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
str(testing)
summary(testing)
# testing[,6:159]=as.numeric(testing[,6:159])
for(i in 6:159) testing[,i]=as.numeric(testing[,i])

testing$problem_id
testing[,problem_id:=as.numeric(problem_id)]


OpsList = vector('list',159-6)
for(i in 6:159)
{
    cn = colnames(testing)[i]
    OpsList[[i-5]] = paste0(cn,':=as.numeric(',cn,')')
}
testing[,OpsList]
testing[,list("magnet_forearm_x:=as.numeric(magnet_forearm_x)")]
summary(testing)

numNames = colnames(testing)[c(2,3,6:159)]
testing[,(numNames):=lapply(.SD,as.numeric),.SDcols=numNames]
summary(testing)
f=function(v)sum(is.na(v))
NAs=testing[,lapply(.SD,f)]
str(NAs)
NAs = unlist(NAs)
naNames = names(NAs)[NAs == 0]
testing = testing[,naNames,with=F]
summary(testing)
table(training$user_name)
