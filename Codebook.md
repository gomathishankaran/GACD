Codebook

List of files from the downloaded zip file:
 
 [1] "activity_labels.txt"                         
 [2] "features.txt"                                
 [3] "features_info.txt"                           
 [4] "README.txt"                                  
 [5] "test/Inertial Signals/body_acc_x_test.txt"   
 [6] "test/Inertial Signals/body_acc_y_test.txt"   
 [7] "test/Inertial Signals/body_acc_z_test.txt"   
 [8] "test/Inertial Signals/body_gyro_x_test.txt"  
 [9] "test/Inertial Signals/body_gyro_y_test.txt"  
[10] "test/Inertial Signals/body_gyro_z_test.txt"  
[11] "test/Inertial Signals/total_acc_x_test.txt"  
[12] "test/Inertial Signals/total_acc_y_test.txt"  
[13] "test/Inertial Signals/total_acc_z_test.txt"  
[14] "test/subject_test.txt"                       
[15] "test/X_test.txt"                             
[16] "test/y_test.txt"                             
[17] "train/Inertial Signals/body_acc_x_train.txt" 
[18] "train/Inertial Signals/body_acc_y_train.txt" 
[19] "train/Inertial Signals/body_acc_z_train.txt" 
[20] "train/Inertial Signals/body_gyro_x_train.txt"
[21] "train/Inertial Signals/body_gyro_y_train.txt"
[22] "train/Inertial Signals/body_gyro_z_train.txt"
[23] "train/Inertial Signals/total_acc_x_train.txt"
[24] "train/Inertial Signals/total_acc_y_train.txt"
[25] "train/Inertial Signals/total_acc_z_train.txt"
[26] "train/subject_train.txt"                     
[27] "train/X_train.txt"                           
[28] "train/y_train.txt"   

Variable list and descriptions(got from the files above)

Variable name 	Description
subject 	ID the subject who performed the activity for each window sample. Its                 range is from 1 to 30.
activity 	Activity name
featDomain 	Feature: Time domain signal or frequency domain signal (Time or Freq)
featInstrument 	Feature: Measuring instrument (Accelerometer or Gyroscope)
featAcceleration 	Feature: Acceleration signal (Body or Gravity)
featVariable 	Feature: Variable (Mean or SD)
featJerk 	Feature: Jerk signal
featMagnitude 	Feature: Magnitude of the signals calculated using the Euclidean norm
featAxis 	Feature: 3-axial signals in the X, Y and Z directions (X, Y, or Z)
featCount 	Feature: Count of data points used to compute average
featAverage 	Feature: Average of each variable for each activity and each subject

Different features heading given by
head(Features)
   featureNum       featureName        featureCode
          1       tBodyAcc-mean()-X          V1
          2       tBodyAcc-mean()-Y          V2
          3       tBodyAcc-mean()-Z          V3
          4       tBodyAcc-std()-X           V4
          5       tBodyAcc-std()-Y           V5
          6       tBodyAcc-std()-Z           V6


Dataset structure given by:
str(Tidy)

Classes ‘data.table’ and 'data.frame':  11880 obs. of  11 variables:
 $ subject         : int  1 1 1 1 1 1 1 1 1 1 ...
 $ activity        : Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ featDomain      : Factor w/ 2 levels "Time","Freq": 1 1 1 1 1 1 1 1 1 1 ...
 $ featAcceleration: Factor w/ 3 levels NA,"Body","Gravity": 1 1 1 1 1 1 1 1 1 1 ...
 $ featInstrument  : Factor w/ 2 levels "Accelerometer",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ featJerk        : Factor w/ 2 levels NA,"Jerk": 1 1 1 1 1 1 1 1 2 2 ...
 $ featMagnitude   : Factor w/ 2 levels NA,"Magnitude": 1 1 1 1 1 1 2 2 1 1 ...
 $ featVariable    : Factor w/ 2 levels "Mean","SD": 1 1 1 2 2 2 1 2 1 1 ...
 $ featAxis        : Factor w/ 4 levels NA,"X","Y","Z": 2 3 4 2 3 4 1 1 2 3 ...
 $ count           : int  50 50 50 50 50 50 50 50 50 50 ...
 $ average         : num  -0.0166 -0.0645 0.1487 -0.8735 -0.9511 ...
 - attr(*, "sorted")= chr  "subject" "activity" "featDomain" "featAcceleration" ...
 - attr(*, ".internal.selfref")=<externalptr> 

Key variables in the data table given by:

key(Tidy)

[1] "subject"          "activity"         "featDomain"      
[4] "featAcceleration" "featInstrument"   "featJerk"        
[7] "featMagnitude"    "featVariable"     "featAxis"

Few rows of the dataset given by:

Tidy

[1] "subject"          "activity"         "featDomain"       "featAcceleration"
[5] "featInstrument"   "featJerk"         "featMagnitude"    "featVariable"    
[9] "featAxis"

head(Tidy)

   subject activity featDomain featAcceleration featInstrument featJerk
1:       1   LAYING       Time               NA      Gyroscope       NA
2:       1   LAYING       Time               NA      Gyroscope       NA
3:       1   LAYING       Time               NA      Gyroscope       NA
4:       1   LAYING       Time               NA      Gyroscope       NA
5:       1   LAYING       Time               NA      Gyroscope       NA
6:       1   LAYING       Time               NA      Gyroscope       NA
   featMagnitude featVariable featAxis count     average
1:            NA         Mean        X    50 -0.01655309
2:            NA         Mean        Y    50 -0.06448612
3:            NA         Mean        Z    50  0.14868944
4:            NA           SD        X    50 -0.87354387
5:            NA           SD        Y    50 -0.95109044
6:            NA           SD        Z    50 -0.90828466

tail(Tidy)

   subject         activity featDomain featAcceleration featInstrument featJerk
1:      30 WALKING_UPSTAIRS       Freq             Body  Accelerometer     Jerk
2:      30 WALKING_UPSTAIRS       Freq             Body  Accelerometer     Jerk
3:      30 WALKING_UPSTAIRS       Freq             Body  Accelerometer     Jerk
4:      30 WALKING_UPSTAIRS       Freq             Body  Accelerometer     Jerk
5:      30 WALKING_UPSTAIRS       Freq             Body  Accelerometer     Jerk
6:      30 WALKING_UPSTAIRS       Freq             Body  Accelerometer     Jerk
   featMagnitude featVariable featAxis count    average
1:            NA         Mean        Z    65 -0.7378039
2:            NA           SD        X    65 -0.5615652
3:            NA           SD        Y    65 -0.6108266
4:            NA           SD        Z    65 -0.7847539
5:     Magnitude         Mean       NA    65 -0.5497849
6:     Magnitude           SD       NA    65 -0.5808781



Summary of variables of given by

summary(dtTidy)

    subject                   activity    featDomain  featAcceleration
 Min.   : 1.0   LAYING            :1980   Time:7200   NA     :4680    
 1st Qu.: 8.0   SITTING           :1980   Freq:4680   Body   :5760    
 Median :15.5   STANDING          :1980               Gravity:1440    
 Mean   :15.5   WALKING           :1980                               
 3rd Qu.:23.0   WALKING_DOWNSTAIRS:1980                               
 Max.   :30.0   WALKING_UPSTAIRS  :1980                               
       featInstrument featJerk      featMagnitude  featVariable featAxis 
 Accelerometer:7200   NA  :7200   NA       :8640   Mean:5940    NA:3240  
 Gyroscope    :4680   Jerk:4680   Magnitude:3240   SD  :5940    X :2880  
                                                                Y :2880  
                                                                Z :2880  
                                                                         
                                                                         
     count          average        
 Min.   :36.00   Min.   :-0.99767  
 1st Qu.:49.00   1st Qu.:-0.96205  
 Median :54.50   Median :-0.46989  
 Mean   :57.22   Mean   :-0.48436  
 3rd Qu.:63.25   3rd Qu.:-0.07836  
 Max.   :95.00   Max.   : 0.97451  


All possible combinations of features given by

dtTidy[, .N, by = c(names(dtTidy)[grep("^feat", names(dtTidy))])]

    featDomain featAcceleration featInstrument featJerk featMagnitude
 1:       Time               NA      Gyroscope       NA            NA
 2:       Time               NA      Gyroscope       NA            NA
 3:       Time               NA      Gyroscope       NA            NA
 4:       Time               NA      Gyroscope       NA            NA
 5:       Time               NA      Gyroscope       NA            NA
 6:       Time               NA      Gyroscope       NA            NA
 7:       Time               NA      Gyroscope       NA     Magnitude
 8:       Time               NA      Gyroscope       NA     Magnitude
 9:       Time               NA      Gyroscope     Jerk            NA
10:       Time               NA      Gyroscope     Jerk            NA
11:       Time               NA      Gyroscope     Jerk            NA
12:       Time               NA      Gyroscope     Jerk            NA
13:       Time               NA      Gyroscope     Jerk            NA
14:       Time               NA      Gyroscope     Jerk            NA
15:       Time               NA      Gyroscope     Jerk     Magnitude
16:       Time               NA      Gyroscope     Jerk     Magnitude
17:       Time             Body  Accelerometer       NA            NA
18:       Time             Body  Accelerometer       NA            NA
19:       Time             Body  Accelerometer       NA            NA
20:       Time             Body  Accelerometer       NA            NA
21:       Time             Body  Accelerometer       NA            NA
22:       Time             Body  Accelerometer       NA            NA
23:       Time             Body  Accelerometer       NA     Magnitude
24:       Time             Body  Accelerometer       NA     Magnitude
25:       Time             Body  Accelerometer     Jerk            NA
26:       Time             Body  Accelerometer     Jerk            NA
27:       Time             Body  Accelerometer     Jerk            NA
28:       Time             Body  Accelerometer     Jerk            NA
29:       Time             Body  Accelerometer     Jerk            NA
30:       Time             Body  Accelerometer     Jerk            NA
31:       Time             Body  Accelerometer     Jerk     Magnitude
32:       Time             Body  Accelerometer     Jerk     Magnitude
33:       Time          Gravity  Accelerometer       NA            NA
34:       Time          Gravity  Accelerometer       NA            NA
35:       Time          Gravity  Accelerometer       NA            NA
36:       Time          Gravity  Accelerometer       NA            NA
37:       Time          Gravity  Accelerometer       NA            NA
38:       Time          Gravity  Accelerometer       NA            NA
39:       Time          Gravity  Accelerometer       NA     Magnitude
40:       Time          Gravity  Accelerometer       NA     Magnitude
41:       Freq               NA      Gyroscope       NA            NA
42:       Freq               NA      Gyroscope       NA            NA
43:       Freq               NA      Gyroscope       NA            NA
44:       Freq               NA      Gyroscope       NA            NA
45:       Freq               NA      Gyroscope       NA            NA
46:       Freq               NA      Gyroscope       NA            NA
47:       Freq               NA      Gyroscope       NA     Magnitude
48:       Freq               NA      Gyroscope       NA     Magnitude
49:       Freq               NA      Gyroscope     Jerk     Magnitude
50:       Freq               NA      Gyroscope     Jerk     Magnitude
51:       Freq             Body  Accelerometer       NA            NA
52:       Freq             Body  Accelerometer       NA            NA
53:       Freq             Body  Accelerometer       NA            NA
54:       Freq             Body  Accelerometer       NA            NA
55:       Freq             Body  Accelerometer       NA            NA
56:       Freq             Body  Accelerometer       NA            NA
57:       Freq             Body  Accelerometer       NA     Magnitude
58:       Freq             Body  Accelerometer       NA     Magnitude
59:       Freq             Body  Accelerometer     Jerk            NA
60:       Freq             Body  Accelerometer     Jerk            NA
61:       Freq             Body  Accelerometer     Jerk            NA
62:       Freq             Body  Accelerometer     Jerk            NA
63:       Freq             Body  Accelerometer     Jerk            NA
64:       Freq             Body  Accelerometer     Jerk            NA
65:       Freq             Body  Accelerometer     Jerk     Magnitude
66:       Freq             Body  Accelerometer     Jerk     Magnitude
    featDomain featAcceleration featInstrument featJerk featMagnitude
    featVariable featAxis   N
 1:         Mean        X 180
 2:         Mean        Y 180
 3:         Mean        Z 180
 4:           SD        X 180
 5:           SD        Y 180
 6:           SD        Z 180
 7:         Mean       NA 180
 8:           SD       NA 180
 9:         Mean        X 180
10:         Mean        Y 180
11:         Mean        Z 180
12:           SD        X 180
13:           SD        Y 180
14:           SD        Z 180
15:         Mean       NA 180
16:           SD       NA 180
17:         Mean        X 180
18:         Mean        Y 180
19:         Mean        Z 180
20:           SD        X 180
21:           SD        Y 180
22:           SD        Z 180
23:         Mean       NA 180
24:           SD       NA 180
25:         Mean        X 180
26:         Mean        Y 180
27:         Mean        Z 180
28:           SD        X 180
29:           SD        Y 180
30:           SD        Z 180
31:         Mean       NA 180
32:           SD       NA 180
33:         Mean        X 180
34:         Mean        Y 180
35:         Mean        Z 180
36:           SD        X 180
37:           SD        Y 180
38:           SD        Z 180
39:         Mean       NA 180
40:           SD       NA 180
41:         Mean        X 180
42:         Mean        Y 180
43:         Mean        Z 180
44:           SD        X 180
45:           SD        Y 180
46:           SD        Z 180
47:         Mean       NA 180
48:           SD       NA 180
49:         Mean       NA 180
50:           SD       NA 180
51:         Mean        X 180
52:         Mean        Y 180
53:         Mean        Z 180
54:           SD        X 180
55:           SD        Y 180
56:           SD        Z 180
57:         Mean       NA 180
58:           SD       NA 180
59:         Mean        X 180
60:         Mean        Y 180
61:         Mean        Z 180
62:           SD        X 180
63:           SD        Y 180
64:           SD        Z 180
65:         Mean       NA 180
66:           SD       NA 180
    featVariable featAxis   N

Save tidy data to file

Save data table objects to a tab-delimited text file HumanActivityPhones.txt.

f <- file.path(path, "HumanActivityPhones.txt")
write.table(dtTidy, f, quote = FALSE, sep = "\t", row.names = FALSE)
