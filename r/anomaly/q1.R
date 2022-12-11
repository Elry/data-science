library(e1071);
library(ggplot2);

RNGversion("3.5.2");
set.seed(1987);

wdbc <- read.csv(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",
  col.names=c(
    "patientid",
    "outcome",
    "radius_mean",
    "texture_mean",
    "perimeter_mean",
    "area_mean",
    "smoothness_mean",
    "compactness_mean",
    "concavity_mean",
    "concavepoints_mean",
    "symmetry_mean",
    "fractaldimension_mean",
    "radius_error",
    "texture_error",
    "perimeter_error",
    "area_error",
    "smoothness_error",
    "compactness_error",
    "concavity_error",
    "concavepoints_error",
    "symmetry_error",
    "fractaldimension_error",
    "radius_worst",
    "texture_worst",
    "perimeter_worst",
    "area_worst",
    "smoothness_worst",
    "compactness_worst",
    "concavity_worst",
    "concavepoints_worst",
    "symmetry_worst",
    "fractaldimension_worst")
);

wdbc <- as.data.frame(unclass(wdbc),stringsAsFactors=T)

head(wdbc);
summary(wdbc);
any(is.na(wdbc));

mybreast <- wdbc[,!(names(wdbc) %in% c("patientid"))];

# Benign & malignant data
table(mybreast$outcome);

# Anomaly detection
# outcome to numeric
mybreast$outcome = as.factor(as.numeric(mybreast$outcome) - 1);

# Separating benign from malignant data
mybreast_B <- mybreast[mybreast$outcome == 0,];
mybreast_M <- mybreast[mybreast$outcome == 1,];

# nr row check for B/M
nrow(mybreast_B); 
nrow(mybreast_M);

svm <- svm(
  outcome~.,
  data = mybreast_B, 
  scale = TRUE,
  kernel = "radial",
  type = "one-classification"
);

print(svm);
summary(svm);

predict_test <- predict(svm, mybreast_M);
table(predict_test);

cat('Anomaly Detected (FALSE):', table(predict_test)[1]/sum(table(predict_test))*100, ' %');

svm2 <- svm(
  outcome~.,
  data = mybreast,
  scale = TRUE,
  kernel = "radial"
);

summary(svm2);

predict_test2 <- predict(svm2, mybreast_M);

c_matrix = table(predict_test2, mybreast_M$outcome)
print(c_matrix);

acc <- sum(diag(c_matrix))/sum(c_matrix)*100;
cat('Accuracy: ', acc, ' %', "\n")
