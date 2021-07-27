library(e1071);
library(party);

credit = read.csv(
  "https://www.openml.org/data/get_csv/29/dataset_29_credit-a.arff",
  header = T,
  stringsAsFactors = TRUE
);

RNGversion("3.5.2");
set.seed(1987);

credit[credit[] == "?"] <- NA;
credit <- na.omit(credit);

L <- sample(1:nrow(credit), round(nrow(credit)/3));
train <- credit[-L,];
test <- credit[L,];

fitBayes = naiveBayes(class~A9+A5+A14+A10, data=train,laplace=1);

predBayes = predict(fitBayes, test);
predBayes;

c_matrix = table(predBayes, test$class);
print(c_matrix);

cat('Accuracy Bayes: ', sum(diag(c_matrix))/sum(c_matrix)*100, ' %', "\n");

fitTree = ctree(class~A9+A5+A14+A10, data=train);

predTree = predict(fitTree, test);
predTree;

c_matrix <- table(predTree, test$class);
print(c_matrix);

cat('Accuracy Dtree: ', sum(diag(c_matrix))/sum(c_matrix)*100, ' %', "\n");

plot(fitTree)
