library(e1071);
library(party);

mushs = read.csv(
  "https://www.openml.org/data/get_csv/24/dataset_24_mushroom.arff",
  header = T,
  stringsAsFactors = TRUE
);

RNGversion("3.5.2");
set.seed(1987);

mushs[mushs[] == "?"] <- NA;
mushs <- na.omit(mushs);

L <- sample(1:nrow(mushs), round(nrow(mushs)/3));
train <- mushs[-L,];
test <- mushs[L,];

fitBayes <- naiveBayes(
  class~.,
  data = train,
  laplace = 1
);

predBayes <- predict(fitBayes, test);

c_matrix <- table(predBayes, test$class);

cat('Accuracy Bayes: ', sum(diag(c_matrix))/sum(c_matrix)*100, ' %', "\n");

fitTree <- ctree(class~., data = train);
predTree <- predict(fitTree, test);

c_matrix <- table(predTree, test$class);
print(c_matrix);

cat('Accuracy Dtree: ', sum(diag(c_matrix))/sum(c_matrix)*100, ' %', "\n");

plot(fitTree)
