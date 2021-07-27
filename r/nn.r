library(neuralnet)

set.seed(111)

data <- iris

unique(data$Species)
colnames(data)

data$setosa <- data$Species == 'setosa'
data$virginica <- data$Species == 'virginica'
data$versicolor <- data$Species == 'versicolor'

ex <- sample(1:nrow(data), nrow(data)*0.8)
train <- data[ex,]
test <- data[-ex,]

neuralNetwork <- neuralnet(
  setosa +
  versicolor +
  virginica ~
  Sepal.Length +
  Sepal.Width +
  Petal.Length + 
  Petal.Width,
  train,
  hidden = c(5,4)
)

x11()

plot(neuralNetwork,
  col.entry='orange',
  col.entry.synapse = 'orange',
  col.out='purple',
  col.out.synapse = 'purple',
  col.hidden='darkgreen',
  col.hidden.synapse = 'darkgreen',
  col.intercept='navy',
  show.weights=F,
  information= T,
  rep = 'best'
)

Sys.sleep(999)