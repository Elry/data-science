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

table(mybreast$outcome);

T <- sample(1:nrow(my_breast), round(0.3*nrow(my_breast)));

my_breast_test <- my_breast[T,];
my_breast_train <- my_breast[-T,];

# Q1 Quantos atributos e linhas nos conjuntos de treinamento e teste? (check)
ncol(my_breast_test)
nrow(my_breast_test)
ncol(my_breast_train)
nrow(my_breast_train)

# Fa�a o Treinamento com o conjunto de dados de treinamento

# SVM radial
svm_radial <- svm(
  outcome~.,
  data = mybreast_B, 
  scale = TRUE,
  kernel = "radial",
  type = "one-classification"
);

# exploring
print(svm_radial)
summary(svm_radial)

# SVM Polinomial
svm_polynomial <- svm(
  outcome~.,
  data = mybreast_B, 
  scale = TRUE,
  kernel = "polinomial",
  type = "one-classification"
);

# exploring
print(svm_polynomial)
summary(svm_polynomial)

# SVM Linear
svm_linear  <- svm(
  outcome~.,
  data = mybreast_B, 
  scale = TRUE,
  kernel = "linear",
  type = "one-classification"
);

# exploring
print(svm_linear)
summary(svm_linear)

# SVM sigmoid
svm_sigmoid  <- svm(
  outcome~.,
  data = mybreast_B, 
  scale = TRUE,
  kernel = "sigmoid",
  type = "one-classification"
);

# exploring
print(svm_sigmoid)
summary(svm_sigmoid)

# Fa�a a predi��o do conjunto de dados de teste 
# Q5 Qual a acuracidade da SVM na classifica��o dos tumores quando 
# aplicada diretamente aos dados para cada um dos kernels? 
svm <- svm(
  outcome~.,
  data = mybreast_B, 
  scale = TRUE,
  kernel = "radial",
  type = "one-classification"
);

predict_test <- predict(svm, mybreast_M);

c_matrix <- table(my_breast_test$outcome, predict_test);
print(c_matrix);

cat('Accuracy kernel type (', svm$kernel, ") =", sum(diag(c_matrix))/sum(c_matrix)*100, ' %')

# Parte b. Classificando os dados ap�s aplicar o PCA 
# Agora, vamos transformar a base my_breast, em uma base com atributos 
# de componente principais e, ent�o, aplicar o classificador SVM para
# a um conjunto de atributos reduzido para compararmos os resultados
#

#
# Passo 1. � transformar o dataframe my_breast em um dataframe de componentes
# principais my_breast_col_pc (com as colunas de componentes principais)
#

#
# Construindo my_breast em PC

# 
# Voc� pode empregar princomp ou principal. A entrada tem de eliminar 
# o atributo "outcome". Ele � n�o num�rico e a classe de treinamento, 
# n�o � portanto um atributo para redu��o a componentes principais
# 
# Consulte o help() dos comandos
#

breast_pc = princomp(my_breast, cor=TRUE);
# breast_pc = principal(my_breast[,-1], nfactors=......)

#
# Explore a reconstru��o em PCs

# biplot(breast_pc) # opcional
names(breast_pc);
summary(breast_pc);

# Q6 Quantos componentes s�o necess�rios para se ter uma representa��o com MAIS
# de 75% da variancia dos dados? E com MAIS 90% da variancia dos dados?   

# Os scores s�o os novos atributos com base em componentes principais.
# Construa agora a base com os componentes principais, adicionado o atributo classe 
breast_col_pc = data.frame(breast_pc$scores); 

# vamos readicionar o aributo classe "outcome" 
breast_col_pc = cbind(wdbc$outcome, breast_col_pc);
names(breast_col_pc)[1] = "outcome";

#
# Explore my_breast e breast_col_pc, a constru�da com PCs 
#
# S�o os mesmos conjuntos de dados, mas a base em PCs tem os dados rotacionados  
#

#
# Note que as bases tem dados completamente diferentes. my_breast, os dados originais
# e breast_col_pc com os atributos que s�o os componentes principais
#
# Q7 Quantos componentes principais foram gerados?  
# Q8 Considere os 3 primeiros atributos de cada conjunto de dados (o original e o 
# de componentes principais) e a matriz de correla��o de cada conjunto. Quais as 
# correla��es observadas?
#

head(my_breast[,1:4]);
cor(my_breast[,2:4]);

head(breast_col_pc[,1:4]);
cor(breast_col_pc[,2:4]);

#
# Passo 2. � classificar os dados com base nos Componentes Principais
#
# Vamos primeiramente empregar 3 componentes, em seguida 7 

# 
# Construindo os conjuntos de treinamento e teste...

# Empregar a mesma sele��o de dados T empregada anteriormente 
#

my_breast_col_pc_test <- breast_col_pc[T,];
my_breast_col_pc_train <- breast_col_pc[-T,];


# Treinamento da SVM com os dados de treinamento para 3 componentes 
# ...[,c(1:4)] = "outcome","Comp.1","Comp.2","Comp.3"
#
# Vamos empregar somente o kernel com base radial

svm_3 <- svm(
  outcome~.,
  data = .....[,c(1:4)], 
  scale = TRUE,
  kernel ="radial"
);

# Explore o modelo obtido
# check, 97 vetores de suporte devem ser formados 
print(svm_3)
summary(svm_3)

# Predi��o do conjunto de testes 
predict_test = predict(svm_3, ......) 

c_matrix=table(my_breast_col_pc_test$outcome, ......)
print(c_matrix)

cat('Accuracy (3 PCs): ', sum(diag(c_matrix))/sum(c_matrix)*100, ' %')


# Treinamento da SVM com os dados de treinamento para 7 componentes 
# ...[,c(1:8)] = "outcome","Comp.1","Comp.2",...,"Comp.7"
#
# Vamos empregar somente o kernel com base radial

svm_7 <- svm(outcome ~ ., data = ...... , 
             scale = TRUE, kernel ="radial")

# Explore o modelo obtido
#

# check, 130 vetores de suporte devem ser formados 
#
print(svm_7)
summary(svm_7)

# Predi��o do conjunto de testes 
#

predict_test = predict(svm_7, ......) 

c_matrix=table(my_breast_col_pc_test$outcome, ......)
print(c_matrix)


cat('Accuracy (7 PCs): ', sum(diag(c_matrix))/sum(c_matrix)*100, ' %')

# Q9 Qual a acur�cia produzida pela classifica��o com 3 e 7 componentes principais?
#

# Q10 Qual a sua conclus�o sobre a acur�cia produzida pela classifica��o com os dados
# originais e com 3 e 7 componentes principais? 