# title: "Data Mining - K-NN biopsy data"
# author: "Rafael Garcia de Lima"

library(MASS);
library(class);

# initial data analysis
summary(biopsy);

# removing unused column
biopsy$ID <- NULL;

# function to calc knn
# type === TRUE: row cleaning
# type === FALSE: column removal
biopsy_knn <- function(data, k, type){
  if(type){
    # cleaning rows from NA values
    data <- na.omit(data);
    typeTxt <- "with row cleaning";
  }else{
    # removing column with NA
    data$V6 <- NULL;
    typeTxt <- "without columns that have NA";
  }
  
  len <- length(data);

  # creating accuracy vector
  accuracy = c(1:100)*0

  for (i in 1:100) {
    L <- sample(1:nrow(data),round(nrow(data)/3));
    
    train = data[-L, 1:len-1];
    test = data[L, 1:len-1];
  
    cl = factor(data[-L, len]);
    fit = knn(train, test, cl, k = k);
  
    c_matrix = table(fit[1:length(L)], factor(data[L, len]))
    
    accuracy[i] = sum(diag(c_matrix))/sum(c_matrix)*100
  }
  
  cat('\nAccuracy', typeTxt, ': ', mean(accuracy), '%\n');
}

# knn runs with row cleaning
biopsy_knn(biopsy, 2, TRUE);
biopsy_knn(biopsy, 5, TRUE);
biopsy_knn(biopsy, 12, TRUE);

# nn runs with column cleaning
biopsy_knn(biopsy, 2, FALSE);
biopsy_knn(biopsy, 5, FALSE);
biopsy_knn(biopsy, 12, FALSE);