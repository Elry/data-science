import numpy as np
from sklearn import tree
from sklearn.datasets import load_iris
from sklearn.metrics import accuracy_score

def showPredictions(p, y):
  # flower species of test
  print(p)
  # real labels
  print(y)
  # accuracy
  print((accuracy_score(p, y))*100)  

iris = load_iris()

x = iris.data
y = iris.target
y_names = iris.target_names

# splits into train and test
test_ids = np.random.permutation(len(x))

x_train = x[test_ids[:-10]]
x_test = x[test_ids[-10:]]

y_train = y[test_ids[:-10]]
y_test = y[test_ids[-10:]]

classify = tree.DecisionTreeClassifier()
classify.fit(x_train, y_train)

predictions = classify.predict(x_test)

showPredictions(predictions, y_test)