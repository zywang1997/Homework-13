###Making the mtrain matrix###
if (!exists("mtrain")) {
  mtrain <- read.csv("mnist_train.csv", header=F) %>% as.matrix
  train_classification <- mtrain[,1] #Go through this vector, set equal to zero if not 3 and 1 if 3. Gives y values
  mtrain <- mtrain[,-1]/256 #x values
  
  
  colnames(mtrain) <- 1:(28^2)
  rownames(mtrain) <- NULL
  
  x <- mtrain[1:1000,]
}
y <- rep(NA, length(train_classification))

#Converting all threes to one and all other numbers to zero
for (i in 1:length(train_classification)){
  cn <- train_classification[i]

  if (cn==3){
    cn <- 1
  } else {
    cn <- 0
  }
  y[i] <- cn
}

y <- factor(y, levels=c(0,1))
y <- y[1:1000]

#Essentially repeating the above for the test data set 
###Making the mtrain matrix###
if (!exists("mtrain2")) {
  mtrain2 <- read.csv("mnist_test.csv", header=F) %>% as.matrix
  train_classification2 <- mtrain2[,1] #Go through this vector, set equal to zero if not 3 and 1 if 3. Gives y values
  mtrain2 <- mtrain2[,-1]/256 #x values
  
  
  colnames(mtrain2) <- 1:(28^2)
  rownames(mtrain2) <- NULL
  
  x2 <- mtrain2[1:1000,]
}
y2 <- rep(NA, length(train_classification))

#Converting all threes to one and all other numbers to zero
for (i in 1:length(train_classification2)){
  cn <- train_classification2[i]
  
  if (cn==3){
    cn <- 1
  } else {
    cn <- 0
  }
  y2[i] <- cn
}

y2 <- factor(y, levels=c(0,1))
y2 <- y[1:1000]

print(head(x))
print(head(y))
print(head(x2))
print(head(y2))


