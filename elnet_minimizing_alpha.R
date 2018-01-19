elnet_minimizing_alpha <- function(X, Y, Xtest, Xpred, step = 10 ){
# Takes as inputs: matrix X 
#                  vector Y:      independent variable to predict on
#                  matrix Xpred:  vector of real values in test interval to check predictions. 
  
argmin = 1  
error = Inf  
v1 <- c(0:step)/step 
for(a in  v1) {
  cv.elnet=cv.glmnet(X,Y,alpha=a)
  current_error = mean((Ytest-c(predict(cv.elnet,Xtest, s = "lambda.min")))^2)
  if(current_error <error){
    error <- current_error 
    argmin <- a 
  }
  
}
  # Y2.elnet=c(exp(predict(cv.elnet,X, s = "lambda.min")))
  # Ytest2.elnet=c(exp(predict(cv.elnet,Xtest, s = "lambda.min")))
  # Ypred2.elnet=c(exp(predict(cv.elnet,Xpred, s = "lambda.min")))
  # 
  # Y2.elnet=c(exp(predict(cv.elnet,X, s = "lambda.min")))
  # Ytest2.elnet=c(exp(predict(cv.elnet,Xtest, s = "lambda.min")))
  # Ypred2.elnet=c(exp(predict(cv.elnet,Xpred, s = "lambda.min")))
  # 
  # sq_error_elnet = (Ytest-c(predict(cv.elnet,Xtest, s = "lambda.min")))^2
  # sq_error_elnet_pred = (Ypred-c(predict(cv.elnet,Xpred, s = "lambda.min")))^2
  # sq_error_elnet_train = (Y-c(predict(cv.elnet,X, s = "lambda.min")))^2
  # 
  # vector_errors <- c(mean(sq_error_elnet),  c(mean(sqrt(sq_error_elnet)), mean(Ytest-Ytest2.elnet), mean(Ytest),mean(Ytest-Ytest2.elnet)/mean(Ytest)*100, mean(Ypred-Ypred2.elnet), 
  # mean(Ypred),mean(Ypred-Ypred2.elnet)/mean(Ypred)*100 ))
  # 
  list_returned = list(argmin, error,  coef(cv.elnet))   # x contains copies of n, s, b
  
  return(list_returned)
}
