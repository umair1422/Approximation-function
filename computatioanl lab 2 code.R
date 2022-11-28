quadratic_function <- function(opt_parameter, x, y) {
  a0<-opt_parameter[1]
  a1<-opt_parameter[2]
  a2<-opt_parameter[3]
  quadratic_equation <- sum(y-(a0 + a1*x + a2*x^2))^2
  return(quadratic_equation)
}
quadratic_function(c(1,2,3),6,8)


approximates_function <- function(interval_number, my_function) {
 
  interval <- seq(0, 1, length.out = interval_number+1)
  values_A <- matrix(NA, nrow = interval_number, ncol = 3)
  colnames(values_A) <- c("a0", "a1", "a2")
  values_X <- matrix(NA, nrow = interval_number, ncol = 3)
  colnames(values_X) <- c("x0", "x1", "x2")
  Result_matrix <- matrix(NA, nrow = interval_number, ncol = 6)
  
    index<-1
    while (index <length(interval)) {
    function_x0 <- my_function(interval[index])
    
    function_x1 <- my_function(mean(c(interval[index], interval[index+1]))) #mid point of interval
    function_x2 <- my_function(interval[index+1]) #end interval
    
    optimization_parameter <- optim(par = c(0,1,1),
                     fn = quadratic_function,
                     x = c(interval[index],
                           mean(c(interval[index], interval[index+1])),
                           interval[index+1]),
                     y = c(function_x0, function_x1, function_x2))$par
    values_X[index,1:3] <- c(interval[index], mean(c(interval[index], interval[index+1])), interval[index+1])
    values_A[index,1:3]<-optimization_parameter
    
    index<-index+1
    }
    Result_matrix<-cbind(values_A,values_X)
    print(Result_matrix)
    Result_matrix<-as.data.frame(Result_matrix)
  return(Result_matrix)
}

function1_x <- function(x) {
  cal1<- -x * (1-x)
  return(cal1)
}

function2_x <- function(x) {
  cal2<- -x*sin(10*pi*x)
  return(cal2)
}
F1_result <- approximates_function(100, function1_x)
F1_result$`g(x)` <- F1_result[,1] + F1_result[,2]*F1_result[,5] + F1_result[,3]*F1_result[,5]^2

library(ggplot2)
ggplot(F1_result, aes(x =x1, y =`g(x)`)) +
  geom_function(fun = function1_x, aes(color = "red"), size = 1) +
  geom_point() + labs(x="x",y="y")+
  theme(legend.position = "none")


F2_result <- approximates_function(100, function2_x)
F2_result$`g(x)` <- F2_result[,1] + F2_result[,2]*F2_result[,5] + F2_result[,3]*F2_result[,5]^2

ggplot(F2_result, aes(x = x1, y = `g(x)`)) +
  geom_function(fun = function2_x, aes(color = "red"), size = 1) +
  geom_point() +
  labs(x="x",y="y")+
  theme(legend.position = "none")