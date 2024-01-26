library(smoof)
library(ggplot2)
library(gridExtra)
library(ecr)

PRS <- function(f, dim, n){
  min_value <- Inf
  
  for(i in 1:n){
    point <- runif(dim, getLowerBoxConstraints(f), getUpperBoxConstraints(f))
    value <- f(point)
    
    if(value < min_value){
      min_value <- value
    }
  }
  
  return(min_value)
}

GA <- function(min_func){
  return(ecr(min_func, lower = getLowerBoxConstraints(min_func), upper = getUpperBoxConstraints(min_func), minimize =TRUE,
                          representation = "float", mu = 50L, lambda = 25L, terminators = list(stopOnIters(1000L)),
                          mutator = setup(mutGauss, lower = getLowerBoxConstraints(min_func), upper = getUpperBoxConstraints(min_func)))$best.y)
}

compare <- function(f, dim){
  f1<-f(dim)
  PRS_RES <- replicate(50,PRS(f1,dim, 1000))
  GA_RES <- replicate(50, GA(f1))
  return(list(PRS_RES,GA_RES))
}
ackley_2d_results <- compare(makeAckleyFunction, 2)
ackley_10d_results <- compare(makeAckleyFunction, 10)
ackley_20d_results <- compare(makeAckleyFunction, 20)

ackley_results<-list(ackley_2d_results,ackley_10d_results,ackley_20d_results)

rosenbrock_2d_results<-compare(makeRosenbrockFunction, 2)
rosenbrock_10d_results<-compare(makeRosenbrockFunction, 10)
rosenbrock_20d_results<-compare(makeRosenbrockFunction, 20)

rosenbrock_results<-list(rosenbrock_2d_results,rosenbrock_10d_results,rosenbrock_20d_results)

dims<- c(2,10,20)
histograms_violins<-function(results,name){
  for(i in 1:3){
    h1<-sprintf("Histogram PRS dla funkcji %s dla %s wymiarów",name, dims[[i]])
    h2<-sprintf("Histogram GA dla funkcji %s dla %s wymiarów",name, dims[[i]])
    hist(results[[i]][[1]], main = h1, xlab = "Zakresy minimów", ylab = "Ilość minimów")
    hist(results[[i]][[2]], main = h2, xlab = "Zakresy minimów", ylab = "Ilość minimów")
    boxplot(list(results[[i]][[1]], results[[i]][[2]]), main = sprintf("Porównanie PRS oraz GA dla funkcji %s w %s wymiarach",name, dims[[i]]),
            names = c("PRS", "GA"), ylab = "Zakres minimów")
    
    print(t.test(x = results[[i]][[1]], conf.level = 0.95))
    print(t.test(x = results[[i]][[2]], conf.level = 0.95))
    print(t.test(x = results[[i]][[2]], y = results[[i]][[1]], conf.level = 0.95))
  }
}


main<-function(){
  histograms_violins(ackley_results,"Ackleya")
  histograms_violins(rosenbrock_results,"Rosenbrocka")
}
main()




