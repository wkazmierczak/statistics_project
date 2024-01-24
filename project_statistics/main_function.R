library(stats)
library(smoof)
library(ecr)
library(vioplot)

PRS <- function(min_func, lower, upper){
  
  min_value <- Inf
    
  for (i in 1:1000){
    
    point <- runif(length(lower), lower, upper)
    
    current_value <- min_func(point)
    
    if (current_value < min_value) {
      min_value <- current_value
    }
  }
  return(min_value)
}

PRS_n_times <- function(min_func, lower, upper, n){
  
  return(replicate(n, PRS(min_func, lower, upper)))
}

GA_n_times <- function(min_func, lower, upper, n){
  return(replicate(n, ecr(min_func, lower = lower, upper = upper, minimize =TRUE,
                           representation = "float", mu = 50L, lambda = 25L, terminators = list(stopOnIters(1000L)),
                           mutator = setup(mutUniform, lower = lower, upper = upper))$best.y))
}

lower_ackley_2d <- c(replicate(2, -32.768))
upper_ackley_2d <- c(replicate(2, 32.768))

lower_ackley_10d <- c(replicate(10, -32.768))
upper_ackley_10d <- c(replicate(10, 32.768))

lower_ackley_20d <- c(replicate(20, -32.768))
upper_ackley_20d <- c(replicate(20, 32.768))

lower_rosenbrock_2d <- c(replicate(2, -30))
upper_rosenbrock_2d <- c(replicate(2, 30))

lower_rosenbrock_10d <- c(replicate(10, -30))
upper_rosenbrock_10d <- c(replicate(10, 30))

lower_rosenbrock_20d <- c(replicate(20, -30))
upper_rosenbrock_20d <- c(replicate(20, 30))


ackley_2d <- makeAckleyFunction(2)
rosenbrock_2d <- makeRosenbrockFunction(2)
ackley_10d <- makeAckleyFunction(10)
rosenbrock_10d <- makeRosenbrockFunction(10)
ackley_20d <- makeAckleyFunction(20)
rosenbrock_20d <- makeRosenbrockFunction(20)


n <- 50
res_prs_a_2d <- PRS_n_times(ackley_2d, lower_ackley_2d, upper_ackley_2d, n)
res_prs_a_10d <- PRS_n_times(ackley_10d, lower_ackley_10d, upper_ackley_10d, n)
res_prs_a_20d <- PRS_n_times(ackley_20d, lower_ackley_20d, upper_ackley_20d, n)

res_prs_r_2d <- PRS_n_times(rosenbrock_2d, lower_rosenbrock_2d, upper_rosenbrock_2d, n)
res_prs_r_10d <- PRS_n_times(rosenbrock_10d, lower_rosenbrock_10d, upper_rosenbrock_10d, n)
res_prs_r_20d <- PRS_n_times(rosenbrock_20d, lower_rosenbrock_20d, upper_rosenbrock_20d, n)

res_ga_a_2d <- GA_n_times(ackley_2d, lower_ackley_2d, upper_ackley_2d, n)
res_ga_a_10d <- GA_n_times(ackley_10d, lower_ackley_10d, upper_ackley_10d, n)
res_ga_a_20d <- GA_n_times(ackley_20d, lower_ackley_20d, upper_ackley_20d, n)

res_ga_r_2d <- GA_n_times(rosenbrock_2d, lower_rosenbrock_2d, upper_rosenbrock_2d, n)
res_ga_r_10d <- GA_n_times(rosenbrock_10d, lower_rosenbrock_10d, upper_rosenbrock_10d, n)
res_ga_r_20d <- GA_n_times(rosenbrock_20d, lower_rosenbrock_20d, upper_rosenbrock_20d, n)

# means
mean_prs_a_2d <- mean(res_prs_a_2d)
mean_prs_a_10d <- mean(res_prs_a_10d)
mean_prs_a_20d <- mean(res_prs_a_20d)

mean_prs_r_2d <- mean(res_prs_r_2d)
mean_prs_r_10d <- mean(res_prs_r_10d)
mean_prs_r_20d <- mean(res_prs_r_20d)

mean_ga_a_2d <- mean(res_ga_a_2d)
mean_ga_a_10d <- mean(res_ga_a_10d)
mean_ga_a_20d <- mean(res_ga_a_20d)

mean_ga_r_2d <- mean(res_ga_r_2d)
mean_ga_r_10d <- mean(res_ga_r_10d)
mean_ga_r_20d <- mean(res_ga_r_20d)

# hist and boxplots Ackley PRS
hist(res_prs_a_2d)
boxplot(res_prs_a_2d)
hist(res_prs_a_10d)
boxplot(res_prs_a_10d)
hist(res_prs_a_20d)
boxplot(res_prs_a_20d)

# hist and boxplots Ackley GA
hist(res_ga_a_2d)
boxplot(res_ga_a_2d)
hist(res_ga_a_10d)
boxplot(res_ga_a_10d)
hist(res_ga_a_20d)
boxplot(res_ga_a_20d)

# hist and boxplots Rosenbrock PRS
hist(res_prs_r_2d)
boxplot(res_prs_r_2d)
hist(res_prs_r_10d)
boxplot(res_prs_r_10d)
hist(res_prs_r_20d)
boxplot(res_prs_r_20d)
vioplot(res_ga_a_2d) #vioplot

# hist and boxplots Rosenbrock GA
hist(res_ga_r_2d)
boxplot(res_ga_r_2d)
hist(res_ga_r_10d)
boxplot(res_ga_r_10d)
hist(res_ga_r_20d)
boxplot(res_ga_r_20d)


#tests
t.test(res_ga_a_2d, res_prs_a_2d, conf.level = 0.95)#?
t.test(res_ga_a_10d, res_prs_a_10d)
t.test(res_ga_a_20d, res_prs_a_20d)


print(res_ga_a_2d)
print(mean_ga_a_2d)
print("----------")
print(res_prs_a_2d)
print(mean_prs_a_2d)

print(res_ga_a_2d)
print(res_prs_a_2d)

a <- as.numeric(res_ga_a_2d)
b <- as.numeric(res_prs_a_2d)

t.test(a,b, conf.level=0.95)




