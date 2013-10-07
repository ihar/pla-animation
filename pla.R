# Perceptron learning algorithm with visualisation

# TODO: 
# - output of target coefficients
# - output of rmse values

POINTS_NUM <- 100
ITERATIONS_NUM <- 100

# Generating test data set
generate_data_set <- function(num_of_points) {
  res <- data.frame(x=runif(num_of_points, -1, 1), y=runif(num_of_points, -1, 1))
  return(res)
}

# Classify point with current weigths
# weight - c(w1, w2, w3)
# point - c(1, x, y)
calculate_classes <- function(point, weights) {
  cl <- drop(weights %*% point)
  if (cl == 0) cl <- 1
  return(sign(cl))
}

# Determine target function randomly. PLA should find it.
# The target function divides data set into two parts, blue and red
# Returns data_points with labeled elements
set_target_function <- function(data_points) {
  pts <- data.frame(x=runif(2, -1, 1), y=runif(2, -1, 1))
  classes <- c()
  x <- data_points$x
  y <- data_points$y
  x1 <- pts$x[1]
  y1 <- pts$y[1]
  x2 <- pts$x[2]
  y2 <- pts$y[2]
  classes <- sign( (x-x1)*(y2-y1) - (y-y1)*(x2-x1) )
  classes[classes==0] = 1
  data_points$class = classes
  return(data_points)
}

# Generate test data set
dp <- generate_data_set(POINTS_NUM)
dp <- set_target_function(dp)
plot(dp$x, dp$y, col=(dp$class+3), pch=16)

# Animate PLA procedure step by step
library("animation")

alpha <- 1 #learning rate

# First zero is for bias term, second is for x, third is for y
w <- c(0, 0, 0)
it <- 0
rmse <- 1

# Result saved as animated gif
saveGIF(
{
  while((it <= ITERATIONS_NUM) & (rmse > 0))  {
    global_error <- 0  
    for (i in 1:POINTS_NUM) {
      point <- c(1, dp$x[i], dp$y[i])
      curr_class <- calculate_classes(point, w)  
      local_error <- dp$class[i] - curr_class
      w <- w + alpha * local_error * point
      global_error <- global_error + local_error^2
    }
    rmse <- sqrt(global_error/POINTS_NUM)
    print(rmse)
    it <- it + 1
    
    plot(dp$x, dp$y, col=(dp$class+3), pch=16)
    abline(-w[1]/w[3], -w[2]/w[3])
    title_text <- paste0("y=", -w[2]/w[3], "x + ",  -w[1]/w[3])
    title(title_text)
  }
  # using of GraphicsMagick
}, convert="gm convert", outdir=getwd()
)


