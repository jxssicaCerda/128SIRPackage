random_infect <- function(nrow = 6, ncol = 10, prob = 0.1) {
  # calculate total # of cells & number of people infected
  total_cells <- nrow * ncol
  num_infected <- round(total_cells * prob)
  
  # use those to create vector
  initial_states <- c(rep(1, num_infected), rep(0, total_cells - num_infected))
  
  # shuffle the vector to randomize data
  initial_states <- sample(initial_states)
  
  # convert the vector to a matrix to represent a grid, return
  grid <- matrix(initial_states, nrow = nrow, ncol = ncol)
  grid
}


infect_corners <- function(nrow = 6, ncol = 10){
  x <- matrix(0, nrow = nrow, ncol = ncol)
  x[1, 1] <- 1
  x[1, ncol] <- 1
  x[nrow, 1] <- 1
  x[nrow, ncol] <- 1
  
  # This will allow plot(x) to work (later)
  # TODO: Add this to all other functions that create or modify these matrices.
  class(x) <- c("SIRmatrix", class(x))
  x
}


plot.SIRmatrix = function(x, col = c("white","red", "gray"), ...) {
  # ... means pass through every other argument on to the next function
  rows <- seq(nrow(x))
  x2 <- t(x[rev(rows), ])
  image(x2, zlim=c(0,2), col = col, axes = FALSE, ...)
}


step <- function(x, prob = 0.125){
  
  # make a slightly bigger matrix, so we don't have to worry about the boundaries.
  nr2 = nrow(x) + 2
  nc2 = ncol(x) + 2
  x2 <- matrix(0, nrow = nr2, ncol = nc2)
  infected <- which(x == 1, arr.ind = TRUE) + 1
  ni <- nrow(infected)
  
  directions <- c(-1, 0, 1)
  # Don't worry about cells that are already infected or removed
  for(i in directions){
    for(j in directions){
      infect_ij <- infected
      infect_ij[, "row"] <- infect_ij[, "row"] + i
      infect_ij[, "col"] <- infect_ij[, "col"] + j
      new_inf_rows <- sample(c(TRUE, FALSE), size = ni,
                             prob = c(prob, 1-prob), replace = TRUE)
      new_infect <- infect_ij[new_inf_rows, , drop = FALSE]
      x2[new_infect] <- 1
    }
  }
  
  # Remove the edges from the too big matrix
  result <- x2[-c(1, nr2), -c(1, nc2)]
  
  # Fix the cells that are already infected or removed
  result[1 <= x] <- 2
  result
}


simulate_sir <- function(x, prob = 0.125){
  count <- 0
  while(any(x == 1)){
    x <- step(x, prob)
    count <- count + 1
  }
  list(x = x, count = count, prob = prob, prop_infected = mean(x==2))
}


# For vignette show usage:
x0 <- random_infect()
x1 <- step(x0)

c0 <- infect_corners()
plot(c0)

c1 <- simulate_sir(c0)

# TODO: Change the class of x so we see the correct plot
plot(c1$x)