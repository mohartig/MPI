#' Multidimensional Poverty Index
#'
#' @param households a vector (1 x n)
#' @param data a vector (1 x n) n={10,20,30, ...}
#'
#' @return H, A and MPI
#' @export
#'
MPI <- function(households, data){

  data <- matrix(data, nrow = 10, ncol = length(households), byrow = FALSE)
  weights <- c(rep((1/3)/2,4), rep((1/3)/6,6))

  # check if matrix has ten rows
  if (nrow(data) != 10)
    stop("Matrix has to be of row number 10\n")

  # establish the weights and the deprivation matrix
  dep_value <- weights %*% data
  helper <- numeric(ncol(data))

  # check if deprived
  for (i in 1:ncol(data)) {
    if(dep_value[i] >= 1/3) {helper[i] <- 1}
  }

  # calculate H
  H <- sum(households[helper == 1]) / sum(households)

  # calculate A and MPI
  A <- numeric(1)
  for (i in 1:ncol(data)) {
    if(helper[i] == 1){A <- A + (households[c(i)]*dep_value[i])}
  }
  A <- A / sum(households[helper == 1])
  MPI <- A * H

  # return indexes
  return(paste("H=", round(H,5),"A=", round(A,5),"MPI=", round(MPI,5)))
}
