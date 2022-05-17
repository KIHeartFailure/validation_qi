ynfac <- function(var) {
  var <- factor(var,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
}
qifac <- function(var, type = "f") {
  if (type == "f") {
    var <- factor(var,
      levels = c(0, 1),
      labels = c("Not fulfilled", "Fulfilled")
    )
  }
  if (type == "ob") {
    var <- factor(var,
      levels = c(0, 1),
      labels = c("<50% fulfilled", ">=50% fulfilled")
    )
  }
  if (type == "a") {
    var <- factor(var,
      levels = c(0, 1),
      labels = c("None fulfilled", "All fulfilled")
    )
  }
  return(var)
}
