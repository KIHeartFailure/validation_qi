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
      labels = c("Unattained", "Attained")
    )
  }
  if (type == "ob") {
    var <- factor(var,
      levels = c(0, 1),
      labels = c("<50% attained", ">=50% attained")
    )
  }
  return(var)
}
