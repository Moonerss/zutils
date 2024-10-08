#' Marry Christmas
#'
#' Create a Christmas tree
#' @param bulbs Whether plot bulbs.
#' @param snow Whether plot snow.
#' @param star Whether plot star.
#' @param bg.col Same as \code{bg} in \code{\link[graphics]{par}}
#'
#' @export
funny_christmas_tree <- function(bulbs = TRUE, snow = TRUE, star = TRUE, bg.col = 1){

  set.seed(1)

  p <- graphics::points
  r <- stats::runif
  l <- 500
  s <- (l:1)/1000

  graphics::par(bg = bg.col, pch = 8, mar = rep(0, 4))
  plot(0, xlim = c(-1.25, 1.25), axes = F, xlab = "", ylab = "", ylim = c(-100, 550))
  a <- 0

  graphics::rect(-0.1, -100, 0.1, 0, col = "brown", border = "brown")

  if(star == TRUE) {
    # Star glow
    for(i in 11:1){
      graphics::points(0.005, 500.2, pch = 19, col = grDevices::rgb(1, 1, 0, 0.05), cex = 5 + (0.65 * i))
    }
  }

  # Tree
  for(i in rep(exp(-(1:125)* 0.016), 4) * s + s){
    a <- a + 1

    for(j in seq(-i, i, 0.02)){
      p(j, a, col = grDevices::rgb(0, i*.7 + 0.2 * r(1, 0.6, 1) - 0.5 * j^2, 0))
    }
  }

  if(bulbs == TRUE) {
    # Christmas bulbs
    cols <- sample(c(grDevices::rgb(1, 0, 0, 0.1), grDevices::rgb(0, 1, 0, 0.1), grDevices::rgb(1, 1, 0, 0.1), grDevices::rgb(0, 0.2, 1, 0.1)))
    cols2 <- sample(c(grDevices::rgb(1, 0, 0, 0.1), grDevices::rgb(0, 1, 0, 0.1), grDevices::rgb(1, 1, 0, 0.1), grDevices::rgb(0, 0.2, 1, 0.1)))
    cols3 <- sample(c(grDevices::rgb(1, 0, 0, 0.1), grDevices::rgb(0, 1, 0, 0.1), grDevices::rgb(1, 1, 0, 0.1), grDevices::rgb(0, 0.2, 1, 0.1)))
    cols4 <- sample(c(grDevices::rgb(1, 0, 0, 0.1), grDevices::rgb(0, 1, 0, 0.1), grDevices::rgb(1, 1, 0, 0.1), grDevices::rgb(0, 0.2, 1, 0.1)))

    for(i in 11:1){
      graphics::points(seq(-1, 1, 0.1), rep(0, 21), pch = 19, col = cols, cex = 0.15 + (0.12 * i))
      graphics::points(seq(-0.75, 0.75, 0.1), rep(125, 16), pch = 19, cex = 0.15 + (0.12 * i), col = cols2)
      graphics::points(seq(-0.5, 0.5, 0.1), rep(250, 11), pch = 19, cex = 0.15 + (0.12 * i), col = cols3)
      graphics::points(seq(-0.25, 0.25, 0.1), rep(375, 6), pch = 19, cex = 0.15 + (0.12 * i), col = cols4)

    }

    set.seed(60)
    graphics::points(stats::runif(6, -0.77, 0.77), stats::runif(6, 0, 100), pch = 21, cex = 3, bg = sample(c("gold", "red")))
    set.seed(2)
    graphics::points(stats::runif(7, -0.5, 0.5), stats::runif(7, 125, 300), pch = 21, cex = 3, bg = sample(c("gold", "red")))
    set.seed(5)
    graphics::points(stats::runif(4, -0.2, 0.2), stats::runif(4, 300, 370), pch = 21, cex = 3, bg = sample(c("gold", "red")))
    graphics::points(0, 140, pch = 21, cex = 3, bg = sample(c("gold", "red")))
    graphics::points(-0.15, 180, pch = 21, cex = 3, bg = sample(c("gold", "red")))
  }

  if(star == TRUE) {
    # Star
    graphics::points(0.006, 508, pch = 19, col = 7, cex = 5.6)
    p(0, 520, pch = 8, cex = 13, col = 7)
  }


  if(snow == TRUE) {
    # Snow
    ## use '\u2744\ufe0f' replace '❄️'
    p(r(75, -1.25, 1.25), r(75, -100, l+50), col = "white", cex = sample(seq(0.5, 1.2, length.out = 75)), pch = '\u2744\ufe0f')
    p(r(100, -1.25, 1.25), r(100, -100, -95), col = "white", cex = sample(seq(0.5, 1.2, length.out = 75)), pch = '\u2744\ufe0f')
  }

}
