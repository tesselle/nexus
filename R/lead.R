# GEOLOGICAL MODEL AGE

#' @export
#' @rdname lia_age
#' @aliases lia_age,numeric,numeric,numeric-method
setMethod(
  f = "lia_age",
  signature = signature(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, t0 = 3.8,
                        x_star = 18.75, y_star = 15.63, z_star = 38.86,
                        mu = 9.66, kappa = 3.90, th232 = 0.049475,
                        u238 = 0.155125, u235 = 0.98485, u238_235 = 137.79,
                        tolerance = sqrt(.Machine$double.eps)) {

    x0 <- x_star - mu * (exp(u238 * t0) - 1) # 206Pb/204Pb at t0
    y0 <- y_star - mu / u238_235 * (exp(u235 * t0) - 1) # 207Pb/204Pb at t0
    z0 <- z_star - mu * kappa * (exp(th232 * t0) - 1) # 208Pb/204Pb at t0

    t_model <- vector(mode = "numeric", length = length(x))
    residue <- matrix(data = 0, nrow = length(x), ncol = 2)
    for (i in seq_along(t_model)) {
      tt <- 0
      dtt <- 1
      iter <- 0
      while (abs(dtt) > tolerance) { # Newton loop
        iter <- iter + 1 # Number of iterations

        slope <- (y[i] - y0) / (x[i] - x0)
        u <- exp(u235 * t0) - exp(u235 * tt)
        v <- exp(u238 * t0) - exp(u238 * tt)
        uprime <- -u235 * exp(u235 * tt)
        vprime <- -u238 * exp(u238 * tt)

        f <- u / v - slope * u238_235 # f should be zero
        df <- (1 / v) * (uprime - u / v * vprime) # df/dtt
        dtt <- -f / df # Newton-Raphson increment
        tt <- tt + dtt
      }
      t_model[i] <- tt
      residue[i, ] <- f # Residue
    }

    mu_i <- (x - x0) / (exp(u238 * t0) - exp(u238 * t_model))
    kappa_i <- (z - z0) / (exp(th232 * t0) - exp(th232 * t_model)) / mu_i
    data.frame(
      age = t_model * 1000, # Use Ma instead of Ga
      mu = mu_i,
      kappa = kappa_i,
      f = sqrt(residue[, 1]^2 + residue[, 2]^2)
    )
  }
)

#' @export
#' @rdname lia_age
#' @aliases lia_age,list,missing,missing-method
setMethod(
  f = "lia_age",
  signature = signature(x = "list", y = "missing", z = "missing"),
  definition = function(x, t0 = 3.8,
                        x_star = 18.75, y_star = 15.63, z_star = 38.86,
                        mu = 9.66, kappa = 3.90, th232 = 0.049475,
                        u238 = 0.155125, u235 = 0.98485, u238_235 = 137.79,
                        tolerance = sqrt(.Machine$double.eps)) {
    ## Validation
    k <- match(c("x", "y", "z"), names(x))
    if (anyNA(k)) {
      msg <- sprintf("%s is a list, but does not have components %s, %s and %s.",
                     sQuote("x"), sQuote("x"), sQuote("y"), sQuote("z"))
      stop(msg, call. = FALSE)
    }
    methods::callGeneric(x = x$x, y = x$y, z = x$z, t0 = t0,
                         x_star = x_star, y_star = y_star, z_star = z_star,
                         mu = mu, kappa = kappa, th232 = th232,
                         u238 = u238, u235 = u235, u238_235 = u238_235,
                         tolerance = tolerance)
  }
)
