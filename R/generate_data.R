#' Generate dummy data with several distribution
#'
#' All distributions share the same latent variable \eqn{\eta_{ij} = a + b_i} with \eqn{b_i = N(0, \sigma_r)}
#'
#' - The Poisson distribution uses \eqn{\lambda = e^{\eta_{ij}}}
#' - The negation binomial distribution uses \eqn{\mu = e^{\eta_{ij}}}
#' - The binomial distribution uses \eqn{\pi_{ij} = e^{\eta_{ij}}/(e^{\eta_{ij}}+ 1)}
#' @param a the intercept of the latent variable
#' @param sigma_random The standard error for the random effect \eqn{\sigma_r}
#' @param n_random the number of random effect levels (groups)
#' @param n_replicate the number of observation per random effect level
#' @param nb_size the size parameter of the negative binomial distribution. Passed to the `size` parameter of \code{\link[stats]{rnbinom}}
#' @param b_size the size parameter of the binomial distribution. Passed to the `size` parameter of `\link[stats]{rbinom}`
#' @importFrom assertthat assert_that is.number is.count
#' @importFrom dplyr %>% mutate n
#' @importFrom rlang .data
#' @importFrom stats rnorm rpois rnbinom rbinom plogis
#' @export
generate_data <- function(
  a = 0,
  sigma_random = 0.5,
  n_random = 20,
  n_replicate = 10,
  nb_size = 0.1,
  b_size = 5
) {
  assert_that(is.number(a))
  assert_that(
    is.number(sigma_random),
    sigma_random > 0
  )
  assert_that(is.count(n_random))
  assert_that(is.count(n_replicate))
  assert_that(
    is.number(nb_size),
    nb_size > 0
  )
  assert_that(is.count(b_size))

  re_random <- rnorm(n_random, mean = 0, sd = sigma_random)
  data.frame(
    id = rep(seq_len(n_random), n_replicate)
  ) %>%
    mutate(
      eta = a + re_random[.data$id],
      poisson = rpois(n(), lambda = exp(.data$eta)),
      negbin = rnbinom(n(), size = nb_size, mu = exp(.data$eta)),
      binom = rbinom(n(), size = b_size, prob = plogis(.data$eta))
    )
}
