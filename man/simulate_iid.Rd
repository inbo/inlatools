% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_idd.R
\name{simulate_iid}
\alias{simulate_iid}
\title{simulate data from a second order random walk}
\usage{
simulate_iid(sigma = NULL, tau = NULL, n_sim = 1000)
}
\arguments{
\item{sigma}{the standard deviation of the random intercept}

\item{tau}{the precision of the random intercept}

\item{n_sim}{the number of simulations}
}
\value{
a data.frame with simulated time series from the random walk
}
\description{
simulate data from a second order random walk
}
\examples{
set.seed(20181202)
x <- simulate_iid(sigma = 0.25)
head(x)
}
\seealso{
Other priors: 
\code{\link{plot.sim_iid}()},
\code{\link{plot.sim_rw}()},
\code{\link{select_change}()},
\code{\link{select_divergence}()},
\code{\link{select_poly}()},
\code{\link{select_quantile}()},
\code{\link{simulate_rw}()}
}
\concept{priors}
