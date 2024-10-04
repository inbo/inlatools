# inlatools 0.0.3

* Export `dgpoisson()` and `rgpoisson()`

# inlatools 0.0.2

* Use [`checklist`](https://inbo.github.io/checklist/) CI infrastructure.
* Add `get_anomaly()` to return a set of anomalies.

# inlatools 0.0.1.9000

* Added a `NEWS.md` file to track changes to the package.
* `fast_distribution_check()` handles `zeroinflatedpoisson0`.
* `fast_distribution_check()` works on a list of INLA models.
* `generate_data()` returns a `group_id` (was `id`) and `observation_id` (new).
* `select_change()`, `select_divergence()`, `select_poly()` and 
  `select_quatile()` make the required subsets before sending data to `plot()`
* `plot.distribution_check()` gains an `n` and `scales` argument.
* `plot.sim_iid()` and  `plot.sim_rw()` gain a `baseline`, `center` and 
  `quantiles` argument.
* Move pkgdown website to https://inlatools.netlify.com.
* Vignettes on dispersion check and distribution checks are now combined in a
  single and more elaborate vignette
* Vignette on priors is split into a vignette on priors on iid effects and
  a dedicated vignette on priors for random walks
