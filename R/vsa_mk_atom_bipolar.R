#' Make a bipolar atomic VSA vector
#'
#' @description Make an atomic VSA vector with bipolar element values.
#'
#' @param vsa_dim An integer \eqn{> 0} - The dimensionality of the returned VSA
#'   vector.
#' @param seed An integer or `NULL` - The random number generator seed to use.
#'
#' @return A numeric vector with length `vsa_dim` and elements randomly selected
#'   from \eqn{\{-1, +1\}} with equal probability.
#' @export
#'
#' @details Creates a randomly selected bipolar VSA vector with dimensionality
#'   `vsa_dim`: \eqn{v \in \{-1, +1\}^{vsa\_dim}}. The values from \eqn{\{-1,
#'   +1\}} are randomly selected with equal probability.
#'
#' If `seed` is an integer it is passed to the random number generator so that
#' the result VSA vector is directly reproducible (it is a function of the
#' seed).
#'
#' If `seed` is not given or is `NULL` the random number generator continues
#' from its current state and the result VSA vector is not *directly*
#' reproducible. However, the current state of the random number generator is
#' *indirectly* reproducible from the initial seed and all the intervening
#' invocations of the random number generator (assuming deterministic
#' execution).
#'
#' If you really care about randomisation and reproducibility you will have to
#' think carefully about generating seeds before you use them.
#'
#' @examples
#' vsa_mk_atom_bipolar(10)

vsa_mk_atom_bipolar <- function(vsa_dim,
                                seed = NULL)
{
  ### Check the arguments ###
  # vsa_dim: integerish & not NA & > 0
  vsa_dim <-
    checkmate::assert_count(vsa_dim, positive = TRUE, coerce = TRUE)
  # seed: (integerish & not NA) | NULL
  seed <- checkmate::assert_int(seed, null.ok = TRUE, coerce = TRUE)

  ### Set seed ###
  # If seed is set (an integer) then the returned vector is a function of the
  # seed, otherwise it is continued from the current state of the random number
  # generator
  if (!is.null(seed))
    set.seed(seed)

  ### Construct a random equiprobable bipolar vector ###
  sample(c(-1L, 1L), size = vsa_dim, replace = TRUE)
}
