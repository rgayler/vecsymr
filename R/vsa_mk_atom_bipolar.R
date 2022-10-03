# function to make an atomic VSA vector

vsa_mk_atom_bipolar <- function(
    vsa_dim, # integer - dimensionality of VSA vector
    seed = NULL # integer - seed for random number generator
) # value # one randomly selected VSA vector of dimension vsa_dim
{
  ### Set up the arguments ###
  # The OCD error checking is probably more useful as documentation
  if(missing(vsa_dim))
    stop("vsa_dim must be specified")

  if(!(is.vector(vsa_dim, mode = "integer") && length(vsa_dim) == 1))
    stop("vsa_dim must be an integer")

  if(vsa_dim < 1)
    stop("vsa_dim must be (much) greater than zero")

  # check that the specified seed is an integer
  if(!is.null(seed) &&!(is.vector(seed, mode = "integer") && length(seed) == 1))
    stop("seed must be an integer")

  # if seed is set the the vector is fixed
  # otherwise it is randomised
  set.seed(seed)

  # Construct a random bipolar vector
  sample(c(-1L, 1L), size = vsa_dim, replace = TRUE)
}
