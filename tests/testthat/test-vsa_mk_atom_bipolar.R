# All successful invocations of vsa_mk_atom_bipolar() advance the random number
# generator, therefore alter the global state. Good test hygiene requires tests
# to leave the global environment as they found it, hence the calls to
# withr::local_seed(). vsa_mk_atom_bipolar() *should* work correctly regardless
# of the prior seed value, so there's no need to have some specific prior seed
# value. However, in the interests of absolute reproducibility of the tests I
# use withr::local_seed() with a specific seed value.

test_that("argument checks work", {
  withr::local_seed(seed = 12345)
  # Far from exhaustive tests. Essentially one per error condition expected to
  # be caught, and a token few non-error conditions.

  # The calls to expect_error() and expect_no_error() should check the error
  # message or error class to ensure that they only refer to the intended errors
  # from the checkmate assertions. Unfortunately, checkmate doesn't currently
  # (2022-10-14) set a custom error class so we have to check the error
  # messages. Also unfortunately, the checkmate error messages are a bit
  # generic, so it's possible that other non-checkmate errors might produce
  # matching error messages.

  # vsa_dim: mandatory & integerish & not NA & > 0
  expect_error(vsa_mk_atom_bipolar(), regexp = "vsa_dim") # vsa_dim missing
  expect_error(vsa_mk_atom_bipolar(vsa_dim = NULL), regexp = "vsa_dim") # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = ""), regexp = "vsa_dim") # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = TRUE), regexp = "vsa_dim") # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 1.1), regexp = "vsa_dim") # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = NA), regexp = "vsa_dim") # NA
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 0), regexp = "vsa_dim") # not > 0
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1.00000001), message = "vsa_dim") # integerish
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1), message = "vsa_dim") # integerish

  # seed: (integerish & not NA) | NULL
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = NULL), message = "seed") # NULL
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = ""), regexp = "seed") # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = TRUE), regexp = "seed") # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = 1.1), regexp = "seed") # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = NA), regexp = "seed") # NA
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = 1.00000001), message = "seed") # integerish
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = 1), message = "seed") # integerish
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = 0), message = "seed") # integerish, zero
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = -1), message = "seed") # integerish, -ve
})

test_that("result is correct type and dimensionality", {
  withr::local_seed(seed = 12345)

  # result is integer vector with length vsa_dim
  expect_vector(vsa_mk_atom_bipolar(vsa_dim = 1),
                ptype = integer(),
                size = 1)
  expect_vector(vsa_mk_atom_bipolar(vsa_dim = 100),
                ptype = integer(),
                size = 100)
  expect_vector(vsa_mk_atom_bipolar(vsa_dim = 1e4),
                ptype = integer(),
                size = 1e4)
})

test_that("result is correct domain", {
  withr::local_seed(seed = 12345)

  # result elements are from {-1, +1}
  expect_setequal(vsa_mk_atom_bipolar(vsa_dim = 100), c(-1L, 1L))
})

test_that("result levels are equiprobable", {
  withr::local_seed(seed = 12345)

  # result elements ({-1, +1}) are approximately equiprobable
  #   expected mean is 0.0
  #   SD is 2*sqrt(0.5/vsa_dim)
  #   check mean is within 6*SD of 0
  expect_lt(abs(mean(vsa_mk_atom_bipolar(vsa_dim = 1e4))), 6 * 2 * sqrt(0.5 /
                                                                          1e4))
})

test_that("results are reproducible when expected", {
  withr::local_seed(seed = 12345)

  # Generated results should be identical across calls when seed is set identically,
  expect_identical(
    vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = 42),
    vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = 42)
  )
  expect_not_identical(
    vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = 42),
    vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = 43)
  )

  # Generated results should be different across calls when seed is not set
  expect_not_identical(vsa_mk_atom_bipolar(vsa_dim = 1e4),
                       vsa_mk_atom_bipolar(vsa_dim = 1e4))
  expect_not_identical(vsa_mk_atom_bipolar(vsa_dim = 1e4),
                       vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = NULL))
  expect_not_identical(
    vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = NULL),
    vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = NULL)
  )
  expect_not_identical(
    vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = 42),
    vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = NULL)
  )
})
