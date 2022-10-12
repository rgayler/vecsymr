test_that("argument checks work", {
  # Far from exhaustive tests. Essentially one per error condition expected to
  # be caught, and a token few non-error conditions.

  # vsa_dim: mandatory & integerish & not NA & > 0
  expect_error(vsa_mk_atom_bipolar()) # vsa_dim missing
  expect_error(vsa_mk_atom_bipolar(vsa_dim = NULL)) # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = "")) # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = TRUE)) # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 1.1)) # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = NA)) # NA
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 0)) # not > 0
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1.00000001)) # integerish
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1)) # integerish

  # seed: (integerish & not NA) | NULL
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = NULL)) # NULL
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = "")) # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = TRUE)) # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = 1.1)) # not integerish
  expect_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = NA)) # NA
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = 1.00000001)) # integerish
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = 1)) # integerish
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = 0)) # integerish, zero
  expect_no_error(vsa_mk_atom_bipolar(vsa_dim = 1, seed = -1)) # integerish, -ve
})

test_that("result is correct type and dimensionality", {
  # result is integer vector with length vsa_dim
  expect_vector(vsa_mk_atom_bipolar(vsa_dim = 1), ptype = integer(), size = 1)
  expect_vector(vsa_mk_atom_bipolar(vsa_dim = 100), ptype = integer(), size = 100)
  expect_vector(vsa_mk_atom_bipolar(vsa_dim = 1e4), ptype = integer(), size = 1e4)
})

test_that("result is correct domain", {
  # result elements are from {-1, +1}
  expect_setequal(vsa_mk_atom_bipolar(vsa_dim = 100), c(-1L, 1L))
})

test_that("result levels are equiprobable", {
  # result elements ({-1, +1}) are approximately equiprobable
  #   expected mean is 0.0
  #   SD is 2*sqrt(0.5/vsa_dim)
  #   check mean is within 6*SD of 0
  expect_lt(abs(mean(vsa_mk_atom_bipolar(vsa_dim = 1e4))), 6*2*sqrt(0.5/1e4))
})

test_that("results are reproducible when expected", {
  # Generated results should be identical across calls when seed is set identically,
  expect_identical(vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = 42),
                   vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = 42))
  expect_not_identical(vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = 42),
                       vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = 43))
  # Generated results should be different across calls when seed is not set
  expect_not_identical(vsa_mk_atom_bipolar(vsa_dim = 1e4),
                       vsa_mk_atom_bipolar(vsa_dim = 1e4))
  expect_not_identical(vsa_mk_atom_bipolar(vsa_dim = 1e4),
                       vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = NULL))
  expect_not_identical(vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = NULL),
                       vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = NULL))
  expect_not_identical(vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = 42),
                       vsa_mk_atom_bipolar(vsa_dim = 1e4, seed = NULL))
})
