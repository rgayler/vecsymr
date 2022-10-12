# Derived from stripped down code of expect_identical()
expect_not_identical <- function(object, expected, info = NULL, label = NULL,
                             expected.label = NULL, ...) {
  # 1. Capture objects and labels
  act <- testthat::quasi_label(rlang::enquo(object), label, arg = "object")
  exp <- testthat::quasi_label(rlang::enquo(expected), expected.label, arg = "expected")
  # 2. Call expect()
  ident <- identical(act$val, exp$val, ...)
  testthat::expect(
    !ident, # NOT identical
    sprintf("%s unexpectedly identical to %s.\n%s", act$lab, exp$lab, ""),
    info = info
  )
  # 3. Invisibly return the value
  invisible(act$val)
}
