# freedom 1.1.0 (2022-03-17)

## CHANGES

* Added unit tests for all functions in the package
* Added functionality to the `hse_finite` function to allow for
  rounding, floor or ceiling to adjust the estimates for small
  fractions so that the minimum design prevalence is 1/N which is a
  reasonable approach in some cases. Thanks to Petter Hopp for this
  code.

## BUG FIXES

* A bug was fixed in the `hse` function which incorrectly selected the
  `infinite_hse` method when the sampling fraction is 1. This is now
  corrected and when the sampling fraction is greater than the
  `threshold` argument, the `hse_finite` is used as expected. To be
  clear, this bug probably did not result in errors very often as it
  should be a rare case that every member of a target population is
  sampled in a surveillance initiative.

# freedom 1.0.1 (2020-09-04)

## CHANGES

* Removed line numbers from sample data.
* Added tests for proportions not between 0 and 1

## BUG FIXES

* Removed all tests that required values to be identical after a
  mathematical operation. This resulted in non-identical results when
  testing on R without long doubles. There were some other places in
  the code where tests did not fail but could under simple
  circumstances and this was also improved to allow a tolerance of
  1e-7.

# freedom 1.0 (2020-08-28)

* First release.
