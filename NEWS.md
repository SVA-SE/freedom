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
