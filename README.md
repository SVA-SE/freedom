[![Build Status](https://github.com/SVA-SE/freedom/actions/workflows/r.yml/badge.svg)](https://github.com/SVA-SE/freedom/actions/workflows/r.yml)
[![CRAN status](https://www.r-pkg.org/badges/version/freedom)](https://CRAN.R-project.org/package=freedom)
[![Code coverage](https://codecov.io/gh/SVA-SE/freedom/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SVA-SE/freedom)

# freedom

An R package to calculate probability of freedom from disease in a
population based on surveillance data.

## Background

At SVA, we perform calculations of probability of freedom from disease
based on ongoing surveillance activities in animal populations that
over a period of time have had negative results. This is referred to a
Demonstrating Disease Freedom (DDF) and the tools and results from
this are very important as a foundation for affecting and maintaining
trade relationships as well as trade restrictions to avoid future
introductions.

These calculations have been successfully done in @risk software which
is an add-on toolset to the Microsoft Excel spreadsheet programme. In
order to make these calculations more reproducible and based on openly
available tools that are tailored to our needs, this package has been
produced in R. The vision is ultimately to have an adaptable tool that
can quickly be used to perform these calculations for various
surveillance systems.
