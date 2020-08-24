[![Build Status](https://travis-ci.org/SVA-SE/freedom.svg?branch=master)](https://travis-ci.org/SVA-SE/freedom)

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
is add-on toolset to the Microsoft Excel spreadsheet package. In order
to make these calculations more reproducible and based on openly
available tools, this package is being produced in R. The vision is
ultimately to have an adaptable tool that can quickly be used to
perform these calculations for various surveillance systems.

As a first step, we will define a tool to calculate the probability of
freedom from 4 diseases in Sweden (BVDV, IBR, Bovine Leucosis, and
Paratuberculosis). This first step will fulfil the requirements to
report these results. The tool may be adaptable for other
calculations, but at this point, absolute adaptability is not the main
focus.
