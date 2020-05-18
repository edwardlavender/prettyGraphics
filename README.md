
# plot.pretty

<!-- badges: start -->

<!-- badges: end -->

`plot.pretty` is an R package designed to make the production of plots
and data exploration easier, more flexible and prettier, with base R.
`plot.pretty` has been particularly inspired by the requirements of
continuous ecological datasets, although some functions are designed to
work with factors. The package includes multiple ‘building block’
functions which help to define the initial arguments of a plot and then
add elements to a plot in sequence. Some integrative functions draw on
the flexibility of building blocks to define prettier plots for a
variety of equivalent plotting functions in base R
(e.g. `graphics::plot()`, `graphics::hist()` and more). Key
functionality includes:

  - The definition of pretty axes for plots.
  - Colouring lines by covariates to elucidate relationships between
    several variables.
  - Adding shading to elucidate relationships between several variables.
  - Adding statistical summaries to reveal patterns.
  - Adding model predictions to plots to compare observations with model
    predictions.
  - Integrative functions which create prettier plots more easily.

## Installation

You can install the released version of plot.pretty from
[CRAN](https://CRAN.R-project.org) with:

``` r
# Install from CRAN 
install.packages("plot.pretty")

# Or, install the development version from github:
devtools::install_github("edwardlavender/plot.pretty")
```

## The definition of pretty axes

  - `pretty_axis()` is a very flexible function which is used to define
    and add pretty axes to plots (i.e., axes with intelligible tick mark
    labels that are positioned in appropriate, adjoining positions,
    rather than as an approximate box around a plot);
  - `sci_notation()` translates the ‘e’ notation used by base R into
    scientific notation;
  - `add_lagging_point_zero()` brings all numbers up to the same number
    of decimal places;
  - `add_grid_xy_rect()` adds a rectangular grid to a plot at
    user-defined positions;

## Colouring lines by covariates

  - `add_lines()` adds a line to a plot illustrating a relationship
    between y and x that can be coloured by the values of a third
    variable;
  - `add_colour_bar()` adds a customisable colour bar legend to a plot;

## Shading

  - `add_shading()` adds blocks of shading to a plot to elucidate
    relationships between a response and explanatory variables, one of
    which is a factor;

## Statistical summaries

  - `summarise_in_bins()` computes statistical summaries of continuous
    data in bins, which can be added to plots using `add_lines()`;

## Model predictions

  - `list_CIs()` lists model predictions/confidence intervals from
    fitted values and standard errors (or similar);
  - `add_model_predictions()` adds model predictions (e.g. fitted lines,
    confidence intervals) to plots;

## Integrative functions

  - `pretty_plot()` creates prettier plots for a variety of functions;
  - `pretty_hist()` creates prettier histograms;
  - `pretty_boxplot()` creates prettier boxplots;
  - `pretty_density()` creates prettier probability density plots;
  - `pretty_numline()` creates pretty number lines and timelines;
  - `pretty_residuals()` creates prettier diagnostic residual plots
    (including standard diagnostic plots alongside residuals against
    covariates, timestamps and the autocorrelation function, if
    applicable);
  - `Tools4ETS::pretty_ts()`, which is based on `plot.pretty`, creates
    pretty timeseries plots;
  - `Tools4ETS::vis_ts()`, which is based on `plot.pretty`, is an R
    Shiny-Dashboard user interface for the interactive exploration of
    (ecological) timeseries and creation of publication quality plots;

## Plot layout

  - `pretty_mf()` defines a suitable plotting window for a given number
    of plots;
