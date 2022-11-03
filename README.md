
# Simulations on using restricted cubic splines to create calibration curves

Contains code to reproduce simulations showing that logit transforming predicted probabilities prior to assessing calibration leads to better performance.

- `sim/sim_functions.R` - functions used to simulate data, apply different methods for creating calibration curves, calculate calibration metrics, and format results.
- `sim/sim[1,2].R` - scripts for running the two simulation studies. Output of simulations can be found in `sim/sim[1,2]-res/`
- `plots.R` - script creates all plots reported in paper (plus additional). Plots found in `sim/plots/`
