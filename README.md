
# Simulations on the use of restricted cubic splines to create calibration curves

Contains code to reproduce simulations, reported here https://osf.io/4n86q, showing that logit transforming predicted probabilities prior to assessing calibration with restricted cubic splines leads to better performance relative to using the predicted probabilities themselves. The latter approach can suggest miscalibration where there is none. 

- `sim/sim_functions.R` - functions used to simulate data, apply different methods for creating calibration curves, calculate calibration metrics, and format results.
- `sim/sim[1,2].R` - scripts for running the two simulation studies. Output of simulations can be found in `sim/sim[1,2]-res/`
- `plots.R` - script creates all plots reported in paper (plus additional). Plots found in `sim/plots/`
- `p.Rmd` and `p.bib` - files needed to reproduce paper
