[![Build Status](https://travis-ci.org/aktest/GroCurveFit.svg?branch=master)](https://travis-ci.org/aktest/GroCurveFit)

# GroCurveFit
---

This module was developed for analysis of results of growth experiments. It contains Fit Growth Curve method that fits growth curve and returns four characteristics of the curve: lag-phase length, maximal growth rate, maximal growth and integral (area under the growth curve). For calculation of growth parameters, this method utilizes the GcFit function of R package 'grofit' (Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). grofit: Fitting Biological Growth Curves with R. Journal of Statistical Software, 33(7), 1-21. URL http://www.jstatsoft.org/v33/i07/). User can select between model-free spline fits and model-based curve fits. Model-based fits supports four different models: logistic growth, Gompertz growth, modified Gompertz growth and Richards growth. However, in some cases parametric models cannot be fitted to the real data, especially when diauxic growth occurs or stationary phase was not reached in the experiment. In that cases, model-free spline fit would be a preferred option.
