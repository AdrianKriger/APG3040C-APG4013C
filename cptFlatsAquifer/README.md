**Trend surfaces using R**

A trend surface is a map of some continuous variable, computed as a function of the coördinates. This corresponds to the concept of a geographic trend, where the variable changes its value along a geographic gradient.

The trend can be modelled as a linear trend, i.e., the variable increases or decreases a fixed amount for each unit change in the coördinates in
some direction. This is called a first-order trend surface. 

It can also be modelled as a polynomial trend, i.e., a linear model of some polynomials of the coördinates, for example, a quadratic, which is called asecond-order trend surface. 

It can also be modelled as an empirical smooth function of the coördinates, for example a generalized additive model or a minimum-curvature surface (thin-plate spline).

The residuals from any of the above approaches may have spatial structure. This has two implications:

1. The OLS fit may not be optimal, and a Generalized Least Squares (GLS) trend should be fit (§10).
2. The OLS or GLS trend surfaces can be modified by
          (1) interpolating the residuals from the trend-surface fit and
   (2) adding these to the trend.
4. The trend and local deviations can be modelled together with Uni-
versal Kriging (UK) (§13).

In this exercise we compare these approaches.

We use a dataset that is well-suited to illustrate the concepts of trend surface: a set of observations on the elevation above mean sea level of the top of the Cape Town aquifer, South Africa measured at ~ 30 wells.
