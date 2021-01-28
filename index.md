Lecture 4: Stan Demo + Multivariate Normal intro
================

#### Stan Overview

Recall the weather data from Bridger Bowl in January of 2021.

``` r
temp <- c(26, 45, 44, 36, 22, 25, 31, 31, 37, 34, 35, 37, 32, 31)
```

We are going to fit a few this model in stan but we will vary the prior
distribution to assess how the posterior distribution and results
change.

##### 1\. Stan Code

First re-write the stan code to allow mu and sigma to be specified by
the user and passed to stan.

``` stan
data {
  int<lower=0> N;
  vector[N] y;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  y ~ normal(mu, sigma);
  mu ~ normal(20, 10);
}
```

##### 2\. Compute Posterior with Different Priors

Use the model to estimate the mean high temperature, but using the
different prior structures below:

1.  mu \~ N(20,10^2)

2.  mu \~ N(20,1000^2)

3.  mu \~ N(20,.1^2)

4.  Explore another prior distribution. If you want to challenge
    yourself even consider a non-normal distribution.

##### 3\. Visualize Posterior and Prior

1.  mu \~ N(20,10^2)

2.  mu \~ N(20,1000^2)

3.  mu \~ N(20,.1^2)

#### Multivariate Normal Distribution

Next we will segue from standard linear models to analyzing correlated
data.

First we will start with the a bivariate normal distribution: y \~
N(theta,sigma), where theta is a mean vector and sigma = sigmasq \* I is
a covariance matrix.

To provide a motivating context, not consider jointly estimating the
temperature at Bridger Bowl *and* Big Sky Resort.

##### 1\. Simulate independent bivariate normal

Simulate a set of temperature values from each location, where the
temperature values are independent (sigma = sigmasq \* I)

``` r
library(mnormt)
n <- 100
```

Then create a few graphs to show marginal distribution of temperature as
well as how the temperatures evolve in time.

##### 2\. Simulate correlated bivariate normal

Simulate a set of temperature values from each location, where the
temperature values are not independent (sigma = sigmasq \* H), where H
is a correlation matrix. (Note there are some constraints we will
discuss later)

Then create a few graphs to show marginal distribution of temperature as
well as how the temperatures evolve in time.

##### 3\. Write STAN code for bivariate normal

Write stan code that will allow you to estimate theta and sigma
(including H)

``` stan
data {
  int<lower=0> p;
  int<lower=0> N;
  matrix[N,p] y;
}
```

##### 4\. Use STAN to estimate bivariate normal parameters

Use your stan code to estimate theta and sigma (including H and sigmasq)

##### 5\. Final Thoughts About Correlation

In many statistical models there is an assumption about independence.
When independence is violated, uncertainty is under estimated and in
incorrect inferences can be made.

While lack of independence often has a negative connotation, in spatial
statistics we can actually exploit correlation. For instance, by knowing
the temperature at the weather station at Bozeman High School or Bridger
Bowl, we can estimate temperature at other locations.
