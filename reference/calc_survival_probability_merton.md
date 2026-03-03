# Calculate survival probability

Function calculates survival probability for a maturity based on a
structural Merton model. For details on implementation please compare
`CreditRisk::Merton()`. Unlike `CreditRisk::Merton()` this
implementation:

1.  only holds functionality to calculate probability of survival

2.  can be called in vectorised fashion

3.  additionally checks that all input values are of the same length

4.  additionally checks input vectors for implausible values (`r` must
    be =\> 0 and all other args \> 0)

## Usage

``` r
calc_survival_probability_merton(L, V0, sigma, r, t)
```

## Arguments

- L:

  Numeric vector, holding debt values at maturity.

- V0:

  Numeric vector, holding company values at time t0.

- sigma:

  Numeric vector, holding volatility values.

- r:

  Numeric vector, holding risk free interest rates.

- t:

  Vector vector holding debt maturities.

## Value

A vector holding survival probabilities,
