# Keep rows that fulfill constraints of the merton model

Keep rows that fulfill constraints of the merton model as line out for
[`calc_survival_probability_merton()`](calc_survival_probability_merton.md).

## Usage

``` r
keep_merton_compatible_rows(data, stage)
```

## Arguments

- data:

  A tibble holding at least the columns `debt`, `equity_0_baseline` or
  `equity_t_baseline`, `equity_0_late_sudden` or `equity_t_late_sudden`,
  `volatility`, `risk_free_rate` and `term`.

- stage:

  String, indicating if checks are done for overall or annual results.

## Value

Tibble holding rows from `data` that are compatible with constraints of
[`calc_survival_probability_merton()`](calc_survival_probability_merton.md).
