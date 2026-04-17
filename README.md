# mikrosim

This repository now includes a runnable Fortran implementation of the multinomial logit model described below.

## Model specification

Multinomial logit model with 3 alternatives. Utility function variables:
- `Purchase_cost`
- `Fuel_cost`
- `Curb_weight`

Parameters read from standard input:
- `b1`, `b2`, `b3`, `b4`

Data (8 observations):

| RowID | Purchase_cost | Fuel_cost | Curb_weight |
|------:|--------------:|----------:|------------:|
| 1 | 100 | 6 | 1200 |
| 2 | 120 | 8 | 1300 |
| 3 | 140 | 7 | 1600 |
| 4 | 160 | 4 | 1800 |
| 5 | 180 | 9 | 2300 |
| 6 | 200 | 8 | 1700 |
| 7 | 300 | 14 | 1900 |
| 8 | 400 | 16 | 2000 |

## Development environment setup

Install GNU Fortran (`gfortran`) and run:

```bash
make build
```

Run the app:

```bash
make run
```

When prompted, enter 4 parameter values, for example:

```text
0.5 0.2 -0.01 -0.05
```

The program prints probabilities `P(alt1..alt3)` for each row.
