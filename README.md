# mikrosim

Fortran microsimulation model for:
1. Household probability to buy a new car (binary logit)
2. Car type choice (multinomial logit)

## Program stages implemented

- Initialization of model parameters for both sub-models.
- Reading synthetic population data (up to 100000 rows).
- Reading zone data.
- Reading base car alternatives.
- Applying scenario adjustments to technical/policy-sensitive attributes.
- Building transformed variables for all alternatives.
- For each household:
  - computing `p_newcar`
  - computing car type probabilities `p_ct`
  - accumulating `weight * hhvikt * p_newcar * p_ct`
- Writing aggregated outputs.

## Files and expected formats

Input files (in `data/`):
- `population.csv`
- `zones.csv`
- `base_car_alternatives.csv`
- `Fuelprice.dat`
- `scenario.dat`

Output files (in `output/`):
- `alternative_demand.csv`
- `national_fuel_income.csv`
- `zonal_fuel_income.csv`

## Build and run

```bash
make build
make run
```

## Notes

- Target source file is `src/Mikrosim_ny.f90`.
- National output is aggregated by fuel type and household income class (`HI1..HI5`).
- Zonal output provides the same dimensions for each zone.
