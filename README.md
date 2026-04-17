# mikrosim

Fortran implementation of a household-level car demand microsimulation model.

## What the program does

The model runs in two steps for each household:

1. **Binary logit** for probability to buy a new car (`p_newcar`).
2. **Multinomial logit** across car alternatives for car type probabilities (`p_ct`).

For each household and alternative, expected demand contribution is:

- `HH_weight * p_newcar * p_ct`

The program aggregates contributions to:

- Alternative level
- National fuel-type x income-class level
- Zone x fuel-type x income-class level

## Build

Requires `gfortran`:

- `gfortran -std=f2008 -O2 microsim_model.f90 -o microsim_model`

## Run

`./microsim_model <population_file> <zone_file> <car_file> <fuelprice_file> <incomeclass_file> <output_dir> [scenario_file]`

Example:

- `./microsim_model testdata/population.dat testdata/zones.dat testdata/cars.dat testdata/fuelprice.dat testdata/hhclass.dat output testdata/scenario.dat`

## Input files

The reader accepts comma, semicolon, tab, or space separated text rows.
Comment lines starting with `#` or `!` are ignored.

- `population_file`: person-level synthetic population (households are deduplicated by `Household_id`).
- `zone_file`: zone-level attributes (detached housing, charging indicators, etc.).
- `car_file`: base car alternatives (make/fuel/technical attributes).
- `fuelprice_file`: two columns: `FuelType`, `Price`.
- `incomeclass_file`: income class bounds (defaults are used if file is missing).
- `scenario_file` (optional): key-value multipliers/adders for technical and policy scenarios.

## Outputs

Written to `<output_dir>`:

- `adjusted_car_alternatives.csv`
- `expected_by_alternative.csv`
- `national_fuel_income.csv`
- `zonal_fuel_income.csv`
