# Recalculate core body temperature

Recalculates core body temperature from ambient chamber temperature and
a calibrated thermal-lag model. Most ShuttleSoft files already contain
`core_T`, so this function is optional.

## Usage

``` r
calc_coreT(data, mass = NULL, initial_T = NULL, a_value = NULL, b_value = NULL)
```

## Arguments

- data:

  An organised shuttle-box data frame.

- mass:

  Optional body mass.

- initial_T:

  Optional initial body temperature.

- a_value:

  Optional calibrated coefficient `a`.

- b_value:

  Optional calibrated coefficient `b`.

## Value

The data frame with recalculated `core_T`, `ambient_T`, and `k`.

## Details

The model uses `k = a_value * mass^b_value`. The `a_value` and `b_value`
coefficients must come from an appropriate calibration or published
source; they cannot be inferred from the ShuttleSoft file itself.

Values may be supplied directly as arguments or stored in columns of the
same names. Direct arguments take priority. When `initial_T` is omitted,
the first valid existing `core_T` value is used where possible.

## Examples

``` r
if (FALSE) { # \dontrun{
fish <- calc_coreT(
  fish,
  mass = 12.4,
  a_value = 0.05,
  b_value = -0.25
)
} # }
```
