# Anions and Cations

Balances of anions and cations are used throughout bcgwcat2 in order to
assess various water chemistry metrics.

In particular they are used to calculate

- **Charge Balances** (the `charge_balance` parameter added by
  [`fmt_minimal()`](https://bcgov.github.io/bcgwcat2/reference/fmt.md))
- **Water Types** (the `water_type` column added by
  [`fmt_minimal()`](https://bcgov.github.io/bcgwcat2/reference/fmt.md))
- **Piper plots**
  [`piper_plot()`](https://bcgov.github.io/bcgwcat2/reference/piper_plot.md)
- **Stiff plots**
  [`stiff_plot()`](https://bcgov.github.io/bcgwcat2/reference/stiff_plot.md)

Generally speaking, charge balances are used to assess whether the water
chemistry data is valid. A charge balance is valid if it is not missing
and is between -10% and 10%. Water types are only calculated for samples
with valid charge balances, and by default, piper plots and stiff plots
only use these samples.

The exact types of anions and cations used depend on the calculation.

| ions | parameter | charge balance | water type | piper plots | stiff plots |
|----|----|----|----|----|----|
| anions |  |  |  |  |  |
| Cl | chloride_dissolved | X | X | X | X |
| SO4 | sulfate_dissolved | X | X | X | X |
| HCO3 | bicarbonate_alkalinity_as_caco3 |  | X | X | X |
| CO3 | carbonate_alkalinity_as_caco3 |  |  | X |  |
| F | fluoride_dissolved | X |  |  |  |
| NO3 | nitrate_no3_dissolved | X |  |  |  |
| NO2 | nitrogen_nitrite_dissolved_no2 | X |  |  |  |
| Alkalinity | alkalinity_total_4.5_as_caco3 | X | \* | \* | \* |
| cations |  |  |  |  |  |
| Ca | calcium_dissolved | X | X | X | X |
| Mg | magnesium_dissolved | X | X | X | X |
| Na | sodium_dissolved | X | X | X | X |
| K | potassium_dissolved | X | X | X |  |
| Al | aluminum_dissolved | X |  |  |  |
| Cu | copper_dissolved | X |  |  |  |
| Fe | iron_dissolved | X |  |  |  |
| Mn | manganese_dissolved | X |  |  |  |
| Zn | zinc_dissolved | X |  |  |  |
| NH4 | nitrogen_ammonia_dissolved | X |  |  |  |
| pH | ph | X |  |  |  |
| \* Used if HCO3 is missing |  |  |  |  |  |

### Charge balances

Charge balances are calculated based on a formula from ALS.

Although EnMoDS data contains charge balances, as well as the anion and
cation sums used to calculate these values, potential changes in
workflows over the years have made it difficult to ascertain exactly how
charge balances were calculated in older samples. This resulted in
discrepancies between EnMoDS and locally calculated charge balances.
Therefore for consistency, we calculate charge balances for all samples
using the ALS formula below.

**Note:** This means that the original EnMoDS charge balances, anion
sums, and cation sums, have been **renamed to `charge_balance_enmods`,
`anion_sum_enmods` and `cation_sum_enmods`**. bcgwcat2 will use the
created values `charge_balance`, `anion_sum` and `cation_sum`.

One difference between our calculation and that of ALS, is that we use
more significant digits when calculating MEQ.

Charge balances use following measures in MEQ

- **Anions** Cl, SO4, F, NO3, NO2, Alkalinity
- **Cations** Ca, Mg, Na, K, Al, Cu, Fe, Mn, Zn, NH4, and pH (lab
  measurements only, not field pH)

And are calculated as follows

$`\textrm{Anion sum} = Cl + SO_4 + F + NO_3 + NO_2 + \textrm{Alkalinity}`$

``` math
\textrm{Cation sum} = Ca + Mg + Na + K + Al +
             Cu + Fe + Mn + Zn + NH_4 +
             (10 ^{-\textrm{pH}}) * 1000
```

$`\textrm{Charge balance} = 100 \times (\textrm{Cation Sum} - \textrm{Anion sum}) / (\textrm{Cation Sum} + \textrm{Anion Sum})`$

Charge balances are positive if there are more cations than anions, and
negative if there are more anions than cations. Anion and cation sums
are rounded to two decimal places and charge balances are rounded to one
decimal place. Charge balances are checked for validity after rounding.

Missing values are ignored (i.e. generally treated as 0). However, if
all values for cations or anions are missing the charge balance is `NA`.

This means that Charge Balances will be invalid if there are too many
missing measures.

For most calculations, if there are repeated MEQ values for a parameter
in a sample, only the first value is used.

### Water Types

Water types identify the major cations and anions in a water sample. In
bcgwcat2, we calculate water types using the following ions:

- **Anions** Cl, SO4, HCO3
- **Cations** Ca, Mg, Na, and K

Water type is only calculated for samples with valid charge balances.
For all other samples, `water_type` is `NA`.

Each ion is calculated as a proportion of the total MEQ of these anions
and cations. All ions with a proportion of at least 10% (after rounding
to three decimal places) are listed in descending order, cations first.
If HCO3 is missing, we use Alkalinity as a replacement (indicated by an
’\*’ on HCO3 in the water type). Otherwise, missing ions are ignored
(i.e. treated as 0). The column `missing_ion` indicates whether there is
a problem with the water type, such that it is missing all cations or
all anions.

For example with this data

| bcgwcat_param                   | result_value | result_unit |
|---------------------------------|--------------|-------------|
| chloride_dissolved              | 0.0226       | meq/l       |
| sulfate_dissolved               | 0.0208       | meq/l       |
| bicarbonate_alkalinity_as_caco3 | 1.5400       | meq/l       |
| alkalinity_total_4.5_as_caco3   | 1.7000       | meq/l       |
| calcium_dissolved               | 0.1870       | meq/l       |
| magnesium_dissolved             | 0.4900       | meq/l       |
| sodium_dissolved                | 0.4650       | meq/l       |
| potassium_dissolved             | 0.0665       | meq/l       |
| charge_balance                  | 0.5000       | %           |

We would get the following water type.

| water_type | missing_ion |
|------------|-------------|
| Mg-Na-HCO3 | FALSE       |

HCO3 is present, so Alkalinity isn’t used, and Ca, K, Cl and SO4 are
each below 10%. Here the charge balance is supplied for illustration,
rather than being calculated from these values.

### Piper and Stiff plots

Piper and Stiff plots both use MEQ values. By default, they only use
samples with valid charge balances (`qc_valid_only`), and use Alkalinity
if HCO3 is missing (`qc_with_alkalinity`).

**Piper plots** combine Na with K, and HCO3 with CO3. If one of these
pairs is missing, the other is used alone. Samples missing Ca, Mg, Cl,
SO4, or both ions in a pair, are omitted from the plot.

Piper plots also omit outliers by default (`qc_omit_outliers`; see
[`dominant_water_types()`](https://bcgov.github.io/bcgwcat2/reference/dominant_water_types.md)).
This also omits samples which don’t have a water type, including those
with invalid charge balances.

**Stiff plots** only include samples which have all of Ca, Mg, Na, HCO3
(or Alkalinity), SO4, and Cl.
