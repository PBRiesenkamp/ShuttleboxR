# ShuttleboxR 0.0.0.9001

- Added `calc_Tbreadth()` for effective selected thermal breadth based on the
  frequency distribution of `core_T`.
- Made metadata optional when importing a single ShuttleSoft file.
- Added direct `trial_start`, `mass`, `initial_T`, `a_value`, and `b_value`
  arguments to `read_shuttlesoft()`.
- `read_shuttlesoft()` now prepares imported files by default.
- Made `file_prepare()` robust when no trial start time is supplied.
- Added direct thermal-model arguments to `calc_coreT()` and clarified that
  `a_value` and `b_value` require external calibration.
- Made metadata optional in `read_shuttlesoft_project()`.
- Added `Tbreadth` to `calc_project_results()` and made recalculation of
  `core_T` optional rather than automatic.
