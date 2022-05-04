# tidyCDISC 0.0.4

# tidyCDISC 0.0.3

* For `tippy` package, added lower bound on to version `1.0.0` since it includes more user friendly options.

# tidyCDISC 0.0.2

This release (PR #65) performs a lot of functions, testing on many non-pilot ADSL files to ensure app's robustness beyond just the CDISC pilot data:

* closes #64: Agg class objects lost droppability when only ADSL uploaded
* get's rid of annoying font awesome message about verifying an icon exists
* updates gt code that was deprecated
* Made sure 'PARAM' exists in the data when trying to label a block
* got rid of unnecessary argument in col_for_list() and col_for_list_expr()
* added some code that helped the FREQ block produce the correct results when ran locally (via a reproducibility script)
* a little code clean up for readability

# tidyCDISC 0.0.1

This release adds meat to the previous skeleton version of `tidyCDISC`. The initial development release was intended to be a "bare bones" platform that could serve as a foundation to build a great clinical data exploratory tool. Version `0.0.1` enriches the user experience by adding useful & commonly needed features for analysis. For more on this release, see [the announcement](https://biogen-inc.github.io/tidyCDISC/articles/Blog.html#announcing-tidycdisc-0-0-1-1) on our blog.


- Major Features added, by tab:
  - **Table Generator**
    - New "STAT Blocks", such as `Y FREQ`, `MAX FREQ`, `NON-MISSING`, and a pair of `NESTED FREQ` blocks
    - Interact with `ADAE`
    - 17 new 'standard analysis' tables in the `ADAE` and `ADLB` space.
    - An R Script Downloader to reproduce analysis performed in the app
  - **Population Explorer**
    - New Chart types
      - line plot - means over time
      - Heat map - endpoint correlations
      - Kaplan Meier Curve (when `TTE` class data uploaded)
- Upgraded `pkgdown` site documentation to be more robust and hassle-free for developers
- Depends on updated version of IDEAFilter (>= 0.1.0.9000)
- Squashed bugs and other minor improvements

# tidyCDISC 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
