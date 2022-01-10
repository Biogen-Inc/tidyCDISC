# tidyCDISC 0.0.1

This release adds meat to the previous skeleton version of `tidyCDISC`. The initial development release was intended to be a "bare bones" platform that could serve as a foundation to build a great clinical data exploratory tool. Version `0.0.1` enriches the user experience by adding useful & commonly needed features for analysis. For more on this release, see [the announcement](file:///C:/Users/aclark5/Documents/GitHub2/tidyCDISC/docs/articles/Blog.html#announcing-tidycdisc-0-0-1-1) on our blog.


- Major Features added, by tab:
  - **Table Generator**
    - New "STAT Blocks", such as `Y FREQ`, `MAX FREQ`, `NON-MISSING`, and a pair of `NESTED FREQ` blocks
    - 17 new 'standard analysis' tables for `ADAE`s and `ADLB`s.
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
