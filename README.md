# vizintel

**vizintel** is an R package built as a course project. It is a toolkit for
understanding data and checking whether a plot communicates it correctly.

Five core visualization intelligence functionalities are available:

1. `viz_recommend()` -- suggests chart types based on column profiles
2. `viz_audit()` -- takes a finished ggplot and checks it for common mistakes
3. `viz_explore()` -- explores relationships between numeric variables using correlation analysis and a heatmap
4. `viz_alternatives()` -- suggests alternative visualization types for an existing ggplot
5. `viz_accessibility()` -- checks a finished ggplot for common accessibility issues

The package also contains internal helper functionality: 
`profile_data()` for profiling data frames.

---

## Installation

```r
# from the local repo
devtools::install("path/to/R-package")
```

Requires R >= 4.0. Packages `e1071`, `ggplot2`, `dplyr`, and `gridExtra` are
installed automatically as dependencies. The `hexbin` package is suggested
(needed only for hexbin chart panels in `plot.viz_recommend_result()`).

---

## What it does

### 1. Getting chart recommendations

`viz_recommend()` looks at your data frame's columns, works out what type each
one is, and suggests the most appropriate chart for each.

```r
library(vizintel)

# Recommendations for every column
viz_recommend(mtcars)

# Recommendations for a specific pair of columns
viz_recommend(mtcars, x = "wt", y = "mpg")

# Force a particular goal
viz_recommend(mtcars["mpg"], goal = "distribution")
```

Sample output:

```
== vizintel chart recommendations ==

  mpg                  ->  histogram      (score: 1.00)
                           "Shows the full distribution of a numeric variable."
                       ->  density        (score: 0.95)
                           "Smoother alternative to a histogram for continuous data."
                       ->  log_histogram  (score: 0.90)
                           "Better than a histogram when the data is heavily right- or left-skewed."
```

The `goal` parameter accepts: `"distribution"`, `"compare"`, `"trend"`,
`"relationship"`, `"composition"`. When left `NULL`, the goal is inferred from
the column type.

#### Plotting recommendations

`viz_recommend()` stores the data inside its result, so you can call `plot()`
directly on it — no need to pass the data again. Each recommended chart type
is rendered as a real `ggplot` panel and all panels are arranged in a single
view using `gridExtra::grid.arrange()`:

```r
res <- viz_recommend(mtcars["mpg"])
plot(res)                          # grid of histogram / density / log_histogram

res2 <- viz_recommend(mtcars, x = "wt", y = "mpg")
plot(res2)                         # scatter + hexbin panels side-by-side

plot(res, ncol = 1)               # pass extra args to grid.arrange()
```

Each panel title shows the chart type and its fit score,
e.g. `histogram (score: 1.00)`.

---

### 2. Auditing a plot

`viz_audit()` takes any finished ggplot and runs four checks on it. Every
issue now includes a plain-English **explanation** field, printed under the
technical message as `"In plain terms: ..."` so non-expert readers can
understand what the problem means.

```r
library(ggplot2)

# A bar chart where the y-axis does not start at 0
p <- ggplot(data.frame(x = c("A", "B"), y = c(50, 80)), aes(x, y)) +
  geom_col() +
  coord_cartesian(ylim = c(40, 85))

report <- viz_audit(p)
print(report)
```

Output:

```
== vizintel audit report ==
Geoms detected: GeomCol
Issues: 1 critical, 0 warning, 0 info

x [CRITICAL] Y-axis starts at 40, not 0 — bar heights are visually exaggerated.
   In plain terms: Because the Y-axis does not start at zero, the bars appear
   much taller relative to each other than the actual values justify. A small
   real difference can look enormous, making the chart misleading at a glance.
```

```r
summary(report)
# [FAIL] 1 issue(s) -- 1 critical / 0 warning / 0 info
```

Each issue object also carries the explanation as a field you can access
programmatically:

```r
report$issues[[1]]$explanation
```

The four checks it runs:

| Check | What triggers it | Level |
|-------|-----------------|-------|
| Truncated axis | Bar/col plot where y-axis does not start at 0 | critical |
| Lie factor | Displayed axis range is more than 5% off the data range | warning |
| Overplotting | More than 30% of scatter points land on the same grid cell | warning |
| Dual y-axis | A secondary y-axis is defined | info |

---

### 3. Profiling a data frame

`profile_data()` is internal -- used by `viz_recommend()` and
`viz_explore()` to identify and summarise columns. You can call 
it directly with `:::` if you want to inspect the raw column statistics.

```r
df <- data.frame(
  score = c(72, 85, 61, 90, 78),
  grade = c("B", "A", "C", "A", "B"),
  date  = seq(as.Date("2024-01-01"), by = "week", length.out = 5)
)

vizintel:::profile_data(df)
```

Returns one row per column with: type classification, missing count, skewness,
kurtosis, normality test result, cardinality (categorical), regularity (datetime).

---

### 4. Exploring relationships between variables

`viz_explore()` explores relationships between numeric variables using a
correlation matrix and a correlation heatmap.

```r
library(vizintel)

result <- viz_explore(mtcars)

# View the correlation matrix
result$correlation

# Display the correlation heatmap
result$plot
```

The function identifies numeric variables automatically and calculates the
correlation between each pair of numeric variables.

The result contains:

| Element | Description |
|---------|-------------|
| `correlation` | Correlation matrix of the numeric variables |
| `plot` | ggplot2 correlation heatmap |

At least two numeric variables are required. Non-data-frame input and data
with fewer than two numeric variables raise clear errors.

---

### 5. Finding visualization alternatives

`viz_alternatives()` suggests alternative visualization types for an existing
ggplot based on its geometry and statistical transformation.

```r
library(ggplot2)

p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

viz_alternatives(p)
```

Example result:

```text
[1] "2D Density Plot"
[2] "2D Histogram"
```

The alternatives depend on the current visualization:

| Current visualization | Alternatives |
|------------------------|--------------|
| Scatter plot | 2D Density Plot, 2D Histogram |
| Bar/column plot | Dot Plot, Lollipop Plot |
| Boxplot | Violin Plot, Strip Plot |
| Histogram | Density Plot, ECDF |

Histograms are distinguished from regular bar plots using the plot's
statistical transformation.

---

### 6. Checking visualization accessibility

`viz_accessibility()` checks an existing ggplot for common accessibility
issues that may make a visualization difficult to interpret.

```r
library(ggplot2)

p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point()

viz_accessibility(p)
```

The function checks for:

| Check | What it detects | Level |
|-------|------------------|-------|
| Colour count | More than 8 colour/fill values represented in the visualization | warning |
| Colour encoding | Categories distinguished through colour/fill | info |
| Text size | Text layers using size below 3 | warning |
| Shape count | More than 8 shape values | warning |
| Label count | More than 30 text labels | warning |
| Overplotting | High concentration of overlapping points | warning |
| Overplotting mitigation | Transparency used for overlapping points | info |


The function returns a list containing an `issues` element. Each issue
contains a severity level, check name, and descriptive message.

If no issues are detected, the function returns:

```text
No obvious accessibility issues detected.
```

The function provides practical checks for common accessibility concerns.
It does not perform full WCAG compliance testing.

---

## Future work

The five core visualization intelligence functionalities are now implemented:

- `viz_recommend()` -- visualization recommendation
- `viz_audit()` -- visualization integrity auditing
- `viz_explore()` -- multivariate exploration
- `viz_alternatives()` -- visualization alternatives
- `viz_accessibility()` -- visualization accessibility

Future work may focus on improving the existing checks, expanding supported
visualization types, and adding more test coverage.

---

## Files in the package

```
R/
  profile-data.R           profile_data() and classify_column() -- internal engine
  extract-plot-geometry.R  shared ggplot geometry extraction for 
                           viz_audit(), viz_alternatives(), and
                           viz_accessibility()
  viz-recommend.R          viz_recommend(), plot.viz_recommend_result(),
                           taxonomy table, scoring logic, and print method
  viz-audit.R              viz_audit(), four check functions (each with an
                           explanation field), print/summary methods
  viz-explore.R            viz_explore(), correlation analysis and heatmap
  viz-alternatives.R       viz_alternatives(), visualization alternatives
  viz-accessibility.R      viz_accessibility(), accessibility checks

tests/testthat/
  test-profile-data.R      7 tests for profile_data()
  test-viz-recommend.R     12 tests for viz_recommend() and plot()
  test-viz-audit.R         11 tests for viz_audit() (incl. explanation field)
  test-viz-explore.R       5 tests for viz_explore()
  test-viz-alternatives.R  5 tests for viz_alternatives()
  test-viz-accessibility.R 5 tests for viz_accessibility()
```

---


## Authors

T Pooja and Ronesha Navaneethan — MIT License 2026

Supervisor: Dr Amrit Pal