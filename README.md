# Distributional Tools for Ordinal Wellbeing Data

This R project is a replication package for Gordon Anderson's working paper. It implements functions to calculate a range of statistics for univariate and multivariate ordinal categorical distributions (e.g. a Cantril Ladder from 1 to 10): 
- Expected Value of a Higher Outcome (Equally Weighted)
- Expected Value of a Higher Outcome (Category Weighted)
- Modal Category
- Informity
- GGGN (Greatest Good of the Greatest Number) Index
- Average Category (univariate only)
- Left Skewness (univariate only)

The package also provides a function to compute Gini's distributional transvariation between two distributions.

For precise mathematical definitions of these statistics, please refer to the original working paper.

The project applies these functions to a subset of Gallup World Poll data, examining well-being and perceived income separately as univariate distributions, and then jointly as a multivariate distribution. 

## Versions
The most recent version is available at ()

## Data Availability

Access to the Gallup World Poll data used in this project was obtained through the University of Toronto’s Map and Data Library. The data are not provided with this replication package; users must obtain access independently.

The analysis uses a subset of the Gallup World Poll data, restricted to the questions WP16 and WP2319, the survey years 2010, 2015, 2020, and 2025, and the following seven countries:

- Canada
- United States
- China
- India
- Russia
- Sweden
- United Kingdom

Responses to the well-being question (WP16) were coded on a scale from 0 to 10, with higher values representing higher levels of satisfaction.

Responses to the income satisfaction question (WP2319) were coded on a scale from 0 to 3, with higher values representing higher levels of satisfaction.

## Project Structure

### `src/math_tools.R`

Contains functions for computing distribution statistics and for computing Gini's transvariation between two distributions.

### `src/main.R`

Applies these functions in `src/math_tools.R` to Gallup World Poll data on well-being and income perception.

## Dependencies

Project dependencies are managed with `renv`. To restore the required packages, open the project in R and run:

```r
renv::restore()
```
## Reference
