# DBmaps R package

## Overview
**DBmaps** is an R package designed to simplify database mapping and merging operations. It provides functions for efficiently joining, aggregating, and analyzing relational database tables.

## Features
- Supports multiple types of joins (inner, left, right, full)
- Allows custom aggregation functions
- Works seamlessly with data frames and database connections
- Optimized for performance on large datasets

## Installation

You can install the package directly from GitHub using `devtools`:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install DBmaps from GitHub
devtools::install_github("Divendra2006/DBmaps_R", subdir = "hard_solution/DBmaps", build_vignettes = TRUE)
```
**To build the vignette and check the package**
```r
devtools::document()  # Generate documentation

devtools::build_vignettes()  # Build the vignette

devtools::check()  # Check the package
```

## Usage

After installation, load the package:

```r
library(DBmaps)
```

### Example Usage

```r
# Example of construct a map of relationships between tables
result <- create.DB.map(
  tables <- list(table1=table1, table2=table2),
  by <- list(c(by.x = "id", by.y = "id")),
  join_types = "inner"
)

# Example of merges multiple tables based on specified keys and join types
multi.merge(
  tables <- list(table1=table1, table2=table2),
   by = "id", types = c("inner")
)

# Example of graphical representation of the database schema
result <- create.DB.map(
  tables <- list(table1=table1, table2=table2),
  by <- list(c(by.x = "id", by.y = "id")),
  join_types = "inner"
)
visualize.DB.map(result)
```

## Contributing
Contributions are welcome! To contribute:
1. Fork the repository
2. Create a new branch
3. Submit a pull request

## License
This package is licensed under the MIT License.

## Author
Developed by **Divendra Yadav**

