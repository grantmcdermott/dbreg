\name{dbtree}
\alias{dbtree}
\title{Run a regression tree on a database backend}
\description{
Fits an approximate regression tree using SQL aggregation on a database backend.
Numeric splits are evaluated on binned candidates, while categorical splits are
evaluated as one-vs-rest. Computation is pushed to the database, with only
aggregated results returned to R.
}
\usage{
dbtree(
  fml,
  conn = NULL,
  table = NULL,
  data = NULL,
  path = NULL,
  weights = NULL,
  bin_scope = c("global", "node"),
  n_bins = 128,
  bin_method = c("quantile", "width"),
  bin_sample_frac = NULL,
  max_depth = 5,
  min_split = 20,
  min_leaf = 5,
  min_gain = 0,
  max_cat = 50,
  drop_missings = TRUE,
  verbose = getOption("dbreg.verbose", FALSE),
  ...
)
}
\arguments{
\item{fml}{A \code{\link[stats]{formula}} representing the relation to be estimated.
Interactions are not supported. Fixed effects after \code{|} are not supported.}

\item{conn}{Database connection, e.g. created with \code{\link[DBI]{dbConnect}}.}

\item{table}{Character string giving the name of the data table in an existing
database connection.}

\item{data}{R dataframe copied into an ephemeral DuckDB connection.}

\item{path}{Character string giving a path to data files, passed to the \code{FROM}
clause for DuckDB and compatible backends.}

\item{weights}{Character string specifying the column name to use as weights,
or \code{NULL} for unweighted trees. Weights must be non-negative; zero-weight
rows are dropped.}

\item{bin_scope}{One of \code{"global"} or \code{"node"}.}

\item{n_bins}{Integer number of bins for numeric split candidates.}

\item{bin_method}{Binning method for numeric features: \code{"quantile"} or \code{"width"}.}

\item{bin_sample_frac}{Optional fraction in (0,1] used to compute global bins.}

\item{max_depth}{Maximum tree depth (root is depth 0).}

\item{min_split}{Minimum number of rows required to split a node.}

\item{min_leaf}{Minimum number of rows required in each leaf.}

\item{min_gain}{Minimum SSE reduction required to accept a split.}

\item{max_cat}{Maximum distinct categories allowed for categorical predictors.}

\item{drop_missings}{Logical indicating whether rows with missing values are dropped.}

\item{verbose}{Logical. Print progress messages?}

\item{...}{Additional arguments (currently unused).}
}
\value{
A list of class \code{"dbtree"} containing a node table and training metrics.
}
\examples{
df = data.frame(y = c(1, 1, 1, 10, 10, 10), x = c(0, 0, 0, 1, 1, 1))
mod = dbtree(y ~ x, data = df, max_depth = 1, n_bins = 2)
predict(mod, df)
}
