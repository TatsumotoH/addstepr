% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RecipeSteps.R
\name{step_dbscan_fpc}
\alias{step_dbscan_fpc}
\title{step_dbscan: a recipe step for clustering by dbscan.
fpcパッケージのdbscan関数を用いたstep関数}
\usage{
step_dbscan_fpc(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  eps = NULL,
  minPts = NULL,
  retain = FALSE,
  id = rand_id("dbscan_fpc")
)
}
\arguments{
\item{role}{For model terms created by this step, what analysis role should
they be assigned?}

\item{trained}{A logical to indicate if the quantities for preprocessing
have been estimated.}

\item{skip}{A logical. Should the step be skipped when the recipe is baked
by [recipes::bake.recipe()]?}

\item{eps}{The parameter eps defines radius of neighborhood around a point x.}

\item{minPts}{The parameter MinPts is the minimum number of neighbors within “eps” radius.}

\item{retain}{Alogical to specify whether the original predictors should be retained along with the new embedding 
variables.}

\item{id}{A character string that is unique to this step to identify it.}
}
\description{
step_dbscan: a recipe step for clustering by dbscan.
fpcパッケージのdbscan関数を用いたstep関数
}
