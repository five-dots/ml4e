% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Project.R
\name{Project}
\alias{Project}
\alias{new_project}
\title{Project Class}
\usage{
new_project(
  formulas = NULL,
  datasets = NULL,
  seeds = NULL,
  models = NULL,
  measure = NULL,
  show_progress = TRUE,
  keep_data = FALSE,
  parallel = FALSE
)
}
\arguments{
\item{formulas}{A \link{Formulas} class object.}

\item{datasets}{A \link{Datasets} class object.}

\item{seeds}{A \link{Seeds} class object.}

\item{models}{A \link{Models} class object.}

\item{measure}{A \link{Measure} class object. If \code{NULL}, a Measure class
object with default metrics are used.}

\item{show_progress}{A logical scalar wheather to show a progress bar.}

\item{keep_data}{A logical scalar wheather to keep original data.
If \code{TRUE}, the original dataset are kept in predicted values and
cross-validation folds.}

\item{parallel}{A logical scalar wheather to calculate parallely.}
}
\value{
A new \code{Project} class object
}
\description{
A class defines a project specification.
}
\section{Super class}{
\code{\link[ml4e:Options]{ml4e::Options}} -> \code{Project}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{cv_table}}{A cross-validation table.}

\item{\code{grid_table}}{A grid search table.}

\item{\code{random_table}}{A random search table.}

\item{\code{bayes_tables}}{A list of bayesian search tables by \code{metric_name}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Project$new()}}
\item \href{#method-print}{\code{Project$print()}}
\item \href{#method-run_cv}{\code{Project$run_cv()}}
\item \href{#method-run_grid}{\code{Project$run_grid()}}
\item \href{#method-run_random}{\code{Project$run_random()}}
\item \href{#method-run_bayes}{\code{Project$run_bayes()}}
\item \href{#method-get_scores}{\code{Project$get_scores()}}
\item \href{#method-get_preds}{\code{Project$get_preds()}}
\item \href{#method-get_stacking_data}{\code{Project$get_stacking_data()}}
\item \href{#method-get_search_result}{\code{Project$get_search_result()}}
\item \href{#method-get_search_result_by_model}{\code{Project$get_search_result_by_model()}}
\item \href{#method-get_ranks}{\code{Project$get_ranks()}}
\item \href{#method-print_ranks}{\code{Project$print_ranks()}}
\item \href{#method-get_best_params}{\code{Project$get_best_params()}}
\item \href{#method-clone}{\code{Project$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="ml4e" data-topic="Options" data-id="print_options">}\href{../../ml4e/html/Options.html#method-print_options}{\code{ml4e::Options$print_options()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Construct a new \code{Project} class object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$new(
  formulas = NULL,
  datasets = NULL,
  seeds = NULL,
  models = NULL,
  measure = NULL,
  show_progress = TRUE,
  keep_data = FALSE,
  parallel = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{formulas}}{A \link{Formulas} class object.}

\item{\code{datasets}}{A \link{Datasets} class object.}

\item{\code{seeds}}{A \link{Seeds} class object.}

\item{\code{models}}{A \link{Models} class object.}

\item{\code{measure}}{A \link{Measure} class object. If \code{NULL}, a Measure class
object with default metrics are used.}

\item{\code{show_progress}}{A logical scalar wheather to show a progress bar.}

\item{\code{keep_data}}{A logical scalar wheather to keep original data.
If \code{TRUE}, the original dataset are kept in predicted values and
cross-validation folds.}

\item{\code{parallel}}{A logical scalar wheather to calculate parallely.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Project} class object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Print object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \code{Project} object by \code{invisible(self)}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-run_cv"></a>}}
\if{latex}{\out{\hypertarget{method-run_cv}{}}}
\subsection{Method \code{run_cv()}}{
Run cross-validation by each key combinations.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$run_cv(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Filtering expressions passed to \link[dplyr:filter]{dplyr::filter} or
\link[dplyr:slice]{dplyr::slice}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{Project} object by \code{invisible(self)}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-run_grid"></a>}}
\if{latex}{\out{\hypertarget{method-run_grid}{}}}
\subsection{Method \code{run_grid()}}{
Run grid search for parameters set by each key combinations.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$run_grid(..., num = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Filtering expressions passed to \link[dplyr:filter]{dplyr::filter} or
\link[dplyr:slice]{dplyr::slice}.}

\item{\code{num}}{A integer scalar of how many parameters to be calculated.
If \code{NULL}, all parameters in param set will be calculated.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{Project} object by \code{invisible(self)}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-run_random"></a>}}
\if{latex}{\out{\hypertarget{method-run_random}{}}}
\subsection{Method \code{run_random()}}{
Run random search for parameters set by each key combinations.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$run_random(..., num = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Filtering expressions passed to \link[dplyr:filter]{dplyr::filter} or
\link[dplyr:slice]{dplyr::slice}.}

\item{\code{num}}{A integer scalar of how many parameters to be calculated.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{Project} object by \code{invisible(self)}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-run_bayes"></a>}}
\if{latex}{\out{\hypertarget{method-run_bayes}{}}}
\subsection{Method \code{run_bayes()}}{
Run bayes search for parameters set by each key combinations.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$run_bayes(
  ...,
  metric_name = NULL,
  init_points = 4L,
  n_iter = 5L,
  acq = "ucb",
  kappa = 2.576,
  eps = 0,
  kernel = list(type = "exponential", power = 2L)
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Filtering expressions passed to \link[dplyr:filter]{dplyr::filter} or
\link[dplyr:slice]{dplyr::slice}.}

\item{\code{metric_name}}{A character scalar of metric name. If \code{NULL}, the
first metric_name in \code{self$measure$metric_names} is used.}

\item{\code{init_points}}{A integer scalar of \code{init_points}.}

\item{\code{n_iter}}{A integer scalar of \code{n_iter}.}

\item{\code{acq}}{A character scalar of aquisition function. Can be "ucb", "ei"
or "poi".}

\item{\code{kappa}}{A numeric scalar of \code{kappa}.}

\item{\code{eps}}{A numeric scalar of \code{eps}.}

\item{\code{kernel}}{A list of kernal parameters.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{Project} object by \code{invisible(self)}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_scores"></a>}}
\if{latex}{\out{\hypertarget{method-get_scores}{}}}
\subsection{Method \code{get_scores()}}{
Get cross-validation scores.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$get_scores(..., simplify = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Filtering expressions passed to \link[dplyr:filter]{dplyr::filter} or
\link[dplyr:slice]{dplyr::slice}.}

\item{\code{simplify}}{A logical scalar wheater to drop columns of a signle key.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{data.frame} class object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_preds"></a>}}
\if{latex}{\out{\hypertarget{method-get_preds}{}}}
\subsection{Method \code{get_preds()}}{
Get predictions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$get_preds(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Filtering expressions passed to \link[dplyr:filter]{dplyr::filter} or
\link[dplyr:slice]{dplyr::slice}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list of predictions by keys
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_stacking_data"></a>}}
\if{latex}{\out{\hypertarget{method-get_stacking_data}{}}}
\subsection{Method \code{get_stacking_data()}}{
Get stacking data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$get_stacking_data(..., prob = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Filtering expressions passed to \link[dplyr:filter]{dplyr::filter} or
\link[dplyr:slice]{dplyr::slice}.}

\item{\code{prob}}{A logical scalar wheather to use probability data.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{data.frame} of stacking data.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_search_result"></a>}}
\if{latex}{\out{\hypertarget{method-get_search_result}{}}}
\subsection{Method \code{get_search_result()}}{
Get merged search result.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$get_search_result(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Filtering expressions passed to \link[dplyr:filter]{dplyr::filter} or
\link[dplyr:slice]{dplyr::slice}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{data.frame} of search result.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_search_result_by_model"></a>}}
\if{latex}{\out{\hypertarget{method-get_search_result_by_model}{}}}
\subsection{Method \code{get_search_result_by_model()}}{
Get merged search result by model key.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$get_search_result_by_model(model_key = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{model_key}}{A character scalar of model key.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{data.frame} of search result.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_ranks"></a>}}
\if{latex}{\out{\hypertarget{method-get_ranks}{}}}
\subsection{Method \code{get_ranks()}}{
Get paramter ranking.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$get_ranks(..., metric_name = NULL, n = 5L)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Filtering expressions passed to \link[dplyr:filter]{dplyr::filter} or
\link[dplyr:slice]{dplyr::slice}.}

\item{\code{metric_name}}{A character scalar of metric name. If \code{NULL}, the
first  metric_name in \code{self$measure$metric_names} is used.}

\item{\code{n}}{A integer scalar of a number of the ranking.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list of ranking by keys
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print_ranks"></a>}}
\if{latex}{\out{\hypertarget{method-print_ranks}{}}}
\subsection{Method \code{print_ranks()}}{
Print parameters ranking.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$print_ranks(..., metric_name = NULL, n = 5L)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Filtering expressions passed to \link[dplyr:filter]{dplyr::filter} or
\link[dplyr:slice]{dplyr::slice}.}

\item{\code{metric_name}}{A character scalar of metric name. If NULL, the first
metric_name in \code{self$measure$metric_names} is used.}

\item{\code{n}}{A integer scalar of a number of the ranking.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{Project} object by \code{invisible(self)}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_best_params"></a>}}
\if{latex}{\out{\hypertarget{method-get_best_params}{}}}
\subsection{Method \code{get_best_params()}}{
Get best parameters by grid/bayes search.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$get_best_params(..., metric_name = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Filtering expressions passed to \link[dplyr:filter]{dplyr::filter} or
\link[dplyr:slice]{dplyr::slice}.}

\item{\code{metric_name}}{A character scalar of metric name. If \code{NULL}, the
first metric_name in \code{self$measure$metric_names} is used.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list of best parameters by keys
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Project$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
