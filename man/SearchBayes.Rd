% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SearchBayes.R
\name{SearchBayes}
\alias{SearchBayes}
\alias{new_search_bayes}
\title{SearchBayes Class}
\usage{
new_search_bayes(options = NULL, keys = NULL, metric_name = NULL)
}
\arguments{
\item{options}{A \link{Options} class object.}

\item{keys}{A named character vector of keys.}

\item{metric_name}{A character scalar of metric_name.}
}
\value{
A new \code{SearchBayes} class object
}
\description{
A class defines a bayesian search task specification.
}
\section{Super class}{
\code{\link[ml4e:Task]{ml4e::Task}} -> \code{SearchBayes}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{metric_name}}{A metric name.}

\item{\code{is_new_grid}}{A logical wheather grid is new.}

\item{\code{done}}{A logical if finished}

\item{\code{error}}{A logical if error}

\item{\code{result}}{A task result.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{SearchBayes$new()}}
\item \href{#method-print}{\code{SearchBayes$print()}}
\item \href{#method-do}{\code{SearchBayes$do()}}
\item \href{#method-clone}{\code{SearchBayes$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="ml4e" data-topic="Task" data-id="eval_call_args">}\href{../../ml4e/html/Task.html#method-eval_call_args}{\code{ml4e::Task$eval_call_args()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ml4e" data-topic="Task" data-id="print_task">}\href{../../ml4e/html/Task.html#method-print_task}{\code{ml4e::Task$print_task()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Construct a new \code{SearchBayes} class object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SearchBayes$new(options = NULL, keys = NULL, metric_name = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{options}}{A \link{Options} class object.}

\item{\code{keys}}{A named character vector of keys.}

\item{\code{metric_name}}{A character scalar of metric name. If NULL, the first
metric_name in \code{self$options$measure$metric_names} is used.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{SearchBayes} class object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Print object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SearchBayes$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-do"></a>}}
\if{latex}{\out{\hypertarget{method-do}{}}}
\subsection{Method \code{do()}}{
Run a bayesian optimization. The result will be stored in
\code{self$result}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SearchBayes$do(
  init_grid_dt = NULL,
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
\item{\code{init_grid_dt}}{A data.frame of \code{init_grid_dt}.}

\item{\code{init_points}}{A integer scalar of \code{init_points}.}

\item{\code{n_iter}}{A integer scalar of \code{n_iter}.}

\item{\code{acq}}{A character scalar of \code{acq} (aquisition function).
Can be "ucb", "ei" or "poi".}

\item{\code{kappa}}{A numeric scalar of \code{kappa}.}

\item{\code{eps}}{A numeric scalar of \code{eps}.}

\item{\code{kernel}}{A list of \code{kernal} parameters.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{data.frame} of best parameters.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SearchBayes$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
