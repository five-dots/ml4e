% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Task.R
\name{Task}
\alias{Task}
\title{Task Class}
\description{
A base class describe a machine learning task specification. A task is
defined by a \link{Options} and \code{keys} which is named character vector specifiyng
option's key. The main functionality is evaluate quoted call arguments and
assign the acutal values to be called in the later task.
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{options}}{A \link{Options} object.}

\item{\code{keys}}{A named character vector of keys.}

\item{\code{formulas}}{A \link{Formulas} object.}

\item{\code{datasets}}{A \link{Datasets} object.}

\item{\code{seeds}}{A \link{Seeds} object.}

\item{\code{models}}{A \link{Models} object.}

\item{\code{measure}}{A \link{Measure} object.}

\item{\code{labels}}{A vector of labels (response variable).}

\item{\code{label_var}}{A label (response) variable name.}

\item{\code{label_levels}}{Levels of label (response) variable.}

\item{\code{task_type}}{A task type.}

\item{\code{show_progress}}{Wheather to show progress messages.}

\item{\code{keep_data}}{Wheather to keep data.}

\item{\code{parallel}}{Wheather to calculate parallely.}

\item{\code{formula}}{A tasks' formula.}

\item{\code{dataset}}{A task's dataset.}

\item{\code{train}}{A task's train data.}

\item{\code{test}}{A task's test data.}

\item{\code{resample_call}}{A task's resample call.}

\item{\code{seed}}{A task's seed.}

\item{\code{model}}{A task's model.}

\item{\code{search_seed}}{A task's search seed for resampling.}

\item{\code{cv_seed}}{A task's cv seed for resampling.}

\item{\code{model_seed}}{A task's model seed.}

\item{\code{id_col}}{A task's datasets' id_col.}

\item{\code{test_ids}}{A task's datasets' test_ids.}

\item{\code{preproc_call}}{A task's preproc call.}

\item{\code{data_call}}{A task's data call.}

\item{\code{fit_call}}{A task's fit call.}

\item{\code{pred_call}}{A task's prediction call.}

\item{\code{has_test_labels}}{Wheather the dataset has test labels.}

\item{\code{result}}{A task result.}

\item{\code{done}}{Wheather the task is finished.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Task$new()}}
\item \href{#method-print}{\code{Task$print()}}
\item \href{#method-print_task}{\code{Task$print_task()}}
\item \href{#method-eval_call_args}{\code{Task$eval_call_args()}}
\item \href{#method-clone}{\code{Task$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Construct a new \code{Task} class object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Task$new(options = NULL, keys = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{options}}{A \link{Options} class object.}

\item{\code{keys}}{A named character vector of keys. Names must be "formula",
"dataset", "resample", "seed", "model", "preproc", "fit_param" and
values must be key of the each options.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Task} class object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Print object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Task$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print_task"></a>}}
\if{latex}{\out{\hypertarget{method-print_task}{}}}
\subsection{Method \code{print_task()}}{
Print task object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Task$print_task(title = " Task ", key_width = 14L)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{title}}{A character scalar of printed title.}

\item{\code{key_width}}{A integer scalar of key column width.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-eval_call_args"></a>}}
\if{latex}{\out{\hypertarget{method-eval_call_args}{}}}
\subsection{Method \code{eval_call_args()}}{
Evaluate call arguments in the task environment.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Task$eval_call_args(call = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{call}}{\code{call} object.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{call} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Task$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
