% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FitParamSpecs.R
\name{FitParamSpecs}
\alias{FitParamSpecs}
\alias{new_fit_param_specs}
\title{FitParamSpecs Class}
\usage{
new_fit_param_specs(...)
}
\arguments{
\item{...}{Key and value pairs.}
}
\value{
A new \code{FitParamSpecs} class object.
}
\description{
Key-value dictionary contains \link{FitParamSpec} class objects.
This class is passed to \link{Model} class and defines parameters of
model's fit functon.
}
\section{Super classes}{
\code{\link[Dict:Dict]{Dict::Dict}} -> \code{\link[ml4e:Option]{ml4e::Option}} -> \code{FitParamSpecs}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{defaults}}{A list of default values}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{FitParamSpecs$new()}}
\item \href{#method-add}{\code{FitParamSpecs$add()}}
\item \href{#method-print}{\code{FitParamSpecs$print()}}
\item \href{#method-get_grid}{\code{FitParamSpecs$get_grid()}}
\item \href{#method-get_random}{\code{FitParamSpecs$get_random()}}
\item \href{#method-get_bayes_info}{\code{FitParamSpecs$get_bayes_info()}}
\item \href{#method-clone}{\code{FitParamSpecs$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="Dict" data-topic="Dict" data-id="get">}\href{../../Dict/html/Dict.html#method-get}{\code{Dict::Dict$get()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="Dict" data-topic="Dict" data-id="has">}\href{../../Dict/html/Dict.html#method-has}{\code{Dict::Dict$has()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="Dict" data-topic="Dict" data-id="sort">}\href{../../Dict/html/Dict.html#method-sort}{\code{Dict::Dict$sort()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ml4e" data-topic="Option" data-id="clear">}\href{../../ml4e/html/Option.html#method-clear}{\code{ml4e::Option$clear()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ml4e" data-topic="Option" data-id="print_items">}\href{../../ml4e/html/Option.html#method-print_items}{\code{ml4e::Option$print_items()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ml4e" data-topic="Option" data-id="remove">}\href{../../ml4e/html/Option.html#method-remove}{\code{ml4e::Option$remove()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Construct a new \code{FitParamSpecs} class object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FitParamSpecs$new(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Key and value pairs.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{FitParamSpecs} class object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add"></a>}}
\if{latex}{\out{\hypertarget{method-add}{}}}
\subsection{Method \code{add()}}{
Adds key-value objects to the dictionary.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FitParamSpecs$add(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Key and value pairs.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Print object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FitParamSpecs$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_grid"></a>}}
\if{latex}{\out{\hypertarget{method-get_grid}{}}}
\subsection{Method \code{get_grid()}}{
Get paramter grid.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FitParamSpecs$get_grid()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \code{data.frame} of parameter grid.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_random"></a>}}
\if{latex}{\out{\hypertarget{method-get_random}{}}}
\subsection{Method \code{get_random()}}{
Get random paramter grid.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FitParamSpecs$get_random(size = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{size}}{A integer scalar of parameter size.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{data.frame} of parameter grid.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_bayes_info"></a>}}
\if{latex}{\out{\hypertarget{method-get_bayes_info}{}}}
\subsection{Method \code{get_bayes_info()}}{
Get information for BayesianSearch.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FitParamSpecs$get_bayes_info()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \code{list} of parameter infomation.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FitParamSpecs$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
