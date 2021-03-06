% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ResampleCalls.R
\name{ResampleCalls}
\alias{ResampleCalls}
\alias{new_resample_calls}
\title{ResampleCalls Class}
\usage{
new_resample_calls(...)
}
\arguments{
\item{...}{Key and value pairs.}
}
\value{
A new \code{ResampleCalls} class object.
}
\description{
Key-value dictionary contains \code{call} class objects. The calls must be calls
that generate \code{rset} class object from \link[rsample:rsample]{rsample::rsample} package.
This class is usually passed to \link{Datasets} class then used to generate
resamples from datasets.
}
\section{Super classes}{
\code{\link[Dict:Dict]{Dict::Dict}} -> \code{\link[ml4e:Option]{ml4e::Option}} -> \code{ResampleCalls}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{ResampleCalls$new()}}
\item \href{#method-print}{\code{ResampleCalls$print()}}
\item \href{#method-get_resample}{\code{ResampleCalls$get_resample()}}
\item \href{#method-clone}{\code{ResampleCalls$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="Dict" data-topic="Dict" data-id="get">}\href{../../Dict/html/Dict.html#method-get}{\code{Dict::Dict$get()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="Dict" data-topic="Dict" data-id="has">}\href{../../Dict/html/Dict.html#method-has}{\code{Dict::Dict$has()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="Dict" data-topic="Dict" data-id="sort">}\href{../../Dict/html/Dict.html#method-sort}{\code{Dict::Dict$sort()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ml4e" data-topic="Option" data-id="add">}\href{../../ml4e/html/Option.html#method-add}{\code{ml4e::Option$add()}}\out{</span>}
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
Construct a new \code{ResampleCalls} class object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ResampleCalls$new(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Key and value pairs.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{ResampleCalls} class object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Print object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ResampleCalls$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_resample"></a>}}
\if{latex}{\out{\hypertarget{method-get_resample}{}}}
\subsection{Method \code{get_resample()}}{
Get resample data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ResampleCalls$get_resample(data = NULL, resample_call_key = NULL, seed = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{A \code{data.frame} to be converted to a resample.}

\item{\code{resample_call_key}}{A character or integer scalar of the
resample_calls keys.}

\item{\code{seed}}{A integer scalar seed.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A resample of \code{rset} class object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ResampleCalls$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
