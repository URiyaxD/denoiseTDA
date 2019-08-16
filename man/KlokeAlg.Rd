\name{KlokeAlg}
\alias{KlokeAlg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Shifts points by Using Kloke's Algorithm
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The de-noising algorithm. By algorithm 

\deqn{S_{n+1}=\Big\{p+c\frac{\nabla F_n(p)}{M}|p\in S_n\Big\}}{% 
              S_(n+1)= { p+c ( \grad F_n(p))/M | p \in S_n }}

Where

\deqn{F_n(x)=\frac{1}{|D|}\sum_{p\in D}e^\frac{-||x-p||^2}{2\sigma^2}-\frac{\omega}{|S_n|}\sum_{p\in S_n}e^\frac{-||x-p||^2}{2\sigma^2}}{%
              1/(|D|) sum_(p \in D) exp((-||x-p||^2)/(2 \sigma^2)) - \omega/(S_n|) sum_(p \in S_n) exp((-||x-p||^2)/(2\sigma^2))}
}
\usage{
KlokeAlg(sample, set, sigma, omega, iter, step)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{
%%     ~~Describe \code{sample} here~~
Select a sample from the set, which you want to de-noise
}
  \item{set}{
%%     ~~Describe \code{set} here~~
Select the set you want to denoise
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
this variable shows how tightly you want to denoise. A high value will result to shift more points to the same point. A very low value will result in insignificant shifting *** need to edit that 
}
  \item{omega}{
%%     ~~Describe \code{omega} here~~
Low value of omega for not that noisy data (<0.2).  And higher value of omega for highly noisy data (>0.2). 
}
  \item{iter}{
%%     ~~Describe \code{iter} here~~
Number of iterations.
}
  \item{step}{
%%     ~~Describe \code{step} here~~
The step value, basically a percent of the maximum shift at first iteration. Too low value will require a larger iteration number (<0.6). Too high value might result in overshooting(>0.9). However high values are more usefull for really noisy data(>0.9). And a lower values are useful for less noisy data(<0.7). 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (sample, set, sigma, omega, iter, step) 
{
    .Call(`_denoiseTDA_KlokeAlg`, sample, set, sigma, omega, 
        iter, step)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
