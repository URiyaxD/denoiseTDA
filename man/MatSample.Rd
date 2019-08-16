\name{MatSample}
\alias{MatSample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Salect a sample from matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Selects sample of length `N` from the matrix `Mat`. Keeping the demension. 
}
\usage{
MatSample(Mat, N = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mat}{
%%     ~~Describe \code{Mat} here~~
Your any matrix, any dimension. Dimension is a number of columns in the matrix.
}
  \item{N}{
%%     ~~Describe \code{N} here~~
A length of the sample (number of rows) you want to select.
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

Returned another matrix with same dimension and row length = `N`.
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
function (Mat, N = 100) 
{
    ncol <- length(Mat[, 1])
    nrow <- length(Mat[1, ])
    sam <- sample(1:nrow, N)
    selected <- matrix(Mat[sam[1], ], ncol = ncol)
    for (i in 2:N) {
        selected <- rbind(selected, matrix(Mat[sam[i], ], ncol = ncol))
    }
    return(selected)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
