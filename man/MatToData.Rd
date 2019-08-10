\name{MatToData}
\alias{MatToData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Transforms Matrix to Data, with names 1 column = x, 2nd column=y, and 3rd column =z
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MatToData(Mat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mat}{
%%     ~~Describe \code{Mat} here~~
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
function (Mat) 
{
    if (length(Mat[1, ]) == 2) {
        Data <- data.frame(x = Mat[, 1], y = Mat[, 2])
    }
    if (length(Mat[1, ]) == 1) {
        Data <- data.frame(x = Mat[, 1])
    }
    if (length(Mat[1, ]) == 3) {
        Data <- data.frame(x = Mat[, 1], y = Mat[, 2], z = Mat[, 
            3])
    }
    return(Data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
