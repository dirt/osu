\name{mygame}
\alias{mygame}
\title{Yet another MineSweeper game}
\description{
  Lanuch a minesweeper game in R.
  }
\usage{
    mygame(ny=12, nx=25, prob=0.80)
}
\arguments{
  \item{ny}{ the rows/height of the game interface; if not provided, set to 12 by default.}  
  \item{nx}{ the cols/width of the game inferface; if not provided, set to 25 by default}
  \item{prob}{ a real number between 0 and 1 to specify the probabiliy of empety cells}
  
}
\value{
  The function has no output! 
  
}
\examples{
library(osu)
mygame(10,20,0.8) 
}

\keyword{misc}
