# Coded by Rebecca Ralston (ralston.119)

mymax = function(x) {
  maxValueSoFar = x[1]
  for (value in x) {
    if (value > maxValueSoFar) {
      maxValueSoFar = value
	}
  }
  return(maxValueSoFar)
}


# Coded by Grant Ravary
add3 = function (x,y,z ) { 

  #self explanitory
  total=x+y+z
  return(total) 
  
}       

# Solve the qudratic equtions given three coefficents
quad <- function(a, b, c) {

  dicrimant = b^2 - (4 * a *c)
  
  if (dicrimant < 0) {
    warning("There are no real roots")
    root =NA
  }
  else if (dicrimant > 0) {
    pos = (-b + sqrt(dicrimant))/(2*a)
    neg = (-b - sqrt(dicrimant))/(2*a)
    root = c(pos, neg);
  } 
  else {
    root = -b / (2*a)	
  } 
  
  return(root)
  
}