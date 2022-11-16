# A naive plus fuction for strings and numbers

"+" = function (x,y){

   if (is.character(x)){
      return( paste(x,y)  )
   }
   else if (is.numeric(x)){
     return( base::"+"(x,y) )
   }
   else{
     warning("Unrecognized inputs: NA returend.")
     return(NA)
   }   
}