lorenz = function (n=5000) {  
  a=10; 
  r=28; 
  b=8/3; 
  dt=0.01
  X=0.01;
  Y=0.01; 
  Z=0.04; 
  t=0
  XYZ = array(0,dim=c(n,3))
  for(i in 1:n)  { 
	X1=X;
	Y1=Y; 
	Z1=Z
	X=X1+(-a*X1+a*Y1)*dt
	Y=Y1+(-X1*Z1+r*X1-Y1)*dt
	Z=Z1+(X1*Y1-b*Z1)*dt
	XYZ[i,]=c(X,Y,Z) 
  }
  plot(XYZ[,1],XYZ[,3],'type'='l');
}	