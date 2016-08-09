require(fields)
N<- 5e3
for( N in c( 1:5)*1e3){
  x<- matrix( runif( N*2), N,2)
  temp <- exp( -rdist( x,x))
  print(system.time( obj<- chol( temp)))
}

# user  system elapsed 
# 0.145   0.002   0.149 
# user  system elapsed 
# 1.129   0.014   1.158 
# user  system elapsed 
# 3.903   0.024   3.933 
# user  system elapsed 
# 8.995   0.047   9.058 
# user  system elapsed 
# 16.531   0.063  16.598 

# user  system elapsed 
# 0.026   0.009   0.018 
# user  system elapsed 
# 0.192   0.018   0.060 
# user  system elapsed 
# 0.631   0.034   0.184 
# user  system elapsed 
# 1.438   0.062   0.355 
# user  system elapsed 
# 2.459   0.090   0.682 