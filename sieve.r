## sieve of sundaram excludes non-primes from odd numbers where
## n != 2(i + j + 2ij) + 1 or n != (((2*i)+1)*((2*j)+1))

modulus <- function(numerator, denominator){ 
  if((numerator==0)|(denominator==0)) {
    stop("Error: divide of or by zero")
  }
  if((numerator<denominator)) {
    stop("Error: denominator larger than numerator")
  }
  return(numerator-(trunc(numerator/denominator)*denominator))
}

is.even <- function(testforeven){
  if(testforeven==1) return(FALSE)
  if(modulus(testforeven,2)==0) return(TRUE)
  return(FALSE)
}

sundaram <- function(candidate) { ##determines if input value candidate is
                                  ## prime. Returns Boolean.
  if(candidate==2|candidate==1) return(TRUE)   # Shortcuts for 1 and 2
  if(is.even(candidate)) return(FALSE)         # Shortcut for even numbers
  quickCheck <- 51L
  for(checkMod in seq(from=quickCheck,by=-2, to=3)){
#  for(checkMod in seq(from=3,                  # Trial-by-division short-cut
#                      to=quickSieve,           # Quickly sieves non-prime
#                      by=2)){                  # candidates before applying 
    if(checkMod>=candidate) next              # sundaram algorithm. 
    if(modulus(candidate,checkMod)==0){        # Discards candidate modulus
      return(FALSE)                            # checkMod == 0.
    }
  }
  limit <- trunc(sqrt(candidate)+1)         ## Division by greater than sqrt 
                                            ## of the candidate value results  
                                            ## in wasted operations.
  for( i in limit:1 ){                      ## Count down from limit for i.
    if ((i-1)^2>=(candidate-1)) {           ## Skip over values of i that are
      next  ## i                            ## too large to matter.
    }                                       ##
    for (j in i:1){                         ## Count down from i for j.
      result <-(((2*i)+1)*((2*j)+1))        ## Store sieve
      if (result > candidate) {             ## Skip sieve values too big to
        over<-TRUE                          ## matter.  First tries should be
        next ## j                           ## larger than candidate. 
      }
      if (over & result < candidate) {      ## If a subsequent try is under
       return(TRUE)                         ## without being equal then seive  
      }                                     ## works  
      if (candidate == result){             ## Apply sieve and eliminate 
        return(FALSE)                       ## values.
      }
    }
  }
  return(TRUE)
}

testSundaram <-function(){
  n <- 500
  oddArray <-c(2, seq(from=3L, ## oddArray is a sequence of 2
                      to=n, ## to n odd numbers
                      by=2L))  ## 
  p_list <- NULL
  print(system.time(  
  for(a in 1:length(oddArray)){
    if(sundaram(oddArray[a])) 
      {
      p_list[length(p_list)+1]<-oddArray[a]
      }
   }
  ))
  return(p_list)
}

timeSundaram <- function() { #find the next prime after n.
  n <- 999999993
  print(paste("trying ", n))
  runTime <-system.time(
    while(sundaram(n)==FALSE) {
      n=n+2
      print(paste("trying ", n))
    }
  )
  print(runTime)
  print(paste("Found ", n))
}