## sieve of sundaram excludes non-primes from odd numbers where
## n != 2(i + j + 2ij) + 1 or n != (((2*i)+1)*((2*j)+1))

function sundaram(candidate)        # approximates a sundaram sieve to detect
  if(candidate==2)                  # primes. Returns boolean for candidate is 
    return(true)                    # Prime.
  end                               # Shortcuts for low-value primes,
  if(candidate == 1)                # zero, non-primes, and even number inputs.
   return(true)                     # Returns true for candidate is prime.
  end
  if(candidate < 1)
    return(false)
  end
  if(iseven(candidate))
    return(false)
  end
#  for i in [3 , 5, 7, 11, 13, 19, 23]
  quickCheck=43
  for i in [quickCheck:-2:3]        # Non-sundaram shortcut via trial-by-
    if ( i >= (candidate/2))        # division. Break to next on attempt to
      continue                      # evaluate via division for small numbers.
    end
    if (mod(candidate, i) == 0)     # Use modulus != 0 for evaluation by
       return(false)                # division.
    end
  end                               
  limit = trunc(sqrt(candidate)+1)  # values such that i=j and 
                                    # (2i)+1 > sqrt(candidate) are not useful
                                    # to check.
                                    # Do not evaluate via sundaram using values
  for i in limit:1                  # for i and j that exceed the value of the
    if (2*i >= (candidate-1))       # candidate input. 
      continue                      # Values > i=j=3 check candidate 
    end                             # values over 49. Trial-by-division has 
    for ( j in i:1 )                # eliminated all of the values to 102. Start 
      result =(((2*i)+1)*((2*j)+1)) # at i==3 so as to overlap a little, while 
      if (result > candidate)       # eliminating some cycles. Quit trying when 
        over = true                 # sieve exceeds value of candidate. Set flag 
        continue                    # Then break from loop.
      end                           # 
      if(over & result < candidate) # If evaluating high to low, and value has
        return(true)                # once been over, and has never been
      end                           # equivalent, then candidate is sieved.
      if (candidate == result)      # Else Sundaram-style elimination.
        return(false)
      end
    end
  end
  return(true)                      # Candidate must be prime.
end 

function testSundaram()             # Test to output the first n primes
  n = 500
  primecount = 0
  printcount = 0
  x = [2, 3:2:n]                   # a candidate array of 2, followed by odd 
  tic()                            # numbers 3...n. Then start the stopwatch...
  for a in 1:length(x)
    if sundaram(x[a])
      primecount= primecount+1     # count found primes for stats
      if(x[a]<1000)                # Pretty print for low primes
        print(" ")                 #
      end                          #
      if(x[a]<100)                 #
        print(" ")                 #
      end                          #
      if(x[a]<10)                  #
        print(" ")                 #
      end                          #
      print(x[a], ", ")            # Print a prime
      printcount = printcount+1    # how many have we printed.
      if mod(printcount,12) == 0   # line break for more pretty print
        println(" ")               #
        printcount = 0             # 0 after line break
      end
    end
  end
  toc()                            # Stop the stopwatch and print the stats.
  println("\n", primecount, " primes found in range 2 to ", n )
end

function timeSundaram()            # How long to find next prime after n.
  n=99999993
  println("trying ", n)
  tic()                            # Start the stopwatch
  while !(sundaram(n))
    n=n+2
    println("trying ", n)
  end
  toc()                            # Stop the stopwatch
  println("found ", n)             # Print result
end
