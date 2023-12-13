#Aubrey Clayton Ch.1
##Jaynes attributes main idea to R.T. Cox and George Polya
##Jaynes based on Cox's consistency theorem
##Qualitative considerations, Ch.1
##"Plausible reasoning": reasoning with incomplete information

##imagine 20 questions game to figure something out
##this is based on plausible reasoning

##Premise: If P is true, then Q is true
##Observation: Q is true | Q is plausible
##Conclusion: P is "more plausible"
##But Jaynes points out that this is dependent on background information
##Whether it makes it more plausible has more to do with other hypotheses that are available

##A Ā

##A + B
A <- c(T,T,F,F)
B <- c(T,F,T,F)

AplusB <- A | B
AplusB
##AB
AB <- A & B
AB

##Jaynes shows that you can do everything with and, or, not
##How do we show this?
##Imagine you had some mystery function, 
##there's only 16 possibilities, you can derive all of them from and, or, not
##Write a function that gives you  F, T, T, F
mystery_function <- function(a,b){
  a != b
}

mystery_function(A,B)

##What are the qualitative considerations that we want the plausibility robot to satisfy?
#Desiderata
##I) Plausibility is a real number, greater plausibility corresponds to larger number
##A|B plausibility of A given B
##II) plausibility agrees with common sense
##write down, needs to be revised
##III) Reason consistently
##a) if we reach conclusion in more than one way
##we should arrive at the same conclusion
##b) we should use all the information available
##c) equivalent states of knowledge should give us same plausibility number
##next chapter we get to the principle of indifference
##IIIc is the main thing that kick starts assigning numbers to plausibilities

##Mind-projection fallacy
##confusion ontological statements with epistemological statements
##X is Y vs. X is by nature Y confusion
##Are probabilities ontological or epistemological things













