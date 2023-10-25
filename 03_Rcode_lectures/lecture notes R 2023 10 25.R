##Vectors

object_01 <- c(1,2,3,4,5,6)

is.vector(object_01) #asking whether it is a vector

length(object_01) #asking about how many elements

object_02 <- c(1,7,2,6,7,9,2)

object_02[2]

object_02[-2]

#A double or a numeric

typeof(object_01)
typeof(object_02)

is.numeric(object_01)
is.double(object_01)

##Integers

is.numeric(3)
is.integer(3)

is.integer(3L)
3L
1.5L

object_integers_02 <- c(1L, 3L, 5L)

typeof(object_integers_02)

sqrt(2)^2-2

##Character

object_chr_03 <- c("Please", "write", "down", "the", "answer", "now", "everyone")

object_chr_03[2]

"the" %in% object_chr_03

sum(object_chr_03)


object_chr_04 <- c("hello", 3)

object_chr_04

object_logical_05 <- c(TRUE, FALSE, TRUE)

typeof(object_logical_05)

object_06 <- c(1,3,6,7,8, "unknown")

sum(object_06)
typeof(object_06)
object_06

match("unknown", object_06)

object_06a <- object_06[-6]


sum(object_06a)

sum(as.numeric(object_06a))

sum(as.numeric(object_06[c()]))

object_numbers <- c(10,4,6,7,8,9,10, 7)

mean(object_numbers)

sum(object_numbers)

2*object_numbers

object_numbers_02 <- c(2,3)

object_numbers * object_numbers_02

## Reading in a file

practice_data <- read.csv("/Users/Adan Tallman/Desktop/practice.csv")
practice_data <- read.table("/Users/Adan Tallman/Desktop/practice.csv", sep=",", header=TRUE)
practice_data$Feeling*practice_data$Confused

plot(practice_data$Feeling~practice_data$Confused)
