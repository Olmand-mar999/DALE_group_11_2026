## Exercise 1.1:

a=2*(5+4)
b=5+90/(3*10)
c=14+6-(2+11)
d=21/(7-(3+4))

print("Using parantheses below should return: 18,8,7,Inf")
print(a)
print(b)
print(c)
print(d)

## Exercise 1.2:
# Create a vector representing ages as integers from 15 to 50 using seq")
# Should return 15,16, ..50"
print(agevector)

# Create a vector representing income as integers starting at 10000"
# with 5000 as step. It must have the same length as agevector"
# Should return 10000,15000,..,185000"
print(income)

#Create a scatterplot of age related to income"
qplot(agevector,income)

## Exercise 1.3:
# Create a function that returns the salary given the age of the player
age_to_salary <- function( age ) {
  result <- age
  return(result)
}

print("below should return: 10000,50000,125000")
age_to_salary(15)
age_to_salary(23)
age_to_salary(38)


## Exercise 1.4:
# Fill out the logic of the function below converting
# Celcius to Fahrenheit.

convert_temp_CtoF <- function( something ) {
  result <- something + something
  return(result)
}

print("below should return: 32,50,99,212")
convert_temp_CtoF(0)
convert_temp_CtoF(10)
convert_temp_CtoF(37)
convert_temp_CtoF(100)

## Exercise 1.5:
# Fill out the logic of the function below converting
# inches to centimeters

convert_in_to_cm <- function( inch ) {
  result <- inch
  return(result)
}

print("below should return: 0,2.54,12.7,25.4")
convert_in_to_cm(0)
convert_in_to_cm(1)
convert_in_to_cm(5)
convert_in_to_cm(10)


## Exercise 1.6:
# Fill out the logic of the function below converting
# centimeters to inches

convert_cm_to_in <- function( cm ) {
  result <- cm
  return(result)
}

print("below should return: 0.39,11.81")
convert_cm_to_in(1)
convert_cm_to_in(30)

