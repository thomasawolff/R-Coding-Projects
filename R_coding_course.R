
# x <- 12
# 
# if (x==10){
#   print('x is equal to 10')
# }else if(x==12){
#   print('x is equal to 12')
# }else{
#   print('x was not equal to 10 or 12')
# }

# x <- 0
# 
# while (x < 10){
#   print(paste0('x is: ',x))
#   
#   x <- x+1
#   if(x==10){
#     print('x is now equal to 10! Break loop!')
#     break
#     print('Woo I printed too!')
#   }
# }

# mat <- matrix(1:25,nrow=5)
# 
# print(mat)
# for (num in mat){
#   print(paste('And the number is',num))
# }
# 
# for (row in 1:nrow(mat)){
#   for (col in 1:ncol(mat)){
#     print(paste('The element at row:',row,'and col:',col,'is',mat[row,col]))
#     
#   }
# }

# Recursive function to find factorial
# Factorial <- function(N)
# {
#   if (N == 0)
#     return(1)
#   else
#     return( N * Factorial (N-1))
# }
# 
# Factorial(5)

# addrand <- function(x){
#   ran <- sample(1:100,1)
#   return(x+ran)
# }
# 
# #print(addrand(10))
# 
# result0 <- lapply(v,addrand)
# result1 <- sapply(v,addrand)
# print(result0)
# print(result1)

v <- 1:10

# result <- sapply(v,function(num){num*2})
# 
# print (result)

add_choice <- function(num,choice){
  return(num+choice)
}

print(sapply(v,add_choice,choice=100))



