
#add a new column to the dataset having values of 1 initialy
DataF[,5] = 1
#Change the values in the new column according to type of species
#setosa=1
#versicolor=-1
DataF[DataF[,4] == "versicolor",5] = -1
#Make table x which has 2 vaiables "sepal and petal"
#table x represents the 2 inputs tp the perceptron
x= DataF[, c(2,3)]
#y is the output which is either 1"setosa" or -1 "versicolor"
y= DataF[,5]

#the perceptron training function
perceptron = function(x,y,rate){
  
  #first, initializa a vector of two weights for the two inputs
  #give each weight a random value = 0.5
  w = rep(0.5,2)
  
  #initialize the error, initially the error of all inputs is 0
  error = rep(0,100)
  #counter to keep track of the number of iterations taken to get the
  #correct weights
  iterations =0
  
  #the first loop that loops over the 100 instances
  for(i in 1:100){
    
    #second loop that loops over the output
    for(j in 1:length(y)){
      
      #calculating the sum of the weights to get the output value
      #total is the output value
      total = sum( w[1:2] * as.numeric(x[j,]))
      
      #checks if sum is greater than threshold or not
      if(total > 0){
        #if total is > 0 then output is a setosa
        output = 1
      }else{
        #if total is <0 then output is a versicolor
        output = -1
      }
      
      #y[j] is the actual value for the current inputs,whether it's actually
      #a setosa or versicolor
      # if output is not the same as the target
      if(y[j]- output != 0.0){
        
        #update the weight
        w = w[1:2] - (rate* (output - y[j])* x[j,])
        #increment the iterations taken to calculate the correct weights
        iterations = iterations+1
       
       # error[i] =error[i] +1
       # error[i] = 1
      }
      #the error function is y-t --> output - target
      
      
      
      
    }
    #the error function is y-t --> output - target
    error[i]= output -y[i]
    
  }
  #print the updated weights
  print(w)
  #print the number of iterations taken to update the weight that give the 
  #correct classification
  print(iterations)
  #Print(error)
  
  #return the error that saves the results of the error function.
  return(error)
  
}
#run the training function of the perceptron giving it the inputs x, output y
#and the learning rate which can be modified to different values.
run= perceptron(x,y,0.2)


#Function that implements the recall/test mode to classify certain instances
#the following function takes as parameters: the inputs vector a,
#the output vector y, the learning rate r, the tested instances s and p.
#"which in this case are the sepal and petal lengths"
RecallMode = function(a,b,r,s,p){
  
  #first, it calls the perceptron using the taken inputs
  per = perceptron(a,b,0.2)

 
 
  #it calculates the output sum.
  result = ((per[1]*as.numeric(s)) + (per[2]*as.numeric(p)))
  outcome = result
  
  # #checks if result is greater than threshold or not and decides the output.
  if(result > 0){
    result = 1
  }else{
    result= -1
  }
  print(outcome)
  print(result)
  print(paste0("Species: " , result))
}

#Testing using the recall function to classify the given inputs.
test = RecallMode(x,y,0.2,5.1,3)



#plots the error against the index of training index
plot(1:100, run, type="l", lwd=1, col="red", xlab="instances", ylab="error")
#plot(1:100,run,main="error")

#plot iris class variable against the petal length
plot(V5,petal,main="Iris class variable Vs petal length")

#using the built-in functions in R, calculates the covariance and correlation
#between iris class and petal length
#covariance = cov(V5,petal)
#print(paste0("Covariance = ",covariance))
#correlation =cor(V5,petal)
#print(paste0("Correlation = ",correlation))


covariance = cov(as.numeric(V5),as.numeric(petal))
print(paste0("Covariance = ",covariance))
correlation =cor(as.numeric(V5),as.numeric(petal))
print(paste0("Correlation = ",correlation))


#plot iris class variable against the sepal length
plot(V5,sepal,main="Iris class variable Vs sepal length")

#using the built-in functions in R, calculates the covariance and correlation
#between iris class and sepal length
#covariance = cov(V5,sepal)
#print(paste0("Covariance = ",covariance))
#correlation =cor(V5,sepal)
#print(paste0("Correlation = ",correlation))

covariance = cov(as.numeric(V5),as.numeric(sepal))
print(paste0("Covariance = ",covariance))

correlation =cor(as.numeric(V5),as.numeric(sepal))
print(paste0("Correlation = ",correlation))

