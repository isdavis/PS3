
#This creates the attributes for a name, creating a student
rec<-function(name) {
  courage<-sample(1:100, size=1)
  effort<-sample(1:100, size=1)
  intelligence<-sample(1:100, size=1)
  ambition<-sample(1:100, size=1)
  structure(list(name, courage, effort, intelligence, ambition), class="student")
}

Ian<-rec("Ian")
Ian

#this initializes the sort function by calling the sort method that already exists
sort.student<-function(x) {
  UseMethod(sort)
}

#sorts 4x4 identity matrix, to make for a simple weight
sortMatrix<-diag(4)

Gryffindor<-setClass("Gryffindor")
Hufflepuff<-setClass("Hufflepuff")
Ravenclaw<-setClass("Ravenclaw")
Slytherin<-setClass("Slytherin")

#the sort function; it prints what the sorting hat yells and also assigns a new class (the house) to a student 
sort.student<-function(x,matrix) {
  att<-c(x[[2]], x[[3]], x[[4]], x[[5]])
  y<-t(matrix)%*%(att)
  house<-""
  vec<-NULL
  for(i in 1:4) {
    vec[i]<-y[i,1]
  }
  if (max(vec)==vec[1]) { 
    house<-"GRYFFINDOR!"
    class(x)<-"Gryffindor"
    class(x)<-append(class(x),"Student")}
  if (max(vec)==vec[2]) { 
    house<-"HUFFLEPUFF!"
    class(x)<-"Hufflepuff"
    class(x)<-append(class(x),"Student")}
  if (max(vec)==vec[3]) { 
    house<-"RAVENCLAW!"
    class(x)<-"Ravenclaw"
    class(x)<-append(class(x),"Student")}
  if (max(vec)==vec[4]) { 
    house<-"SLYTHERIN!"
    class(x)<-"Slytherin"
    class(x)<-append(class(x),"Student")}
  
  print(house)
   return(x)
  
}

#Ian is now sorted, and has a new class
IanSorted<-sort.student(Ian, sortMatrix)
is.environment(IanSorted)
#New environments
Gryffindor_Tower<-new.env()
Black_Lake<-new.env()
Ravenclaw_Tower<-new.env()
Basement<-new.env()




setGeneric("curfew", function(object="Student"){
  standardGeneric("curfew")
})
setMethod("curfew", "Gryffindor", function(object){
  Gryffindor_Tower$a<-object
})
setMethod("curfew", "Ravenclaw", function(object){
  Ravenclaw_Tower$a<-object
})
setMethod("curfew", "Hufflepuff", function(object){
  Black_Lake$a<-object
})
setMethod("curfew", "Slytherin", function(object){
  Basement$a<-object
})

curfew(IanSorted)
ls(Gryffindor_Tower)
ls(Ravenclaw_Tower)
ls(Black_Lake)
ls(Basement)


Gryffindor_Tower$a
Ravenclaw_Tower$a
Black_Lake$a
Basement$a
