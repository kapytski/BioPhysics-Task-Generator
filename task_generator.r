source("Answers_rus.r",encoding = "utf-8")
source("Random.numbers.generator.r",encoding = "utf-8")
source("Tasks_rus.r",encoding = "utf-8")
condition<-function(task.index){
  # task.index=4
  task.text<-tasks[[task.index]]
  task.numbers<-r.numbers(task.index)
  # task.numbers<-c("i"=4,"j"=7)
  foo<-function(i,task.text){sub(paste0("##slot_",i,"##"),task.numbers[i],task.text)}
  res<-foo(1,task.text)
  if(length(task.numbers)>1){
    for(j in 2:length(task.numbers)) res<-foo(j,res)
  }
  answer<-answers(task.index,task.numbers)
  if(is.null(answer)) return(NULL)
  comment<-paste0("// Task of class ",task.index)
  res<-paste0(collapse="\n",c(comment,res,answer))
  res<-paste(res,"\n\n\n")
  res
}

generate.it<-function(n){
  
  for(i in 1:length(tasks)){
    res<-""
    print(paste0("i=",i,"-----------------"))
      for(j in 1:n){
        temp<-condition(i)
        while(is.null(temp)) temp<-condition(i)
        res<-paste0(res,"\n",condition(i))
      }
    cat(res,file=paste0("task_rus",i,".txt"))
  }
 
}

generate.it(200)