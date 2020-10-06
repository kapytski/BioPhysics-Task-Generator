format.it<-function(x,digits){
  # x=right.answer
  # digits=2
  x<-format(x,scientific = T)
  x<-unlist(strsplit(x,"e"))
  x<-as.numeric(x)
  x[1]<-round(x[1],digits)
  x[2]<-round(x[2])
  paste0(x[1],"\\cdot 10^\\{",x[2],"\\}")
}
get.bad.anwers<-function(x){
  n<-floor(log10(abs(x)))
  mantissa<-x/10^n
  # генерируем числа из равномерного распределения от 0.1 до 15
  # из них оставляем только такие, которые отличаются от истинного более чем на 10 процентов
  foo<-function(){
    r<-runif(10,min=0.1, max=sample(10:15,1))
    r<-sort(r[which(abs(r-mantissa)/mantissa>0.15)])
    r
  }
  temp<-foo()
  while(length(temp)<6) temp<-foo()
  sort(sample(temp,6)*10^n)
}
answer_1<-function(task.numbers){
  x<-task.numbers
  right.answer<-( x['E']*10^-3 ) / ( x['t']*6*(x['a']*10^-2)^2 )
  bad.anwers<-get.bad.anwers( right.answer)
  digits=2
  right.answer<-format.it(right.answer,digits)
  bad.anwers<-sapply(bad.anwers,format.it,digits=digits)
  right.answer<-paste0("\\(",right.answer,"\\;Вт/м^2\\)")
  bad.anwers<-paste0("\\(",bad.anwers,"\\;Вт/м^2\\)")
  res1<-paste0("=",right.answer)
  res2<-paste0("~",bad.anwers)
  res<-paste0(collapse="\n",c(res1,res2))
  res<-paste("{",res,"}")
  res
}
answer_2<-function(task.numbers){
  x<-task.numbers
  right.answer<-x['alpha']*5.67*10^-8*x['T']^4*4*pi*(x['r']*10^-2)^2*1/1000
  bad.anwers<-get.bad.anwers(right.answer)
  digits=3
  right.answer<-round(right.answer,digits)
  bad.anwers<-round(bad.anwers,digits)
  right.answer<-paste0("\\(",right.answer,"\\;кДж\\)")
  bad.anwers<-paste0("\\(",bad.anwers,"\\;кДж\\)")
  res1<-paste0("=",right.answer)
  res2<-paste0("~",bad.anwers)
  res<-paste0(collapse="\n",c(res1,res2))
  res<-paste("{",res,"}")
  res
}
answer_3<-function(task.numbers){
  x<-task.numbers
  h<-6.63*10^-34;v<-2.98*10^8;k<-1.38*10^-23
  
  right.answer<-2*pi*h*v^2/((x['lambda']*10^-7)^5)*1/ ( exp( (h*v)/(x['lambda']*10^-7*k*x['T'])  ) - 1 )*x['alpha']
  
  bad.anwers<-get.bad.anwers(right.answer)
  digits=2
  right.answer<-format.it(right.answer,digits)
  bad.anwers<-sapply(bad.anwers,format.it,digits=digits)
  right.answer<-paste0("\\(",right.answer,"\\;Вт/м^3\\)")
  bad.anwers<-paste0("\\(",bad.anwers,"\\;Вт/м^3\\)")
  res1<-paste0("=",right.answer)
  res2<-paste0("~",bad.anwers)
  res<-paste0(collapse="\n",c(res1,res2))
  res<-paste("{",res,"}")
  res
}
answer_4<-function(task.numbers){
  x<-task.numbers
  right.answer<-2900*10^-6*abs(1/x['T1']-1/x['T2'])
  bad.anwers<-get.bad.anwers( right.answer)
  digits=2
  right.answer<-format.it(right.answer,digits)
  bad.anwers<-sapply(bad.anwers,format.it,digits=digits)
  right.answer<-paste0("\\(",right.answer,"\\;м\\)")
  bad.anwers<-paste0("\\(",bad.anwers,"\\;м\\)")
  res1<-paste0("=",right.answer)
  res2<-paste0("~",bad.anwers)
  res<-paste0(collapse="\n",c(res1,res2))
  res<-paste("{",res,"}")
  res
}
answer_5<-function(task.numbers){
  x<-task.numbers
  right.answer<-(x['T2']+273)^4-x['E']/(x['alpha']*x['S']*5.67*10^-8)
  if(right.answer<0) return (NULL)
  right.answer<-round(right.answer^0.25,0)-273
  foo<-function(){
    temp<-sort(sample(10:x['T2'],15))
    i<-which(abs(temp-right.answer)/right.answer<0.15)
    if(length(i)==0) return(temp)
    temp[-i]
  }
  bad.anwers<-foo()
  while(length(bad.anwers)<6) bad.anwers<-foo()
  bad.anwers<-sample(bad.anwers,6)
  
  right.answer<-paste0("\\(",right.answer,"\\;^\\circ C\\)")
  bad.anwers<-paste0("\\(",bad.anwers,"\\;^\\circ C\\)")
  res1<-paste0("=",right.answer)
  res2<-paste0("~",bad.anwers)
  res<-paste0(collapse="\n",c(res1,res2))
  res<-paste("{",res,"}")
  res
}
answer_6<-function(task.numbers){
  right.answer<-round(task.numbers['alpha_1']*task.numbers['l_2']/task.numbers['l_1'],1)
  bad.anwers<-c( seq(from=0.65,to=0.95,by=0.1 )*right.answer,
                 seq(from=1.10,to=1.30,by=0.1 )*right.answer )
  bad.anwers<-round(bad.anwers,1)
  right.answer<-paste0("\\(",right.answer,"\\)")
  bad.anwers<-paste0("\\(",bad.anwers,"\\)")
  res1<-paste0("=",right.answer)
  res2<-paste0("~",bad.anwers)
  res<-paste0(collapse="\n",c(res1,res2))
  res<-paste("{",res,"}")
  res
}

answers<-function(task.index,task.numbers){
  switch(task.index,
         "1"=answer_1(task.numbers),
         "2"=answer_2(task.numbers),
         "3"=answer_3(task.numbers),
         "4"=answer_4(task.numbers),
         "5"=answer_5(task.numbers),
         "6"=answer_6(task.numbers)
         )
}
