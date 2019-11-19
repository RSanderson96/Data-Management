#How many Questions are answered correctly?


#Questions file used as the most visible assessment of attainment: are the students improving?

Q = unique(cyber.security.1.question.response$quiz_question,incomparables = FALSE)
Questions.function = function(x){ #x = file selected to analyse
  
  Questions = x
  Q= unique(Questions$quiz_question, incomparables = FALSE) #list the unique questions
  L =length(Q) #How many questions are there?
  
  Response_Correct = vector() #making the vestor
  for(i in 1:L){ #for loop: L = how many questions will be assessed/length of vector
    Asked = Questions %>% filter (quiz_question == Q[i])
    True = Asked %>% filter (correct == "true")
    T = nrow (True)
    S = nrow (Asked)
  Response_Correct[i] = (T/S)}
  return(Response_Correct)
  }

Correct1 = Questions.function(cyber.security.1.question.response)
Correct2 = Questions.function(cyber.security.2.question.response)
Correct3 = Questions.function(cyber.security.3.question.response)
Correct4 = Questions.function(cyber.security.4.question.response)
Correct5 = Questions.function(cyber.security.5.question.response)
Correct6 = Questions.function(cyber.security.6.question.response)
Correct7 = Questions.function(cyber.security.7.question.response)
Average = (Correct1+Correct2+Correct3+Correct4+Correct5+Correct6+Correct7)/7


CorrectDF = data.frame(Question = Q, Average = Average,
                       Course1 = Correct1, Course2 = Correct2, Course3 = Correct3, Course4 = Correct4, Course5 = Correct5, Course6=Correct6, Course7=Correct7)
cache("CorrectDF")

Correct = ggplot(data=CorrectDF, x=Question, Y = Correct_Answers)
C1 = Correct + geom_point(aes(x=Question, y=Course1, colour = "Course1"))
C2 = C1 + geom_point(aes(x=Question, y=Course2, colour = "Course2"))
C3 = C2 + geom_point(aes(x=Question, y=Course3, colour = "Course3"))
C4 = C3 + geom_point(aes(x=Question, y=Course4, colour = "Course4"))
C5 = C4 + geom_point(aes(x=Question, y=Course5, colour = "Course5"))
C6 = C5 + geom_point(aes(x=Question, y=Course6, colour = "Course6"))
C7 = C6 + geom_point(aes(x=Question, y=Course7, colour = "Course7"))



Correct = within(CorrectDF, rm(Question, Average))
CorrectVec = as.vector(as.matrix(Correct))
Course = rep(1:7, each = 22)
BoxDF = data.frame(Course = Course, Score = CorrectVec)
Box = ggplot(data = BoxDF, x = Course, y = Score)
B1 = Box + geom_boxplot(aes(group = Course, x = Course, y = Score))



boxplot(CorrectDF$Course1, CorrectDF$Course2, CorrectDF$Course3, CorrectDF$Course4, CorrectDF$Course5, CorrectDF$Course6, CorrectDF$Course7,
        main = "Comparison of the results for each course",
        names = c("Course 1", "Course 2", "Course3", "Course 4", "Course 5", "Course 6", "Course 7"),
        ylab = "% Correct Responses",
        xlab = "Course Run")


        

       

