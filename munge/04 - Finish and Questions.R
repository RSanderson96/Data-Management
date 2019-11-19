library(ProjectTemplate)
load.project()

#1) How Many Times For Each Course Does a Learner Start a Section, But Not finish It?

#1a)How many times per learner?

Incomplete_Quantity7= cyber.security.7.step.activity %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
IncompleteDF = data.frame(Course1 = nrow(Incomplete_Quantity1),Course2 = nrow(Incomplete_Quantity2),
                          Course3 = nrow(Incomplete_Quantity3),Course4 = nrow(Incomplete_Quantity4),
                          Course5 = nrow(Incomplete_Quantity5),Course6 = nrow(Incomplete_Quantity6),
                          Course7 = nrow(Incomplete_Quantity7))


UnfinishLeaner.function = function(x,y){
  
  Incomplete_Quantity = x%>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
  #Filter: Number of non-finishes by learner

  Count_Unfinished = (merge(y, Incomplete_Quantity, by = "learner_id"))%>%filter(n != "")
  #dataframe = Question responses & counts the number of unfinished steps for each learner ID
  
  Answers= y %>% filter(correct == "true") %>% count(learner_id) %>% arrange(-n)
  #how many correct answers for each learner ID?
  Attempts =y %>% count(learner_id) %>% arrange(-n)
  #How many attempts for each learner ID?
  
  Unfinished = data.frame(learner_id = Count_Unfinished$learner_id, Unfinished = Count_Unfinished$n)
  Unfinished = unique.data.frame(Unfinished)
  
  UnfinishedQuestions = merge(Unfinished,Answers, by = "learner_id", all = TRUE)
  UnfinishedQuestions = UnfinishedQuestions%>% rename(Correct_Answers = n)
  UnfinishedQuestions = merge(UnfinishedQuestions,Attempts, by = "learner_id", all = TRUE)
  UnfinishedQuestions = UnfinishedQuestions%>% rename(Quant_Attempts = n)
  
  ProportionCorrect = (UnfinishedQuestions$Correct_Answers/UnfinishedQuestions$Quant_Attempts)*100
  
  UnfinishedQuestions = cbind(UnfinishedQuestions, ProportionCorrect)
  UnfinishedQuestions = UnfinishedQuestions[-c(1), ]
  UnfinishedQuestions[is.na(UnfinishedQuestions)] <- 0
  return(UnfinishedQuestions)}

Unfinished7 = UnfinishLeaner.function(cyber.security.7.step.activity, cyber.security.7.question.response)
cache("Unfinished7")
Unfinished6 = UnfinishLeaner.function(cyber.security.6.step.activity, cyber.security.6.question.response)
cache("Unfinished6")
Unfinished5 = UnfinishLeaner.function(cyber.security.5.step.activity, cyber.security.5.question.response)
cache("Unfinished5")
Unfinished4 = UnfinishLeaner.function(cyber.security.4.step.activity, cyber.security.4.question.response)
cache("Unfinished4")
Unfinished3 = UnfinishLeaner.function(cyber.security.3.step.activity, cyber.security.3.question.response)
cache("Unfinished3")
Unfinished2 = UnfinishLeaner.function(cyber.security.2.step.activity, cyber.security.2.question.response)
cache("Unfinished2")
Unfinished1 = UnfinishLeaner.function(cyber.security.1.step.activity, cyber.security.1.question.response)
cache("Unfinished1")

At = ggplot(data = Unfinished7, aes(x = Unfinished, y = Attempts))
At1 = At + geom_point(aes(x = Unfinished, y = Quant_Attempts))
At1

At = ggplot(data = Unfinished1, aes(x = Unfinished, y = Quant_Attempts))
At1 = At + geom_point(aes(x = Unfinished, y = Quant_Attempts, colour = "Course 1"))
At2 = At1 + geom_point(data = Unfinished2,(aes( x = Unfinished, y = Quant_Attempts, colour = "Course2")))
At3 = At2 + geom_point(data = Unfinished3,(aes( x = Unfinished, y = Quant_Attempts, colour = "Course3")))
At4 = At3 + geom_point(data = Unfinished4,(aes( x = Unfinished, y = Quant_Attempts, colour = "Course4")))
At5 = At4 + geom_point(data = Unfinished5,(aes( x = Unfinished, y = Quant_Attempts, colour = "Course5")))
At6 = At5 + geom_point(data = Unfinished6,(aes( x = Unfinished, y = Quant_Attempts, colour = "Course6")))
At7 = At6 + geom_point(data = Unfinished7,(aes( x = Unfinished, y = Quant_Attempts, colour = "Course7")))
At7

An = ggplot(data = Unfinished1, aes(x = Unfinished, y = Correct_Answers))
An1 = An + geom_point(aes(x = Unfinished, y = Correct_Answers, colour = "Course 1"))
An2 = An1 + geom_point(data = Unfinished2,(aes( x = Unfinished, y = Correct_Answers, colour = "Course2")))
An3 = An2 + geom_point(data = Unfinished3,(aes( x = Unfinished, y = Correct_Answers, colour = "Course3")))
An4 = An3 + geom_point(data = Unfinished4,(aes( x = Unfinished, y = Correct_Answers, colour = "Course4")))
An5 = An4 + geom_point(data = Unfinished5,(aes( x = Unfinished, y = Correct_Answers, colour = "Course5")))
An6 = An5 + geom_point(data = Unfinished6,(aes( x = Unfinished, y = Correct_Answers, colour = "Course6")))
An7 = An6 + geom_point(data = Unfinished7,(aes( x = Unfinished, y = Correct_Answers, colour = "Course7")))
An7

P = ggplot(data = Unfinished1, aes(x = Unfinished, y = ProportionCorrect))
P1 = P + geom_point(aes(x = Unfinished, y = ProportionCorrect, colour = "Course 1"))
P2 = P1 + geom_point(data = Unfinished2,(aes( x = Unfinished, y = ProportionCorrect, colour = "Course2")))
P3 = P2 + geom_point(data = Unfinished3,(aes( x = Unfinished, y = ProportionCorrect, colour = "Course3")))
P4 = P3 + geom_point(data = Unfinished4,(aes( x = Unfinished, y = ProportionCorrect, colour = "Course4")))
P5 = P4 + geom_point(data = Unfinished5,(aes( x = Unfinished, y = ProportionCorrect, colour = "Course5")))
P6 = P5 + geom_point(data = Unfinished6,(aes( x = Unfinished, y = ProportionCorrect, colour = "Course6")))
P7 = P6 + geom_point(data = Unfinished7,(aes( x = Unfinished, y = ProportionCorrect, colour = "Course7")))
P7


