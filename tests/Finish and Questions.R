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
  return(UnfinishedQuestions)}

UnfinishLeaner.function(cyber.security.7.step.activity, cyber.security.7.question.response)

T = ggplot(data = UnfinishedQuestions, aes(x = Unfinished, y = ProportionCorrect))
T1 = T + geom_point(aes(x = Unfinished, y = ProportionCorrect))
T1

#add more years???

