#1) How Many Times For Each Course Does a Learner Start a Section, But Not finish It?

#1a)How many times per learner?

Incomplete_Quantity1= cyber.security.1.step.activity %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
Incomplete_Quantity2= cyber.security.2.step.activity %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
Incomplete_Quantity3= cyber.security.3.step.activity %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
Incomplete_Quantity4= cyber.security.4.step.activity %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
Incomplete_Quantity5= cyber.security.5.step.activity %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
Incomplete_Quantity6= cyber.security.6.step.activity %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
Incomplete_Quantity7= cyber.security.7.step.activity %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
IncompleteDF = data.frame(Course1 = nrow(Incomplete_Quantity1),Course2 = nrow(Incomplete_Quantity2),
                          Course3 = nrow(Incomplete_Quantity3),Course4 = nrow(Incomplete_Quantity4),
                          Course5 = nrow(Incomplete_Quantity5),Course6 = nrow(Incomplete_Quantity6),
                          Course7 = nrow(Incomplete_Quantity7))



#flaw in leaving responses - only a survey, not actual leavers.
Responses_match7= cyber.security.7.leaving.survey.responses %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)






#1)for each learner ID with only one not finished, what was that step?

 SingleUnfinish.Function=function(x){
   StepActivity = x
   Incomplete_Quantity= StepActivity %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
   Incomplete_Single = Incomplete_Quantity%>% filter(n == 1)
   L = nrow(Incomplete_Single) #making the DF of "one step unfinished"
   
   Steps=c((StepActivity$week_number)+((StepActivity$step_number)/100)) #acknowledging data complication
   StepActivity = cbind(StepActivity, Steps) #correcting the steps confusion
   
   Learner = vector() #make the vector
   for(i in 1:L){ 
   ID = (StepActivity %>% filter(learner_id == Incomplete_Single$learner_id[i])%>% filter(last_completed_at == ""))
    #Pick out the ID from the dataframe that has been identified as only having one stage unfinished
   Learner [i]= ID$Steps} #Which step was unfinished?
   LearnerSingleDF = data.frame(Learner = Incomplete_Single$learner_id, Step = Learner)
   UnfinishedSteps = LearnerSingleDF%>% count(Step) %>% arrange(-n)
   return(UnfinishedSteps)
   }
 
Single1 = SingleUnfinish.Function(cyber.security.1.step.activity)
Single2 = SingleUnfinish.Function(cyber.security.2.step.activity)
Single3 = SingleUnfinish.Function(cyber.security.3.step.activity)
Single4 = SingleUnfinish.Function(cyber.security.4.step.activity)
Single5 = SingleUnfinish.Function(cyber.security.5.step.activity)
Single6 = SingleUnfinish.Function(cyber.security.6.step.activity)
Single7 = SingleUnfinish.Function(cyber.security.7.step.activity)


cache("Single1")
cache("Single2")
cache("Single3")
cache("Single4")
cache("Single5")
cache("Single6")
cache("Single7")

LargeDF = merge.data.frame(Single1, Single2, by = "Step", all=TRUE)
LargeDF = LargeDF%>% rename(Run_1 = n.x, Run_2 = n.y )
LargeDF = merge.data.frame(LargeDF, Single3, by = "Step", all=TRUE)
LargeDF = LargeDF%>% rename(Run_3 = n)
LargeDF = merge.data.frame(LargeDF, Single4, by = "Step", all=TRUE)
LargeDF = LargeDF%>% rename(Run_4 = n)
LargeDF = merge.data.frame(LargeDF, Single5, by = "Step", all=TRUE)
LargeDF = LargeDF%>% rename(Run_5 = n)
LargeDF = merge.data.frame(LargeDF, Single6, by = "Step", all=TRUE)
LargeDF = LargeDF%>% rename(Run_6 = n)
LargeDF = merge.data.frame(LargeDF, Single7, by = "Step", all=TRUE)
LargeDF = LargeDF%>% rename(Run_7 = n)

LargeDF[is.na(LargeDF)] <- 0
cache("LargeDF")


U = ggplot(data = LargeDF, aes(x = Step, y=Total))
U1 = U + geom_point (aes(x = Step, y=Run_1, colour = "Course1"))
U2 = U1 + geom_point (aes(x = Step, y=Run_2, colour = "Course2"))
U3 = U2 + geom_point (aes(x = Step, y=Run_3, colour = "Course3"))
U4 = U3 + geom_point (aes(x = Step, y=Run_4, colour = "Course4"))
U5 = U4 + geom_point (aes(x = Step, y=Run_5, colour = "Course5"))
U6 = U5 + geom_point (aes(x = Step, y=Run_6, colour = "Course6"))
U7 = U6 + geom_point (aes(x = Step, y=Run_7, colour = "Course7"))

U7

#See number that drop at the start

