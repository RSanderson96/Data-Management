library(ProjectTemplate)
load.project()

#1) How Many Times For Each Course Does a Learner Start a Section, But Not finish It?

#1a)How many times per learner?

Incomplete_Quantity7= cyber.security.7.step.activity %>% filter(last_completed_at == "") %>% count(learner_id) %>% arrange(-n)
IncompleteDF = data.frame(Course1 = nrow(Incomplete_Quantity1),Course2 = nrow(Incomplete_Quantity2),
                          Course3 = nrow(Incomplete_Quantity3),Course4 = nrow(Incomplete_Quantity4),
                          Course5 = nrow(Incomplete_Quantity5),Course6 = nrow(Incomplete_Quantity6),
                          Course7 = nrow(Incomplete_Quantity7))

#flaw in leaving responses - only a survey, not actual leavers.

#for each learner ID with only one not finished, what was that step?


 
 U = ggplot(data = UnfinishedSteps, aes(x = Step, y=Total))
 U1 = U + geom_point (aes(x = Step, y=UnfinishedSteps$n, colour = "Course7"))
 U1
 
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






# A tibble: 1,311 x 2

cyber.security.7.step.activity %>% filter(learner_id == "20e6ec35-0f50-4819-9c2e-d1851fd54638")

# A tibble: 5 x 6