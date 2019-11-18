
library(ProjectTemplate)
load.project() 

Filter = StepActivity7 %>% filter(last_completed_at !="")
StepActivityFilter=data.frame(Filter)


QuantitiesFinishing.function = function(x){ #x = file selected to analyse
  StepActivity = x
  Filter = StepActivity %>% filter(last_completed_at !="")
  StepActivity=data.frame(Filter)
  Steps=c((StepActivity$week_number)+((StepActivity$step_number)/100)) #acknowledging data complication
  StepActivity = cbind(StepActivity, Steps) #editing the data with the new column
  S= unique(StepActivity$Steps, incomparables = FALSE) #list the unique steps
  L =length(S) #How many steps are there?
  
  #making the vector of quantities for each step
  
  total_values = vector() #making the vector
  for(i in 1:L){ #for loop: L = how many steps will be assessed/length of vector
    Step= StepActivity %>% filter(Steps==S[i]) #filter: collect all the rows for a single step
    Quant = nrow(Step) #how many rows were collected?
    total_values[i]= Quant} #compile a vector of how many people started each step
  
  #making the dataframe to be able to chart: Step against how many participants.
  DFActivity = data.frame(Step = S, Total = total_values)
  
  return(DFActivity)} #return: the final vector


#The final data frames
CourseF1 =QuantitiesFinishing.function(cyber.security.1.step.activity) #run the function for each file
CourseF2 =QuantitiesFinishing.function(cyber.security.2.step.activity)
CourseF3 =QuantitiesFinishing.function(cyber.security.3.step.activity)
CourseF4 =QuantitiesFinishing.function(cyber.security.4.step.activity)
CourseF5 =QuantitiesFinishing.function(cyber.security.5.step.activity)
CourseF6 =QuantitiesFinishing.function(cyber.security.6.step.activity)
CourseF7 =QuantitiesFinishing.function(cyber.security.7.step.activity)

FinishersDF = merge.data.frame(CourseF1, CourseF2, by = "Step", all=TRUE)
FinishersDF = FinishersDF%>% rename(Run_1 = Total.x, Run_2 = Total.y )
FinishersDF = merge.data.frame(FinishersDF, CourseF3, by = "Step", all=TRUE)
FinishersDF = FinishersDF%>% rename(Run_3 = Total)
FinishersDF = merge.data.frame(FinishersDF, CourseF4, by = "Step", all=TRUE)
FinishersDF = FinishersDF%>% rename(Run_4 = Total)
FinishersDF = merge.data.frame(FinishersDF, CourseF5, by = "Step", all=TRUE)
FinishersDF = FinishersDF%>% rename(Run_5 = Total)
FinishersDF = merge.data.frame(FinishersDF, CourseF6, by = "Step", all=TRUE)
FinishersDF = FinishersDF%>% rename(Run_6 = Total)
FinishersDF = merge.data.frame(FinishersDF, CourseF7, by = "Step", all=TRUE)
FinishersDF = FinishersDF%>% rename(Run_7 = Total)

cache("FinishersDF")

#making the first graph: How many finished each step?


Finishers = ggplot (data = FinishersDF,aes (x = Step, y = Total))
CourseF1Graph = Plot +geom_point(aes( x= Step, y = Run_1, colour = "Course Run 1"))
CourseF2Graph = CourseF1Graph +geom_point(aes( x= Step, y = Run_2, colour = "Course Run 2"))
CourseF3Graph = CourseF2Graph +geom_point(aes( x= Step, y = Run_3, colour = "Course Run 3"))
CourseF4Graph = CourseF3Graph +geom_point(aes( x= Step, y = Run_4, colour = "Course Run 4"))
CourseF5Graph = CourseF4Graph +geom_point(aes( x= Step, y = Run_5, colour = "Course Run 5"))
CourseF6Graph = CourseF5Graph +geom_point(aes( x= Step, y = Run_6, colour = "Course Run 6"))
CourseF7Graph = CourseF6Graph +geom_point(aes( x= Step, y = Run_7, colour = "Course Run 7"))

CourseS7Graph



#Proportion graph: out of all starters, how many finished?

FinishProp = ggplot (data = CourseF3,aes (x = Step, y = Proportion_Finishers))
#CourseFP1Graph = Plot +geom_point (aes (x= Step, y = Course1$Total, colour = "Course Run 1"))
#CourseFP2Graph = Course1Graph + geom_point (aes(x = Step, y = Course2$Total, colour = "Course Run 2"))
CourseFP3Graph = FinishProp + geom_point (aes (x= Step, y = CourseF3$Total/CourseS3$Total, colour = "Course Run 3"))
CourseFP4Graph = CourseFP3Graph + geom_point (aes(x = Step, y = CourseF4$Total/CourseS4$Total, colour = "Course Run 4"))
CourseFP5Graph = CourseFP4Graph +geom_point (aes (x= Step, y = CourseF5$Total/CourseS5$Total, colour = "Course Run 5"))
CourseFP6Graph = CourseFP5Graph + geom_point (aes(x = Step, y = CourseF6$Total/CourseS6$Total, colour = "Course Run 6"))
CourseFP7Graph = CourseFP6Graph + geom_point (aes(x = Step, y = CourseF7$Total/CourseS7$Total, colour = "Course Run 7"))

CourseFP7Graph


