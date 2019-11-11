library(ProjectTemplate)
load.project()


QuantitiesStarting.function = function(x){ #x = file selected to analyse
  StepActivity = x
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
CourseS1 =QuantitiesStarting.function(cyber.security.1.step.activity) #run the function for each file
sum(CourseS1$Total)  #rows counted? Needs to match the number of rows in the original file(testing function)
CourseS2 =QuantitiesStarting.function(cyber.security.2.step.activity)
sum(CourseS2$Total)
CourseS3 =QuantitiesStarting.function(cyber.security.3.step.activity)
sum(CourseS3$Total)
CourseS4 =QuantitiesStarting.function(cyber.security.4.step.activity)
sum(CourseS4$Total)
CourseS5 =QuantitiesStarting.function(cyber.security.5.step.activity)
sum (CourseS5$Total)
CourseS6 =QuantitiesStarting.function(cyber.security.6.step.activity)
sum(CourseS6$Total)
CourseS7 =QuantitiesStarting.function(cyber.security.7.step.activity)
sum(CourseS7$Total)

cache("CourseS7")

#sum (course)checking the dataframes - making them compatible





#making the first graph

#Decision: dismiss 1&2 as won't fit the graph - need to clean the data

Plot = ggplot (data =CourseS3,aes (x = X, y = Total))
#CourseS1Graph = Plot +geom_point (aes (x= Step, y = Course1$Total, colour = "Course Run 1"))
#CourseS2Graph = Course1Graph + geom_point (aes(x = Step, y = Course2$Total, colour = "Course Run 2"))
CourseS3Graph =Plot +geom_point (aes (x= Step, y = CourseS3$Total, colour = "Course Run 3"))
CourseS4Graph = CourseS3Graph + geom_point (aes(x = Step, y = CourseS4$Total, colour = "Course Run 4"))
CourseS5Graph = CourseS4Graph +geom_point (aes (x= Step, y = CourseS5$Total, colour = "Course Run 5"))
CourseS6Graph = CourseS5Graph + geom_point (aes(x = Step, y = CourseS6$Total, colour = "Course Run 6"))
CourseS7Graph = CourseS6Graph + geom_point (aes(x = Step, y = CourseS7$Total, colour = "Course Run 7"))


#Proportion graph

StartProp = ggplot (data = CourseS3,aes (x = X, y = Total))
#Course1Graph = Plot +geom_point (aes (x= Step, y = Course1$Total, colour = "Course Run 1"))
#Course2Graph = Course1Graph + geom_point (aes(x = Step, y = Course2$Total, colour = "Course Run 2"))
CourseSP3Graph =StartProp +geom_point (aes (x= Step, y = CourseS3$Total/(Cohort_Summaries$Entries[3]), colour = "Course Run 3"))
CourseSP4Graph = CourseSP3Graph + geom_point (aes(x = Step, y = CourseS4$Total/(Cohort_Summaries$Entries[4]), colour = "Course Run 4"))
CourseSP5Graph = CourseSP4Graph +geom_point (aes (x= Step, y = CourseS5$Total/(Cohort_Summaries$Entries[5]), colour = "Course Run 5"))
CourseSP6Graph = CourseSP5Graph + geom_point (aes(x = Step, y = CourseS6$Total/(Cohort_Summaries$Entries[6]), colour = "Course Run 6"))
CourseSP7Graph = CourseSP6Graph + geom_point (aes(x = Step, y = CourseS7$Total/(Cohort_Summaries$Entries[7]), colour = "Course Run 7"))

CourseSP7Graph


