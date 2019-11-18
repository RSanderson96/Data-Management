library(ProjectTemplate)
load.project()

Enrolements1 = cyber.security.1.enrolments
Enrolements2 = cyber.security.2.enrolments
Enrolements3 = cyber.security.3.enrolments
Enrolements4 = cyber.security.4.enrolments
Enrolements5 = cyber.security.5.enrolments
Enrolements6 = cyber.security.6.enrolments
Enrolements7 = cyber.security.7.enrolments

People1 = nrow(Enrolements1)
People2 = nrow(Enrolements2)
People3 = nrow(Enrolements3)
People4 = nrow(Enrolements4)
People5 = nrow(Enrolements5)
People6 = nrow(Enrolements6)
People7 = nrow(Enrolements7)

Entries = c(People1, People2, People3,People4, People5, People6, People7)

Cohort = 1:7

Cohort_Summaries = data.frame(Cohort = Cohort, Entries = Entries)

Cohort_Summaries


#Number of steps

Steps.function = function(x){
StepActivity = x
Steps=c((StepActivity$week_number)+((StepActivity$step_number)/100)) #acknowledging data complication
StepActivity = cbind(StepActivity, Steps) #editing the data with the new column
S= unique(StepActivity$Steps, incomparables = FALSE) #list the unique steps
L =length(S)
return(L)}

Steps1 = Steps.function(cyber.security.1.step.activity)
Steps2 = Steps.function(cyber.security.2.step.activity)
Steps3 = Steps.function(cyber.security.3.step.activity)
Steps4 = Steps.function(cyber.security.4.step.activity)
Steps5 = Steps.function(cyber.security.5.step.activity)
Steps6 = Steps.function(cyber.security.6.step.activity)
Steps7 = Steps.function(cyber.security.7.step.activity)

Steps_Vector = c(Steps1, Steps2, Steps3, Steps4, Steps5, Steps6, Steps7)

Cohort_Summaries = cbind(Cohort_Summaries, Steps_Vector)

VideoSteps = c(0,0,13,13,13,13,13)

Cohort_Summaries = cbind(Cohort_Summaries, VideoSteps)

Cohort_Summaries = Cohort_Summaries%>% rename(Enrolments=Entries,Number_of_Steps= Steps_Vector, Number_of_Video_Steps = VideoSteps)
