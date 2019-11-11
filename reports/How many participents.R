library(ProjectTemplate)
load.project()

StepActivity1= cyber.security.1.step.activity
Steps=c((StepActivity1$week_number)+((StepActivity1$step_number)/100)) #acknowledging data complication
StepActivity1 = cbind(StepActivity1, Steps) #editing the data with the new column
S= unique(StepActivity1$Steps, incomparables = FALSE) #number of steps in the programme
L1 =length(S)

StepActivity2= cyber.security.2.step.activity
Steps=c((StepActivity2$week_number)+((StepActivity2$step_number)/100)) #acknowledging data complication
StepActivity2 = cbind(StepActivity2, Steps) #editing the data with the new column
S= unique(StepActivity2$Steps, incomparables = FALSE) #number of steps in the programme
L2 =length(S)

StepActivity3= cyber.security.3.step.activity
Steps=c((StepActivity3$week_number)+((StepActivity3$step_number)/100)) #acknowledging data complication
StepActivity3 = cbind(StepActivity3, Steps) #editing the data with the new column
S= unique(StepActivity3$Steps, incomparables = FALSE) #number of steps in the programme
L3 =length(S)

StepActivity4= cyber.security.4.step.activity
Steps=c((StepActivity4$week_number)+((StepActivity4$step_number)/100)) #acknowledging data complication
StepActivity4 = cbind(StepActivity4, Steps) #editing the data with the new column
S= unique(StepActivity4$Steps, incomparables = FALSE) #number of steps in the programme
L4 =length(S)

StepActivity5= cyber.security.5.step.activity
  Steps=c((StepActivity5$week_number)+((StepActivity5$step_number)/100)) #acknowledging data complication
  StepActivity5 = cbind(StepActivity5, Steps) #editing the data with the new column
  S= unique(StepActivity5$Steps, incomparables = FALSE) #number of steps in the programme
  L5 =length(S)

StepActivity6= cyber.security.6.step.activity
  Steps=c((StepActivity6$week_number)+((StepActivity6$step_number)/100)) #acknowledging data complication
  StepActivity6 = cbind(StepActivity6, Steps) #editing the data with the new column
  S= unique(StepActivity6$Steps, incomparables = FALSE) #number of steps in the programme
  L6 =length(S)

StepActivity7= cyber.security.7.step.activity
  Steps=c((StepActivity7$week_number)+((StepActivity7$step_number)/100)) #acknowledging data complication
  StepActivity7 = cbind(StepActivity7, Steps) #editing the data with the new column
  S7= unique(StepActivity7$Steps, incomparables = FALSE) #number of steps in the programme
  L7 =length(S7)
#function: make the vector of quantities

Quantities.function = function()
  
total_values7 = vector()
for(i in 1:L7){
    Step= StepActivity7 %>% filter(Steps==S7[i])
    Quant = nrow(Step)
    total_values7[i]= Quant}

total_values7


Quantities1
DFActivity= data.frame(Step = S, Total = total_values)

g = ggplot (data = DFActivity, aes (x = Step, y = Total))
g1 = g + geom_line (aes(x = Step, y = Total, colour = "Course Run 7"))
g1          

StepActivity7 = cyber.security.7.step.activity

char.vec = as.character(cyber.security.7.step.activity[,6])
non.missing = which(char.vec != "")
complete.data = cyber.security.7.step.activity[non.missing,]

complete.data = cyber.security.7.step.activity$last_completed_at != empty
complete.data

False = StepActivity7[1,6]
False

Quantities.function = function()
  
  total_values7 = vector()
for(i in 1:L7){
  Step= StepActivity7 %>% filter(Steps==S7[i])
  Quant = nrow(Step)
  total_values7[i]= Quant}

total_values7


Filter = StepActivity7 %>% filter(last_completed_at !="")
StepActivityFilter=data.frame(Filter)
