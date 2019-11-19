Unknowns.function = function(x){ #x = file selected to analyse
Enrolements = x
L =length(Enrolements$learner_id)
#gender
  Unknown= Enrolements %>% filter(gender=="Unknown")
  Count = nrow(Unknown)
  L =length(Enrolements$learner_id)
  Gender = Count/L
#Country
  Unknown= Enrolements %>% filter(country=="Unknown")
  Count = nrow(Unknown)
  L =length(Enrolements$learner_id)
  Country = Count/L
#Education
  Unknown= Enrolements %>% filter(highest_education_level=="Unknown")
  Count = nrow(Unknown)
  L =length(Enrolements$learner_id)
  Education = Count/L
  
 CourseUnknown = c(Gender, Country, Education)
 
 return(CourseUnknown)}


  Unknowns1=Unknowns.function(cyber.security.1.enrolments)
  Unknowns2=Unknowns.function(cyber.security.2.enrolments)
  Unknowns3=Unknowns.function(cyber.security.3.enrolments)
  Unknowns4=Unknowns.function(cyber.security.4.enrolments)
  Unknowns5=Unknowns.function(cyber.security.5.enrolments)
  Unknowns6=Unknowns.function(cyber.security.6.enrolments)
  Unknowns7=Unknowns.function(cyber.security.7.enrolments)

Run = 1:7
Gender = c(Unknowns1 [1], Unknowns2 [1], Unknowns3 [1], Unknowns4[1], Unknowns5[1], Unknowns6[1], Unknowns7[1])
Country = c(Unknowns1 [2], Unknowns2 [2], Unknowns3 [2], Unknowns4[2], Unknowns5[2], Unknowns6[2], Unknowns7[2])
Education = c(Unknowns1 [3], Unknowns2 [3], Unknowns3 [3], Unknowns4[3], Unknowns5[3], Unknowns6[3], Unknowns7[3])

dfunknowns = data.frame(Run, Gender, Country, Education)

U=ggplot(data=dfunknowns, aes(x=Run, y= Gender)) 
U1 = U + geom_point (aes (x=Run, y=Gender, colour = "Gender"))
U2 = U1 + geom_point (aes (x=Run, y= Country, colour = "Country"))
U3 = U2 + geom_point (aes (x=Run, y=Education, colour = "Education"))
Scaled = U3 + ylim(0.5,1)


U3 #showing all levels of unknowns
Scaled #importance of scale - see that all are high
#conclusion: clear difficulties to do evaluations based on age/gender etc - see poor quality of sample.


#comparing the samples  - need to use 3 - 7, as different step counts, not comparible programmes. 
