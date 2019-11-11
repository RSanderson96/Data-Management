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

