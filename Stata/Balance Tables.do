clear all
import delimited C:\Users\dnlang86\Documents\GitHub\VideoAnalysis\VideoAnalysis_cloud_dir\data\processed\user_characteristics.tsv

keep if inlist(course,"Algorithms", "DBSQL","MedStats","StatLearn")
drop if course=="CS101"
drop if (course=="CS101" & !(cohort=="(1.25x) Fast" | cohort=="Normal Speed (1.0x)"))
tab cohort
gen fast= inlist(cohort,"1.25X","Fast", "Fast (1.25x)")
tab cohort fast

*import delimited C:\Users\dnlang86\Documents\GitHub\VideoAnalysis\VideoAnalysis_cloud_dir\data\processed\user_characteristics.tsv
*drop if course=="CS101"
*gen fast= inlist(cohort,"1.25X","Fast", "Fast (1.25x)")
encode gender,gen(gender_code)
encode course,gen(course_code)
destring year_of_birth,replace force
gen missing_birth_year=year_of_birth==.
tab curr_age
tab level_of_education
encode level_of_education,gen(education_code)
encode country_name ,gen(country_code)
destring grade,force replace
destring curr_age ,force replace
replace grade=0 if grade==.
gen missing_age=curr_age==.
gen male=gender=="m"
gen gender_missing=inlist(gender,"NA","NULL")
gen higher_ed=inlist(level_of_education,"Bachelors","Doctorate", " Masters or professional degree")
gen  domestic=country_name=="United States"

label var male "Male"
label var gender_missing "Missing Gender"
label var higher_ed "College or Higher"
label var domestic "Domestic Learner"
label var curr_age "Age"
label var missing_age "Missing Age"
label values fast
iebaltab male gender_missing higher_ed domestic curr_age missing_age,grpvar(fast) fixedeffect(course_code) vce(cluster course_code) savetex(balance_check.tex) replace rowvarlabel 

