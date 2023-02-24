import excel using "C:\Users\jwang\OneDrive - Johns Hopkins\local\ICTR\2022\202212Mira\ProteomicAnalysesAnd_DATA_2022-12-18_1438 _forARVO2023_withHbA1c.xlsx", firstrow clear case(upper)

/*
"control" is HbA1c<5.7
"prediabetes" is HbA1c 5.7-6.4 (inclusive of the end values)
"diabetes" is HbA1c > 6.4
*/

gen grp=0 if HEMOGLOBIN_A1CV2<5.7
replace grp=1 if HEMOGLOBIN_A1CV2>=5.7&HEMOGLOBIN_A1CV2<=6.4
replace grp=2 if HEMOGLOBIN_A1CV2>6.4& HEMOGLOBIN_A1CV2~=.

label define grpl 0 "Control" 1 "Prediabetes" 2 "Diabetes"
label val grp grpl

rename *_GCL_IPL* *_*

rename SUN_CREATININE_RATIOV2 creatinine
rename INSULIN_LAB_ASSAY insulin
rename LEPTIN_LAB_ASSAY leptin

rename *_OD *0
rename *_OS *1

reshape long CSF SI SO NI NO II IO TI TO, i(IRB) j(OS)

tabstat BMI creatinine ADIPONECTIN insulin leptin CSF SI SO NI NO II IO TI TO, s(n mean sd p50 min max) c(s) format(%5.0f)

sort IRB OS
by IRB: gen num=_n

table1 if num==1, by(grp) vars(BMI contn \ creatinine contn \ ADIPONECTIN contn \ insulin contn \ leptin contn) saving("C:\Users\jwang\OneDrive - Johns Hopkins\local\ICTR\2022\202212Mira\ProteomicAnalyses_forARVO2023_descriptive.xlsx",sheet("Patient", replace)) format(%5.0f)

table1, by(grp) vars(CSF contn \ SI contn \ SO contn \ NI contn \ NO contn \ II contn \ IO contn \ TI contn \ TO contn) saving("C:\Users\jwang\OneDrive - Johns Hopkins\local\ICTR\2022\202212Mira\ProteomicAnalyses_forARVO2023_descriptive.xlsx",sheet("Eye",replace)) format(%5.0f)


/*The question is, are any of the following 5 metabolic parameters associated with inner retinal thickness ("GCL_IPL") among all of our groups (17 controls, 18 prediabetes, 13 diabetes):
 
BMI
Serum BUN/Creatinine ratio
Adiponectin
Insulin
Leptin
 
There are several GCL_IPL retinal thickness values for each participant, representing the corresponding macular field ("csf" = central subfield, "Si" = superior inner, "So" = superior outer, "Ni" = nasal inner, "No" = nasal outer, "Ii" = inferior inner, "Io" = inferior outer, "Ti" = temporal inner, "To" = temporal outer"), plus separate values for the right eye ("OD") and left eye ("OS").  If it's doable to look at associations with thickness in each field, even if only in one eye, that would be wonderful.
*/

foreach varx in BMI creatinine ADIPONECTIN insulin leptin {

   foreach var in CSF SI SO NI NO II IO TI TO {
	
	mixed `var' `varx' ||IRB:
	estimates store `var'_`varx'
	
   }
  
}

foreach varx in BMI creatinine ADIPONECTIN insulin leptin {
   
esttab *_`varx' using "C:\Users\jwang\OneDrive - Johns Hopkins\local\ICTR\2022\202212Mira\ProteomicAnalyses_`varx'.rtf", cells(b(star fmt(%5.3f)) ci(fmt(%5.3f)) p(fmt(%5.3f))) wide replace

}	


foreach varx in BMI creatinine ADIPONECTIN insulin leptin {
	
	quietly: sum `varx'
	gen `varx'_center=`varx'-r(mean)
	
}


log using "C:\Users\jwang\OneDrive - Johns Hopkins\local\ICTR\2022\202212Mira\regression_$S_DATE.log",replace
foreach varx in BMI creatinine ADIPONECTIN insulin leptin {

   foreach var in CSF SI SO NI NO II IO TI TO {
	
	mixed `var' c.`varx'_center##i.grp ||IRB:
	*estimates store `var'_`varx'_grp
	lincom `varx'_center+1.grp#`varx'_center
	lincom `varx'_center+2.grp#`varx'_center
	
   }
  
}
log close

foreach varx in BMI creatinine ADIPONECTIN insulin leptin {
   
esttab *_`varx'_grp using "C:\Users\jwang\OneDrive - Johns Hopkins\local\ICTR\2022\202212Mira\ProteomicAnalyses_`varx'_grp.rtf", cells(b(star fmt(%5.3f)) ci(fmt(%5.3f)) p(fmt(%5.3f))) wide replace

}	