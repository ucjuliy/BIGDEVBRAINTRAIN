
/*ALL PRE-PROCESSING DONE - DO NOT RUN AGAIN*/
/* some useful syntax */
duplicates report p_id



/*merge 1:1 p_id using "C:\Users\keertana\Desktop\MAINSTUDY\T1\Flanker\Flanker.dta"
merge 1:1 p_id using "C:\Users\keertana\Desktop\MAINSTUDY\T1\fmri SSRT\SSRT_fmri_t1.dta"*/


/*insert data file here*/
use "C:\Users\keertana\Desktop\MAINSTUDY\T1\final output\NEWMASTER.dta" 


/*export delimited using "C:\Users\keertana\Desktop\MAINSTUDY\T1\final output\RAW.csv", replace*/
 /*reshape long  havardoxford_cortl_t havardoxford_cortl_proactive_t havardoxford_cortl_reactive_t inhibitionnet_proactive_t inhibitionnet_reactive_t calcarine_r_proactive_t caudate_r_proactive_t putamen_r_proactive_t thalamus_r_proactive_t calcarine_r_reactive_t caudate_r_reactive_t putamen_r_reactive_t thalamus_r_reactive_t thalamus_l_reactive_t thalamus_l_proactive_t putamen_l_proactive_t putamen_l_reactive_t presma_l_proactive_t presma_l_reactive_t, i(p_id) j(session)
 export delimited using "C:\Users\keertana\Desktop\MAINSTUDY\T1\final output\RAWLONGFMRI.csv", replace*/

******************************************/*BASELINE*/*******************************************************
gen agetraingroup = 1 if agegroup == 1 & group == 1 
replace agetraingroup = 2 if agegroup == 2 & group == 1
replace agetraingroup = 3 if agegroup == 1 & group == 2
replace agetraingroup = 4 if agegroup == 2 & group == 2

 ttest age, by(group)
 
 
********VALIDATION FOR TASKS**********
/*ssrt*/

/*BIG FAT NOTE: SSRT for t2 have been accidentally labelled opposite. So use ssrt_t2 for non-replacement method and ssrtnr_t2 for replacement method*/






 pwcorr zssrtnr_t0 meango_corrrtnr_t0, sig star(.05) obs

******************************************/*SSRT*/*******************************************************
/*gen ssrtalt_t0 = ssrt_t0
 gen ssrtalt_t1 = ssrt_t1 */
 
 /*replacement method*/
/* 
replace ssrt_t0 =. if ssrt_t0 < 0
replace ssrt_t1 =. if ssrt_t1 < 0
replace ssrtnr_t2 =. if ssrtnr_t2 < 0


sum ssrt_t0 
replace ssrt_t0 = . if ssrt_t0 > (r(mean) + 2*r(sd)) | ssrt_t0 < (r(mean) - 2*r(sd))
sum ssrt_t1
replace ssrt_t1 = . if ssrt_t1 > (r(mean) + 2*r(sd)) | ssrt_t1 < (r(mean) - 2*r(sd))
sum ssrtnr_t2
replace ssrtnr_t2 = . if ssrtnr_t2 > (r(mean) + 2*r(sd)) | ssrtnr_t2 < (r(mean) - 2*r(sd)) */

/* non-replacement method*/

sum meango_corrrtnr_t0
replace meango_corrrtnr_t0 = . if meango_corrrtnr_t0 > (r(mean) + 2*r(sd)) | meango_corrrtnr_t0 < (r(mean) - 2*r(sd))

sum meango_corrrtnr_t1
replace meango_corrrtnr_t1 = . if meango_corrrtnr_t1 > (r(mean) + 2*r(sd)) | meango_corrrtnr_t1 < (r(mean) - 2*r(sd))

sum meango_corrrt_t2
replace meango_corrrt_t2 = . if meango_corrrt_t2 > (r(mean) + 2*r(sd)) | meango_corrrt_t2 < (r(mean) - 2*r(sd))

drop meango_corrrtnr_t2

rename meango_corrrt_t2 meango_corrrtnr_t2

/*replace meango_corrrtnr_t0 =. if ssrtnr_t0 < 0
replace meango_corrrtnr_t1 =. if ssrtnr_t1 < 0
replace meango_corrrtnr_t2 =. if ssrt_t2 < 0*/


 ttest meango_corrrtnr_t0 == meango_corrrtnr_t1 if group == 1
 ttest meango_corrrtnr_t0 == meango_corrrtnr_t1 if group == 2
  
 ttest meango_corrrtnr_t0 == meango_corrrtnr_t2 if group == 1
 ttest meango_corrrtnr_t0 == meango_corrrtnr_t2 if group == 2
 
ttest meango_corrrtnr_t1 == meango_corrrtnr_t2 if group == 1
 ttest meango_corrrtnr_t1 == meango_corrrtnr_t2 if group == 2
 
 
gen changegort = meango_corrrtnr_t1 - meango_corrrtnr_t0


replace ssrtnr_t0 =. if ssrtnr_t0 < 0
replace ssrtnr_t1 =. if ssrtnr_t1 < 0
replace ssrt_t2 =. if ssrt_t2 < 0

sum ssrtnr_t0 
replace ssrtnr_t0 = . if ssrtnr_t0 > (r(mean) + 2*r(sd)) | ssrtnr_t0 < (r(mean) - 2*r(sd))
sum ssrtnr_t1
replace ssrtnr_t1 = . if ssrtnr_t1 > (r(mean) + 2*r(sd)) | ssrtnr_t1 < (r(mean) - 2*r(sd))
sum ssrt_t2
replace ssrt_t2 = . if ssrt_t2 > (r(mean) + 2*r(sd)) | ssrt_t2 < (r(mean) - 2*r(sd))

/*fmri as well*/

replace ssrtfmri_t0 =. if ssrtfmri_t0 < 0
replace ssrtfmri_t1 =. if ssrtfmri_t1 < 0

sum ssrtfmri_t0 
replace ssrtfmri_t0 = . if ssrtfmri_t0 > (r(mean) + 2*r(sd)) | ssrtfmri_t0 < (r(mean) - 2*r(sd))
sum ssrtfmri_t1
replace ssrtfmri_t1 = . if ssrtfmri_t1 > (r(mean) + 2*r(sd)) | ssrtfmri_t1 < (r(mean) - 2*r(sd))


ttest ssrtnr_t0, by(agetraingroup)
/*
/*REPLACEMENT VALUES*/
gen change_ssrt = ssrt_t1 - ssrt_t0
 
 pwcorr ssrt_t0 ssrt_t1 age change_ssrt ssrt_t2, sig star(.05) obs
 
 ttest ssrt_t0 == ssrt_t1 if group == 1
 ttest ssrt_t0 == ssrt_t1 if group == 2
 
 ttest ssrt_t0 == ssrtnr_t2 if group == 1
 ttest ssrt_t0 == ssrtnr_t2 if group == 2
 
 ttest ssrt_t1 == ssrtnr_t2 if group == 1
 ttest ssrt_t1 == ssrtnr_t2 if group == 2
 
 */


/*NON-REPLACEMENT VALUES*/
gen change_ssrtnr = ssrtnr_t1 - ssrtnr_t0
gen change_ssrtfmri = ssrtfmri_t1 - ssrtfmri_t0
 
pwcorr ssrtnr_t0 ssrtnr_t1 ssrtnr_t2 age change_ssrtnr, sig star(.05) obs
 
 ttest ssrtnr_t0 == ssrtnr_t1 if group == 1
 ttest ssrtnr_t0 == ssrtnr_t1 if group == 2
 
 ttest ssrtfmri_t0 == ssrtfmri_t1 if group == 1
 ttest ssrtfmri_t0 == ssrtfmri_t1 if group == 2
 
 ttest ssrtnr_t0 == ssrt_t2 if group == 1
 ttest ssrtnr_t0 == ssrt_t2 if group == 2

  ttest ssrtnr_t1 == ssrt_t2 if group == 1
 ttest ssrtnr_t1 == ssrt_t2 if group == 2



 ttest ssrtnr_t0, by(group)

 egen zssrtnr_t0 = std(ssrtnr_t0)
 egen zssrtnr_t1 = std(ssrtnr_t1)
 egen zssrtnr_t2 = std(ssrt_t2)

 egen zssrtfmri_t0 = std(ssrtfmri_t0)
 egen zssrtfmri_t1 = std(ssrtfmri_t1)
 
egen zssrt_t0 = std(ssrt_t0)
 egen zssrt_t1 = std(ssrt_t1)
 egen zssrt_t2 = std(ssrtnr_t2)
 
 gen Fssrtnr_t0 = ssrtnr_t0
 gen Fssrtnr_t1 = ssrtnr_t1
 gen Fssrtnr_t2 = ssrt_t2

 
******************************************/*AX-CPT*/*******************************************************


gen ay_bx_rt_t0 = ay_avg_rt_t0 - bx_avg_rt_t0
gen ay_bx_rt_t1 = ay_avg_rt_t1 - bx_avg_rt_t1
gen ay_bx_rt_t2 = ay_avg_rt_t2 - bx_avg_rt_t2


gen ay_bx_error_t0 = ay_error_t0 - bx_error_t0
gen ay_bx_error_t1 = ay_error_t1 - bx_error_t1
gen ay_bx_error_t2 = ay_error_t2 - bx_error_t2


egen zay_bx_rt_t0  = std(ay_bx_rt_t0)
egen zay_bx_rt_t1 = std(ay_bx_rt_t1)
egen zay_bx_rt_t2 = std(ay_bx_rt_t2)

egen zay_bx_error_t0  = std(ay_bx_error_t0)
egen zay_bx_error_t1 = std(ay_bx_error_t1)
egen zay_bx_error_t2 = std(ay_bx_error_t2)

gen ay_bx_t0 = (zay_bx_rt_t0 + zay_bx_error_t0)/2
gen ay_bx_t1 = (zay_bx_rt_t1 + zay_bx_error_t1)/2
gen ay_bx_t2 = (zay_bx_rt_t2 + zay_bx_error_t1)/2

/*compute z-scores based on each participant response*/
gen AX_CPT_meanrt_t0 = (ax_correct_avg_rt_axcpt_t0 + ay_correct_avg_rt_axcpt_t0 + bx_correct_avg_rt_axcpt_t0 + by_correct_avg_rt_axcpt_t0)/4

/*maybe rename axcpt variables with axcpt at the end to avoid any issues with this*/
unab avars: *_axcpt_t0
egen avar_sd=rowsd(`avars')

gen zAX_RT_t0 = (ax_correct_avg_rt_axcpt_t0 - AX_CPT_meanrt_t0)/avar_sd
gen zAY_RT_t0 = (ay_correct_avg_rt_axcpt_t0 - AX_CPT_meanrt_t0)/avar_sd
gen zBX_RT_t0 = (bx_correct_avg_rt_axcpt_t0 - AX_CPT_meanrt_t0)/avar_sd
gen zBY_RT_t0 = (by_correct_avg_rt_axcpt_t0 - AX_CPT_meanrt_t0)/avar_sd

gen PBI_RT_t0 = (zAY_RT_t0-zBX_RT_t0)/(zAY_RT_t0+zBX_RT_t0)
/* first replace any 0 errors - 1/2N*/
replace ay_error_t0=0.04167 if ay_error_t0==0
replace bx_error_t0=0.04167 if bx_error_t0==0

gen PBI_error_t0 = (ay_error_t0-bx_error_t0)/(ay_error_t0+bx_error_t0)

egen zPBI_RT_t0  = std(PBI_RT_t0)
egen zPBI_error_t0  = std(PBI_error_t0)
gen PBI_t0 = (zPBI_RT_t0 + zPBI_error_t0)/2
replace PBI_t0 =. if ay_correct_avg_rt_axcpt_t0 ==.

/*for t1 now*/
/*compute z-scores based on each participant response*/
gen AX_CPT_meanrt_t1 = (ax_correct_avg_rt_axcpt_t1 + ay_correct_avg_rt_axcpt_t1 + bx_correct_avg_rt_axcpt_t1 + by_correct_avg_rt_axcpt_t1)/4

unab bvars: *_axcpt_t1
egen bvar_sd=rowsd(`bvars')

gen zAX_RT_t1 = (ax_correct_avg_rt_axcpt_t1 - AX_CPT_meanrt_t1)/bvar_sd
gen zAY_RT_t1 = (ay_correct_avg_rt_axcpt_t1 - AX_CPT_meanrt_t1)/bvar_sd
gen zBX_RT_t1 = (bx_correct_avg_rt_axcpt_t1 - AX_CPT_meanrt_t1)/bvar_sd
gen zBY_RT_t1 = (by_correct_avg_rt_axcpt_t1 - AX_CPT_meanrt_t1)/bvar_sd

gen PBI_RT_t1 = (zAY_RT_t1-zBX_RT_t1)/(zAY_RT_t1+zBX_RT_t1)
/* first replace any 0 errors - 1/2N*/
replace ay_error_t1=0.04167 if ay_error_t1==0
replace bx_error_t1=0.04167 if bx_error_t1==0

gen PBI_error_t1 = (ay_error_t1-bx_error_t1)/(ay_error_t1+bx_error_t1)

egen zPBI_RT_t1  = std(PBI_RT_t1)
egen zPBI_error_t1 = std(PBI_error_t1)
gen PBI_t1 = (zPBI_RT_t1 + zPBI_error_t1)/2
replace PBI_t1 =. if ay_correct_avg_rt_axcpt_t1 ==.

/*for t2 now*/
gen AX_CPT_meanrt_t2 = (ax_correct_avg_rt_axcpt_t2 + ay_correct_avg_rt_axcpt_t2 + bx_correct_avg_rt_axcpt_t2 + by_correct_avg_rt_axcpt_t2)/4

unab cvars: *_axcpt_t2
egen cvar_sd=rowsd(`cvars')

gen zAX_RT_t2 = (ax_correct_avg_rt_axcpt_t2 - AX_CPT_meanrt_t2)/cvar_sd
gen zAY_RT_t2 = (ay_correct_avg_rt_axcpt_t2 - AX_CPT_meanrt_t2)/cvar_sd
gen zBX_RT_t2 = (bx_correct_avg_rt_axcpt_t2 - AX_CPT_meanrt_t2)/cvar_sd
gen zBY_RT_t2 = (by_correct_avg_rt_axcpt_t2 - AX_CPT_meanrt_t2)/cvar_sd

gen PBI_RT_t2 = (zAY_RT_t2-zBX_RT_t2)/(zAY_RT_t2+zBX_RT_t2)
/* first replace any 0 errors - 1/2N*/
replace ay_error_t2=0.04167 if ay_error_t2==0
replace bx_error_t2=0.04167 if bx_error_t2==0

gen PBI_error_t2 = (ay_error_t2-bx_error_t2)/(ay_error_t2+bx_error_t2)

egen zPBI_RT_t2  = std(PBI_RT_t2)
egen zPBI_error_t2 = std(PBI_error_t2)
gen PBI_t2 = (zPBI_RT_t2 + zPBI_error_t2)/2
replace PBI_t2 =. if ay_correct_avg_rt_axcpt_t2 ==.



/* REPORT*/

sum PBI_t0 
replace PBI_t0 = . if PBI_t0 > (r(mean) + 2*r(sd)) | PBI_t0 < (r(mean) - 2*r(sd))
sum PBI_t1
replace PBI_t1 = . if PBI_t1 > (r(mean) + 2*r(sd)) | PBI_t1 < (r(mean) - 2*r(sd))
sum PBI_t2
replace PBI_t2= . if PBI_t2 > (r(mean) + 2*r(sd)) | PBI_t2 < (r(mean) - 2*r(sd))


 ttest PBI_t0, by(group)
 ttest ay_bx_t0, by(group)

 ttest PBI_t0 == PBI_t1 if group == 1
 ttest PBI_t0 == PBI_t1 if group == 2
 
 ttest ay_bx_t0 == ay_bx_t1 if group == 1
 ttest ay_bx_t0 == ay_bx_t1 if group == 2
 
 gen change_aybx = ay_bx_t1 - ay_bx_t0
 gen change_PBI = PBI_t1 - PBI_t0
 
  ttest ay_avg_rt_t0 == bx_avg_rt_t0
 
  
pwcorr ay_bx_rt_t0 ay_bx_error_t0, sig star(.05) obs

by group, sort: pwcorr ay_bx_rt_t1 ay_bx_error_t1 , sig star(.05) obs

******************************************/*FLANKER*/*******************************************************

/*flanker inh*/

/* gen flank_inh_rt = flank_incongruent_rt - flank_congruent_rt */
gen flank_inh_rt_t0 = flank_incon_corr_avg_rt_t0 - flank_con_corr_avg_rt_t0
ttest flank_incon_corr_avg_rt_t0 = flank_con_corr_avg_rt_t0

egen zflank_incon_corr_avg_rt_t0  = std(flank_incon_corr_avg_rt_t0)
egen zflank_con_corr_avg_rt_t0  = std(flank_con_corr_avg_rt_t0)

egen zflank_incon_error_t0  = std(flank_incon_error_t0)
egen zflank_con_error_t0  = std(flank_con_error_t0)

gen flank_inh_incon_t0 = zflank_con_error_t0  + zflank_con_corr_avg_rt_t0
gen flank_inh_con_t0 = zflank_incon_error_t0  + zflank_incon_corr_avg_rt_t0



gen flank_inh_error_t0 = flank_incon_error_t0 - flank_con_error_t0
ttest flank_incon_error_t0 = flank_con_error_t0

egen zflank_inh_rt_t0  = std(flank_inh_rt_t0)
egen zflank_inh_error_t0  = std(flank_inh_error_t0)
gen flankerinh_t0 = (zflank_inh_rt_t0 + zflank_inh_error_t0)/2

gen flankernewinh_t0 = flank_inh_incon_t0+flank_inh_incon_t0


/*for t1 now*/

gen flank_inh_rt_t1 = flank_incon_corr_avg_rt_t1 - flank_con_corr_avg_rt_t1
ttest flank_incon_corr_avg_rt_t1 = flank_con_corr_avg_rt_t1

gen flank_inh_error_t1 = flank_incon_error_t1 - flank_con_error_t1
ttest flank_incon_error_t1 = flank_con_error_t1

egen zflank_inh_rt_t1  = std(flank_inh_rt_t1)
egen zflank_inh_error_t1  = std(flank_inh_error_t1)
gen flankerinh_t1 = (zflank_inh_rt_t1 + zflank_inh_error_t1)/2

pwcorr flank_inh_rt_t0 flank_inh_error_t0, sig star(.05) obs

by group, sort: pwcorr flank_inh_rt_t1 flank_inh_error_t1, sig star(.05) obs


/*flanker shift*/
/* t0*/

ttest flank_stay_rt_corr_t0 == flank_switch_rt_corr_t0
ttest flank_accuracy_stay_t0 == flank_accuracy_switch_t0

/* note we are using accuracy rather than error so have done stay - switch rather than switch - stay*/
gen flankerswitch_rt_t0 = flank_switch_rt_corr_t0 - flank_stay_rt_corr_t0
gen flankerswitch_error_t0 = flank_accuracy_stay_t0 - flank_accuracy_switch_t0

egen zflankerswitch_rt_t0  = std(flankerswitch_rt_t0)
egen zflankerswitch_error_t0  = std(flankerswitch_error_t0)
gen flankerswitch_t0 = (zflankerswitch_rt_t0 + zflankerswitch_error_t0)/2

/* t1*/

ttest flank_stay_rt_corr_t1 == flank_switch_rt_corr_t1
ttest flank_accuracy_stay_t1 == flank_accuracy_switch_t1

/* note we are using accuracy rather than error so have done stay - switch rather than switch - stay*/
gen flankerswitch_rt_t1 = flank_switch_rt_corr_t1 - flank_stay_rt_corr_t1
gen flankerswitch_error_t1 = flank_accuracy_stay_t1 - flank_accuracy_switch_t1

egen zflankerswitch_rt_t1 = std(flankerswitch_rt_t1)
egen zflankerswitch_error_t1  = std(flankerswitch_error_t1)
gen flankerswitch_t1 = (zflankerswitch_rt_t1 + zflankerswitch_error_t1)/2


pwcorr flankerswitch_rt_t0 flankerswitch_error_t0, sig star(.05) obs

by group, sort: pwcorr flankerswitch_rt_t1 flankerswitch_error_t1, sig star(.05) obs


/*REPORT */

sum flankerinh_t0 
replace flankerinh_t0 = . if flankerinh_t0 > (r(mean) + 2*r(sd)) | flankerinh_t0 < (r(mean) - 2*r(sd))
sum flankerinh_t1
replace flankerinh_t1 = . if flankerinh_t1 > (r(mean) + 2*r(sd)) | flankerinh_t1 < (r(mean) - 2*r(sd))

sum flankerswitch_t0 
replace flankerswitch_t0 = . if flankerswitch_t0 > (r(mean) + 2*r(sd)) | flankerswitch_t0 < (r(mean) - 2*r(sd))
sum flankerswitch_t1
replace flankerswitch_t1 = . if flankerswitch_t1 > (r(mean) + 2*r(sd)) | flankerswitch_t1 < (r(mean) - 2*r(sd))


 ttest flankerswitch_t0, by(group)
 ttest flankerinh_t0, by(group)
 
ttest flankerswitch_t0 == flankerswitch_t1 if group == 1
ttest flankerswitch_t0 == flankerswitch_t1 if group == 2
  
ttest flankerinh_t0 == flankerinh_t1 if group == 1
ttest flankerinh_t0 == flankerinh_t1 if group == 2

gen changeflankinh = flankerinh_t1 - flankerinh_t0
gen changeflankswitch = flankerswitch_t1 - flankerswitch_t0
  
  
pwcorr flankerinh_t0 flankerinh_t1 changeflankinh age, sig star(.05) obs

pwcorr flankerswitch_t0 flankerswitch_t1 changeflankswitch age, sig star(.05) obs


******************************************/*STROOP*/*******************************************************
gen stroop_rt_t0 = stroop_incon_corr_avg_rt_t0 - stroop_neu_corr_avg_rt_t0
 gen stroop_error_t0 = stroop_incon_error_t0 - stroop_neu_error_t0
 
 ttest stroop_incon_corr_avg_rt_t0 == stroop_neu_corr_avg_rt_t0 
 
 
egen zstroop_rt_t0  = std(stroop_rt_t0)
egen zstroop_error_t0 = std(stroop_error_t0)
gen stroop_t0 = (zstroop_rt_t0 + zstroop_error_t0)/2

 /*for t1*/
gen stroop_rt_t1 = stroop_incon_corr_avg_rt_t0 - stroop_neu_corr_avg_rt_t1
 gen stroop_error_t1 = stroop_incon_error_t1 - stroop_neu_error_t1
 
egen zstroop_rt_t1  = std(stroop_rt_t1)
egen zstroop_error_t1 = std(stroop_error_t1)
gen stroop_t1 = (zstroop_rt_t1 + zstroop_error_t1)/2


/*REPORT*/
sum stroop_t0
replace stroop_t0 = . if stroop_t0 > (r(mean) + 2*r(sd)) | stroop_t0 < (r(mean) - 2*r(sd))
sum stroop_t1
replace stroop_t1 = . if stroop_t1 > (r(mean) + 2*r(sd)) | stroop_t1 < (r(mean) - 2*r(sd))

ttest stroop_t0, by(group)

ttest stroop_t0 == stroop_t1 if group == 1
ttest stroop_t0 == stroop_t1 if group == 2

gen changestroop = stroop_t1 - stroop_t0

pwcorr changestroop stroop_t0 stroop_t1 age, sig star(.05) obs

pwcorr stroop_rt_t0 stroop_error_t0, sig star(.05) obs

by group, sort: pwcorr stroop_rt_t1 stroop_error_t1, sig star(.05) obs


******************************************/*N-BACK*/*******************************************************
gen onehitsrate_t0 = oneback_num_hits_t0/12
gen twohitsrate_t0 = twoback_num_hits_t0/12
ttest onehitsrate_t0 == twohitsrate_t0

gen oneFArate_t0 = oneback_num_fa_t0/28
gen twoFArate_t0 = twoback_num_fa_t0/28
ttest oneFArate_t0 == twoFArate_t0

ttest oneback_hits_av_rt_t0 == twoback_hits_av_rt_t0

/*generate new variable with hits and FA rate - replace where value is 0 method*/

gen psedooneFArate_t0 = oneFArate_t0
gen psedotwoFArate_t0 = twoFArate_t0
gen psedoonehitrate_t0 = onehitsrate_t0
gen psedotwohitrate_t0 = twohitsrate_t0

replace psedoonehitrate_t0= 1/24 if psedoonehitrate_t0== 0
replace psedoonehitrate_t0= 23/24 if psedoonehitrate_t0== 1
replace psedotwohitrate_t0= 1/24 if psedotwohitrate_t0== 0
replace psedotwohitrate_t0= 23/24 if psedotwohitrate_t0== 1

replace psedooneFArate_t0= 1/56 if psedoonehitrate_t0== 0
replace psedooneFArate_t0= 55/56 if psedoonehitrate_t0== 1
replace psedotwoFArate_t0= 1/56 if psedotwohitrate_t0== 0
replace psedotwoFArate_t0= 55/56 if psedotwohitrate_t0== 1

ttest zpsedoonehitrate == zpsedotwohitrate

ttest zpsedooneFArate == zpsedotwoFArate_t0

/*compute d-prime*/
/*egen zpsedoonehitrate_t0  = std(psedoonehitrate_t0)
egen zpsedotwohitrate_t0  = std(psedotwohitrate_t0)
egen zpsedooneFArate_t0  = std(psedooneFArate_t0)
egen zpsedotwoFArate_t0  = std(psedotwoFArate_t0)*/

gen dprimeONEBACK_t0 = psedoonehitrate_t0 - psedooneFArate_t0
gen dprimeTWOBACK_t0 = psedotwohitrate_t0 - psedotwoFArate_t0

ttest dprimeONEBACK_t0 == dprimeTWOBACK_t0


/*for t1 now */
gen onehitsrate_t1 = oneback_num_hits_t1/12
gen twohitsrate_t1 = twoback_num_hits_t1/12
ttest onehitsrate_t1 == twohitsrate_t1

gen oneFArate_t1 = oneback_num_fa_t1/28
gen twoFArate_t1 = twoback_num_fa_t1/28
ttest oneFArate_t1 == twoFArate_t1

ttest oneback_hits_av_rt_t1 == twoback_hits_av_rt_t1


gen psedooneFArate_t1 = oneFArate_t1
gen psedotwoFArate_t1 = twoFArate_t1
gen psedoonehitrate_t1 = onehitsrate_t1
gen psedotwohitrate_t1 = twohitsrate_t1

replace psedoonehitrate_t1= 1/24 if psedoonehitrate_t1== 0
replace psedoonehitrate_t1= 23/24 if psedoonehitrate_t1== 1
replace psedotwohitrate_t1= 1/24 if psedotwohitrate_t1== 0
replace psedotwohitrate_t1= 23/24 if psedotwohitrate_t1== 1

replace psedooneFArate_t1= 1/56 if psedoonehitrate_t1== 0
replace psedooneFArate_t1= 55/56 if psedoonehitrate_t1== 1
replace psedotwoFArate_t1= 1/56 if psedotwohitrate_t1== 0
replace psedotwoFArate_t1= 55/56 if psedotwohitrate_t1== 1

/*compute d-prime*/
/*egen zpsedoonehitrate_t1  = std(psedoonehitrate_t1)
egen zpsedotwohitrate_t1  = std(psedotwohitrate_t1)
egen zpsedooneFArate_t1  = std(psedooneFArate_t1)
egen zpsedotwoFArate_t1  = std(psedotwoFArate_t1)*/

gen dprimeONEBACK_t1 = psedoonehitrate_t1 - psedooneFArate_t1
gen dprimeTWOBACK_t1 = psedotwohitrate_t1 - psedotwoFArate_t1

ttest dprimeONEBACK_t1 == dprimeTWOBACK_t1


pwcorr dprimeONEBACK_t0 oneback_hits_av_rt_t0, sig star(.05) obs

by group, sort: pwcorr dprimeONEBACK_t1 oneback_hits_av_rt_t1, sig star(.05) obs

pwcorr dprimeTWOBACK_t0 twoback_hits_av_rt_t0, sig star(.05) obs

by group, sort: pwcorr dprimeTWOBACK_t1 twoback_hits_av_rt_t1, sig star(.05) obs


/*REPORT*/

sum dprimeONEBACK_t0
replace dprimeONEBACK_t0 = . if dprimeONEBACK_t0 > (r(mean) + 2*r(sd)) | dprimeONEBACK_t0 < (r(mean) - 2*r(sd))
sum dprimeONEBACK_t1
replace dprimeONEBACK_t1 = . if dprimeONEBACK_t1> (r(mean) + 2*r(sd)) | dprimeONEBACK_t1 < (r(mean) - 2*r(sd))

sum dprimeTWOBACK_t0
replace dprimeTWOBACK_t0 = . if dprimeTWOBACK_t0 > (r(mean) + 2*r(sd)) | dprimeTWOBACK_t0 < (r(mean) - 2*r(sd))
sum dprimeTWOBACK_t1
replace dprimeTWOBACK_t1 = . if dprimeTWOBACK_t1> (r(mean) + 2*r(sd)) | dprimeTWOBACK_t1 < (r(mean) - 2*r(sd))


ttest dprimeONEBACK_t0, by(group)
ttest dprimeTWOBACK_t0, by(group)


ttest dprimeONEBACK_t0 == dprimeONEBACK_t1 if group == 1
ttest dprimeONEBACK_t0 == dprimeONEBACK_t1 if group == 2

ttest dprimeTWOBACK_t0 == dprimeTWOBACK_t1 if group == 1
ttest dprimeTWOBACK_t0 == dprimeTWOBACK_t1 if group == 2

gen changedprimeTWO = dprimeTWOBACK_t1 - dprimeTWOBACK_t0
gen changedprimeONE = dprimeONEBACK_t1 - dprimeONEBACK_t0


pwcorr changedprimeONE dprimeONEBACK_t0 dprimeONEBACK_t1 age, sig star(.05) obs
pwcorr changedprimeTWO dprimeTWOBACK_t0 dprimeTWOBACK_t1 age, sig star(.05) obs
 
 /*COGFLEX*/
/* note we are using accuracy rather than error so have done stay - switch rather than switch - stay*/
ttest cf_stay_accuray_t0 == cf_switch_accuray_t0
ttest cf_switch_rt_t0 == cf_stay_rt_t0

egen zcf_stay_accuray_t0 = std(cf_stay_accuray_t0)
egen zcf_switch_accuray_t0 = std(cf_switch_accuray_t0)

gen cf_switch_acc_t0  = cf_stay_accuray_t0  - cf_switch_accuray_t0 
gen cf_switchrt_t0  = cf_switch_rt_t0- cf_stay_rt_t0

egen zcf_switch_acc_t0   = std(cf_switch_acc_t0)
egen zcf_switch_rt_t0  = std(cf_switchrt_t0)
gen cogflex_t0 = (zcf_switch_acc_t0 + zcf_switch_rt_t0)/2

/* t1 now*/
ttest cf_stay_accuray_t1 == cf_switch_accuray_t1
ttest cf_switch_rt_t1 == cf_stay_rt_t1

egen zcf_stay_accuray_t1 = std(cf_stay_accuray_t1)
egen zcf_switch_accuray_t1 = std(cf_switch_accuray_t1)

gen cf_switch_acc_t1  = cf_stay_accuray_t1  - cf_switch_accuray_t1 
gen cf_switchrt_t1  = cf_switch_rt_t1- cf_stay_rt_t1

egen zcf_switch_acc_t1   = std(cf_switch_acc_t1)
egen zcf_switch_rt_t1  = std(cf_switchrt_t1)
gen cogflex_t1 = (zcf_switch_acc_t1 + zcf_switch_rt_t1)/2


pwcorr cf_switch_acc_t0 cf_switch_rt_t0, sig star(.05) obs

by group, sort: pwcorr cf_switch_acc_t1 cf_switch_rt_t1, sig star(.05) obs


/* t2 now*/
ttest cf_stay_accuray_t2 == cf_switch_accuray_t2
ttest cf_switch_rt_t2 == cf_stay_rt_t2

egen zcf_stay_accuray_t2 = std(cf_stay_accuray_t2)
egen zcf_switch_accuray_t2 = std(cf_switch_accuray_t2)

gen cf_switch_acc_t2  = cf_stay_accuray_t2  - cf_switch_accuray_t2
gen cf_switchrt_t2  = cf_switch_rt_t2- cf_stay_rt_t2

egen zcf_switch_acc_t2   = std(cf_switch_acc_t2)
egen zcf_switch_rt_t2 = std(cf_switchrt_t2)
gen cogflex_t2 = (zcf_switch_acc_t2 + zcf_switch_rt_t2)/2


/*gen CF_t0 = cf_switchrt_t0*/

/*report*/

sum cogflex_t0
replace cogflex_t0 = . if cogflex_t0 > (r(mean) + 2*r(sd)) | cogflex_t0 < (r(mean) - 2*r(sd))
sum cogflex_t1
replace cogflex_t1 = . if cogflex_t1> (r(mean) + 2*r(sd)) | cogflex_t1 < (r(mean) - 2*r(sd))
sum cogflex_t2
replace cogflex_t2 = . if cogflex_t2> (r(mean) + 2*r(sd)) | cogflex_t2 < (r(mean) - 2*r(sd))


sum cf_switchrt_t0
replace cf_switchrt_t0 = . if cf_switchrt_t0 > (r(mean) + 2*r(sd)) | cf_switchrt_t0 < (r(mean) - 2*r(sd))
sum cf_switchrt_t1
replace cf_switchrt_t1 = . if cf_switchrt_t1 > (r(mean) + 2*r(sd)) | cf_switchrt_t1 < (r(mean) - 2*r(sd))
sum cf_switchrt_t2
replace cf_switchrt_t2 = . if cf_switchrt_t2 > (r(mean) + 2*r(sd)) | cf_switchrt_t2 < (r(mean) - 2*r(sd))


egen wcf_switch_rt_t0  = std(cf_switchrt_t0)
egen wcf_switch_rt_t1  = std(cf_switchrt_t1)
egen wcf_switch_rt_t2  = std(cf_switchrt_t2)

ttest cogflex_t0, by(group)
ttest cf_switchrt_t0, by(group)


ttest cogflex_t0 == cogflex_t1 if group == 1
ttest cogflex_t0 == cogflex_t1 if group == 2

ttest cf_switchrt_t0 == cf_switchrt_t1 if group == 1
ttest cf_switchrt_t0 == cf_switchrt_t1 if group == 2

gen changCFRT = cf_switchrt_t1 - cf_switchrt_t0
gen changCF = cogflex_t1 - cogflex_t0


****additional for Abi switch vs stay
/*convert accuracy to error first*/
gen cf_stay_error_t0 = 100- cf_stay_accuray_t0 
gen cf_switch_error_t0 = 100- cf_switch_accuray_t0
gen cf_stay_error_t1 = 100- cf_stay_accuray_t1 
gen cf_switch_error_t1 = 100- cf_switch_accuray_t1
gen cf_stay_error_t2 = 100- cf_stay_accuray_t2 
gen cf_switch_error_t2 = 100- cf_switch_accuray_t2




pwcorr changCFRT cf_switchrt_t0 cf_switchrt_t1 age, sig star(.05) obs
pwcorr changCF cogflex_t1 cogflex_t0 age, sig star(.05) obs

****************************CORSI*************************
sum corsi_max_wm_t0
replace corsi_max_wm_t0 = . if corsi_max_wm_t0 > (r(mean) + 2*r(sd)) | corsi_max_wm_t0 < (r(mean) - 2*r(sd))
sum corsi_max_wm_t1
replace corsi_max_wm_t1 = . if corsi_max_wm_t1 > (r(mean) + 2*r(sd)) | corsi_max_wm_t1  < (r(mean) - 2*r(sd))
sum corsi_max_wm_t2
replace corsi_max_wm_t2 = . if corsi_max_wm_t2 > (r(mean) + 2*r(sd)) | corsi_max_wm_t2  < (r(mean) - 2*r(sd))


ttest corsi_max_wm_t0 == corsi_max_wm_t1 if group == 1
ttest corsi_max_wm_t0 == corsi_max_wm_t1 if group == 2

gen changecorsi = corsi_max_wm_t1 - corsi_max_wm_t0

pwcorr corsi_max_wm_t0 corsi_acc_t0,sig star(.05) obs

pwcorr changcorsi corsi_max_wm_t1 corsi_max_wm_t0 age, sig star(.05) obs

egen zcorsi_max_wm_t0 = std(corsi_max_wm_t0)
egen zcorsi_max_wm_t1 = std(corsi_max_wm_t1)
egen zcorsi_max_wm_t2 = std(corsi_max_wm_t2)
 
 

pwcorr fit havardoxford_cortl_t0 calcarine_r_t0 caudate_r_t0 putamen_r_t0 thalamus_r_t0 ,sig star(.05) obs


sum havardoxford_cortl_t0
replace havardoxford_cortl_t0 = . if havardoxford_cortl_t0 > (r(mean) + 2*r(sd)) | havardoxford_cortl_t0 < (r(mean) - 2*r(sd))
sum havardoxford_cortl_t1
replace havardoxford_cortl_t1 = . if havardoxford_cortl_t1 > (r(mean) + 2*r(sd)) | havardoxford_cortl_t1 < (r(mean) - 2*r(sd))

sum presma_l_t0
replace presma_l_t0 = . if presma_l_t0 > (r(mean) + 2*r(sd)) | presma_l_t0  < (r(mean) - 2*r(sd))
sum presma_l_t1
replace presma_l_t1 = . if presma_l_t1 > (r(mean) + 2*r(sd)) | presma_l_t1 < (r(mean) - 2*r(sd))

sum caudate_r_t0
replace caudate_r_t0 = . if caudate_r_t0 > (r(mean) + 2*r(sd)) | caudate_r_t0  < (r(mean) - 2*r(sd))
sum caudate_r_t1
replace caudate_r_t1 = . if caudate_r_t1 > (r(mean) + 2*r(sd)) | caudate_r_t1 < (r(mean) - 2*r(sd))

sum calcarine_r_t0
replace calcarine_r_t0 = . if calcarine_r_t0 > (r(mean) + 2*r(sd)) | calcarine_r_t0  < (r(mean) - 2*r(sd))
sum calcarine_r_t1
replace calcarine_r_t1 = . if calcarine_r_t1 > (r(mean) + 2*r(sd)) | calcarine_r_t1 < (r(mean) - 2*r(sd))

sum putamen_r_t0
replace putamen_r_t0 = . if putamen_r_t0 > (r(mean) + 2*r(sd)) | putamen_r_t0 < (r(mean) - 2*r(sd))
sum putamen_r_t1
replace putamen_r_t1 = . if putamen_r_t1 > (r(mean) + 2*r(sd)) | putamen_r_t1 < (r(mean) - 2*r(sd))

sum thalamus_r_t0
replace thalamus_r_t0 = . if thalamus_r_t0 > (r(mean) + 2*r(sd)) | thalamus_r_t0 < (r(mean) - 2*r(sd))
sum thalamus_r_t1
replace thalamus_r_t1 = . if thalamus_r_t1 > (r(mean) + 2*r(sd)) | thalamus_r_t1 < (r(mean) - 2*r(sd))

sum inhibitionnet_t0
replace inhibitionnet_t0 = . if inhibitionnet_t0 > (r(mean) + 2*r(sd)) | inhibitionnet_t0 < (r(mean) - 2*r(sd))
sum inhibitionnet_t1
replace inhibitionnet_t1 = . if inhibitionnet_t1 > (r(mean) + 2*r(sd)) | inhibitionnet_t1 < (r(mean) - 2*r(sd))


sum havardoxford_cortl_go_t0
replace havardoxford_cortl_go_t0 = . if havardoxford_cortl_go_t0 > (r(mean) + 2*r(sd)) | havardoxford_cortl_go_t0 < (r(mean) - 2*r(sd))
sum havardoxford_cortl_go_t1
replace havardoxford_cortl_go_t1 = . if havardoxford_cortl_go_t1 > (r(mean) + 2*r(sd)) | havardoxford_cortl_go_t1 < (r(mean) - 2*r(sd))

sum havardoxford_cortl_stop_t0
replace havardoxford_cortl_stop_t0 = . if havardoxford_cortl_stop_t0 > (r(mean) + 2*r(sd)) | havardoxford_cortl_stop_t0 < (r(mean) - 2*r(sd))
sum havardoxford_cortl_stop_t1
replace havardoxford_cortl_stop_t1 = . if havardoxford_cortl_stop_t1 > (r(mean) + 2*r(sd)) | havardoxford_cortl_stop_t1 < (r(mean) - 2*r(sd))


sum inhibitionnet_go_t0
replace inhibitionnet_go_t0 = . if inhibitionnet_go_t0 > (r(mean) + 2*r(sd)) | inhibitionnet_go_t0 < (r(mean) - 2*r(sd))
sum inhibitionnet_go_t1
replace inhibitionnet_go_t1 = . if inhibitionnet_go_t1 > (r(mean) + 2*r(sd)) | inhibitionnet_go_t1 < (r(mean) - 2*r(sd))

sum inhibitionnet_stop_t0
replace inhibitionnet_stop_t0 = . if inhibitionnet_stop_t0 > (r(mean) + 2*r(sd)) | inhibitionnet_stop_t0 < (r(mean) - 2*r(sd))
sum inhibitionnet_stop_t1
replace inhibitionnet_stop_t1 = . if inhibitionnet_stop_t1 > (r(mean) + 2*r(sd)) | inhibitionnet_stop_t1 < (r(mean) - 2*r(sd))

 ttest havardoxford_cortl_go_t0 == havardoxford_cortl_go_t1 if group == 1
 ttest havardoxford_cortl_stop_t0 == havardoxford_cortl_stop_t1 if group == 1
 
 ttest havardoxford_cortl_go_t0 == havardoxford_cortl_go_t1 if group == 2
 ttest havardoxford_cortl_stop_t0 == havardoxford_cortl_stop_t1 if group == 2

 ttest inhibitionnet_go_t0 == inhibitionnet_go_t1 if group == 1
 ttest inhibitionnet_stop_t0 == inhibitionnet_stop_t1 if group == 1
 
 
 ttest inhibitionnet_go_t0 == inhibitionnet_go_t1 if group == 2
 ttest inhibitionnet_stop_t0 == inhibitionnet_stop_t1 if group == 2

 
 
 ttest inhibitionnet_t0 == inhibitionnet_t1 if group == 1
 ttest inhibitionnet_t0 == inhibitionnet_t1 if group == 2
 
 ttest havardoxford_cortl_t0 == havardoxford_cortl_t1 if group == 1
 ttest havardoxford_cortl_t0 == havardoxford_cortl_t1 if group == 2
 
 ttest presma_l_t0 == presma_l_t1 if group == 1
 ttest presma_l_t0 == presma_l_t1 if group == 2
 
 ttest thalamus_r_t0 == thalamus_r_t1 if group == 1
 ttest thalamus_r_t0 == thalamus_r_t1 if group == 2
 
 ttest putamen_r_t0 == putamen_r_t1 if group == 1
 ttest putamen_r_t0 == putamen_r_t1 if group == 2
 
 ttest caudate_r_t0 == caudate_r_t1 if group == 1
 ttest caudate_r_t0 == caudate_r_t1 if group == 2
 
 ttest calcarine_r_t0 == calcarine_r_t1 if group == 1
 ttest calcarine_r_t0 == calcarine_r_t1 if group == 2
 

 gen changehavardox = havardoxford_cortl_t1 - havardoxford_cortl_t0
 
 gen changehavardox_stop = havardoxford_cortl_stop_t1 - havardoxford_cortl_stop_t0
  gen changehavardox_go = havardoxford_cortl_go_t1 - havardoxford_cortl_go_t0
 
  pwcorr changehavardox changehavardox_stop changehavardox_go change_ssrtnr change_ssrtfmri group,sig star(.05) obs
  
 
 pwcorr inhibitionnet_t0 havardoxford_cortl_t0 ssrtfmri_t0 ssrtnr_t0 change_ssrtnr change_ssrtfmri ,sig star(.05) obs
 
 
 /* proactive/reactive */
 
sum havardoxford_cortl_proactive_t0
replace havardoxford_cortl_proactive_t0 = . if havardoxford_cortl_proactive_t0 > (r(mean) + 2*r(sd)) | havardoxford_cortl_proactive_t0 < (r(mean) - 2*r(sd))
sum havardoxford_cortl_proactive_t1
replace havardoxford_cortl_proactive_t1 = . if havardoxford_cortl_proactive_t1 > (r(mean) + 2*r(sd)) | havardoxford_cortl_proactive_t1 < (r(mean) - 2*r(sd))

sum presma_l_proactive_t0
replace presma_l_proactive_t0 = . if presma_l_proactive_t0 > (r(mean) + 2*r(sd)) | presma_l_proactive_t0  < (r(mean) - 2*r(sd))
sum presma_l_proactive_t1
replace presma_l_proactive_t1 = . if presma_l_proactive_t1 > (r(mean) + 2*r(sd)) | presma_l_proactive_t1 < (r(mean) - 2*r(sd))

sum caudate_r_proactive_t0
replace caudate_r_proactive_t0 = . if caudate_r_proactive_t0 > (r(mean) + 2*r(sd)) | caudate_r_proactive_t0  < (r(mean) - 2*r(sd))
sum caudate_r_proactive_t1
replace caudate_r_proactive_t1 = . if caudate_r_proactive_t1 > (r(mean) + 2*r(sd)) | caudate_r_proactive_t1 < (r(mean) - 2*r(sd))

sum calcarine_r_proactive_t0
replace calcarine_r_proactive_t0 = . if calcarine_r_proactive_t0 > (r(mean) + 2*r(sd)) | calcarine_r_proactive_t0  < (r(mean) - 2*r(sd))
sum calcarine_r_proactive_t1
replace calcarine_r_proactive_t1 = . if calcarine_r_proactive_t1 > (r(mean) + 2*r(sd)) | calcarine_r_proactive_t1 < (r(mean) - 2*r(sd))

sum putamen_r_proactive_t0
replace putamen_r_proactive_t0 = . if putamen_r_proactive_t0 > (r(mean) + 2*r(sd)) | putamen_r_proactive_t0 < (r(mean) - 2*r(sd))
sum putamen_r_proactive_t1
replace putamen_r_proactive_t1 = . if putamen_r_proactive_t1 > (r(mean) + 2*r(sd)) | putamen_r_proactive_t1 < (r(mean) - 2*r(sd))

sum thalamus_r_proactive_t0
replace thalamus_r_proactive_t0 = . if thalamus_r_proactive_t0 > (r(mean) + 2*r(sd)) | thalamus_r_proactive_t0 < (r(mean) - 2*r(sd))
sum thalamus_r_proactive_t1
replace thalamus_r_proactive_t1 = . if thalamus_r_proactive_t1 > (r(mean) + 2*r(sd)) | thalamus_r_proactive_t1 < (r(mean) - 2*r(sd))

sum inhibitionnet_proactive_t0
replace inhibitionnet_proactive_t0 = . if inhibitionnet_proactive_t0 > (r(mean) + 2*r(sd)) | inhibitionnet_proactive_t0 < (r(mean) - 2*r(sd))
sum inhibitionnet_proactive_t1
replace inhibitionnet_proactive_t1 = . if inhibitionnet_proactive_t1 > (r(mean) + 2*r(sd)) | inhibitionnet_proactive_t1 < (r(mean) - 2*r(sd))

sum thalamus_l_proactive_t0
replace thalamus_l_proactive_t0 = . if thalamus_l_proactive_t0 > (r(mean) + 2*r(sd)) | thalamus_l_proactive_t0 < (r(mean) - 2*r(sd))
sum thalamus_l_proactive_t1
replace inhibitionnet_proactive_t1 = . if thalamus_l_proactive_t1 > (r(mean) + 2*r(sd)) | thalamus_l_proactive_t1 < (r(mean) - 2*r(sd))

sum putamen_l_proactive_t0
replace putamen_l_proactive_t0 = . if putamen_l_proactive_t0 > (r(mean) + 2*r(sd)) | putamen_l_proactive_t0 < (r(mean) - 2*r(sd))
sum putamen_l_proactive_t1
replace putamen_l_proactive_t1 = . if putamen_l_proactive_t1 > (r(mean) + 2*r(sd)) | putamen_l_proactive_t1 < (r(mean) - 2*r(sd))


 
 ttest inhibitionnet_proactive_t0 == inhibitionnet_proactive_t1 if group == 1
 ttest inhibitionnet_proactive_t0 == inhibitionnet_proactive_t1 if group == 2
 
 ttest havardoxford_cortl_proactive_t0 == havardoxford_cortl_proactive_t1 if group == 1
 ttest havardoxford_cortl_proactive_t0 == havardoxford_cortl_proactive_t1 if group == 2
 
 ttest presma_l_proactive_t0 == presma_l_proactive_t1 if group == 1
 ttest presma_l_proactive_t0 == presma_l_proactive_t1 if group == 2
 
 ttest thalamus_r_proactive_t0 == thalamus_r_proactive_t1 if group == 1
 ttest thalamus_r_proactive_t0 == thalamus_r_proactive_t1 if group == 2
 
 ttest putamen_r_proactive_t0 == putamen_r_proactive_t1 if group == 1
 ttest putamen_r_proactive_t0 == putamen_r_proactive_t1 if group == 2
 
 ttest caudate_r_proactive_t0 == caudate_r_proactive_t1 if group == 1
 ttest caudate_r_proactive_t0 == caudate_r_proactive_t1 if group == 2
 
 ttest calcarine_r_proactive_t0 == calcarine_r_proactive_t1 if group == 1
 ttest calcarine_r_proactive_t0 == calcarine_r_proactive_t1 if group == 2
 
 
 ttest thalamus_l_proactive_t0 == thalamus_l_proactive_t1 if group == 1
 ttest thalamus_l_proactive_t0 == thalamus_l_proactive_t1 if group == 2
 
 ttest putamen_l_proactive_t0 == putamen_l_proactive_t1 if group == 1
 ttest putamen_l_proactive_t0 == putamen_l_proactive_t1 if group == 2
 
 

 
 /*reactive now */
 
sum havardoxford_cortl_reactive_t0
replace havardoxford_cortl_reactive_t0 = . if havardoxford_cortl_reactive_t0 > (r(mean) + 2*r(sd)) | havardoxford_cortl_reactive_t0 < (r(mean) - 2*r(sd))
sum havardoxford_cortl_reactive_t1
replace havardoxford_cortl_reactive_t1 = . if havardoxford_cortl_reactive_t1 > (r(mean) + 2*r(sd)) | havardoxford_cortl_reactive_t1 < (r(mean) - 2*r(sd))

sum presma_l_reactive_t0
replace presma_l_reactive_t0 = . if presma_l_reactive_t0 > (r(mean) + 2*r(sd)) | presma_l_reactive_t0  < (r(mean) - 2*r(sd))
sum presma_l_reactive_t1
replace presma_l_reactive_t1 = . if presma_l_reactive_t1 > (r(mean) + 2*r(sd)) | presma_l_reactive_t1 < (r(mean) - 2*r(sd))

sum caudate_r_reactive_t0
replace caudate_r_reactive_t0 = . if caudate_r_reactive_t0 > (r(mean) + 2*r(sd)) | caudate_r_reactive_t0  < (r(mean) - 2*r(sd))
sum caudate_r_reactive_t1
replace caudate_r_reactive_t1 = . if caudate_r_reactive_t1 > (r(mean) + 2*r(sd)) | caudate_r_reactive_t1 < (r(mean) - 2*r(sd))

sum calcarine_r_reactive_t0
replace calcarine_r_reactive_t0 = . if calcarine_r_reactive_t0 > (r(mean) + 2*r(sd)) | calcarine_r_reactive_t0  < (r(mean) - 2*r(sd))
sum calcarine_r_reactive_t1
replace calcarine_r_reactive_t1 = . if calcarine_r_reactive_t1 > (r(mean) + 2*r(sd)) | calcarine_r_reactive_t1 < (r(mean) - 2*r(sd))

sum putamen_r_reactive_t0
replace putamen_r_reactive_t0 = . if putamen_r_reactive_t0 > (r(mean) + 2*r(sd)) | putamen_r_reactive_t0 < (r(mean) - 2*r(sd))
sum putamen_r_reactive_t1
replace putamen_r_reactive_t1 = . if putamen_r_reactive_t1 > (r(mean) + 2*r(sd)) | putamen_r_reactive_t1 < (r(mean) - 2*r(sd))

sum thalamus_r_reactive_t0
replace thalamus_r_reactive_t0 = . if thalamus_r_reactive_t0 > (r(mean) + 2*r(sd)) | thalamus_r_reactive_t0 < (r(mean) - 2*r(sd))
sum thalamus_r_reactive_t1
replace thalamus_r_reactive_t1 = . if thalamus_r_reactive_t1 > (r(mean) + 2*r(sd)) | thalamus_r_reactive_t1 < (r(mean) - 2*r(sd))

sum inhibitionnet_reactive_t0
replace inhibitionnet_reactive_t0 = . if inhibitionnet_reactive_t0 > (r(mean) + 2*r(sd)) | inhibitionnet_reactive_t0 < (r(mean) - 2*r(sd))
sum inhibitionnet_reactive_t1
replace inhibitionnet_reactive_t1 = . if inhibitionnet_reactive_t1 > (r(mean) + 2*r(sd)) | inhibitionnet_reactive_t1 < (r(mean) - 2*r(sd))

sum thalamus_l_reactive_t0
replace thalamus_l_reactive_t0 = . if thalamus_l_reactive_t0 > (r(mean) + 2*r(sd)) | thalamus_l_reactive_t0 < (r(mean) - 2*r(sd))
sum thalamus_l_reactive_t1
replace inhibitionnet_reactive_t1 = . if thalamus_l_reactive_t1 > (r(mean) + 2*r(sd)) | thalamus_l_reactive_t1 < (r(mean) - 2*r(sd))

sum putamen_l_reactive_t0
replace putamen_l_reactive_t0 = . if putamen_l_reactive_t0 > (r(mean) + 2*r(sd)) | putamen_l_reactive_t0 < (r(mean) - 2*r(sd))
sum putamen_l_reactive_t1
replace putamen_l_reactive_t1 = . if putamen_l_reactive_t1 > (r(mean) + 2*r(sd)) | putamen_l_reactive_t1 < (r(mean) - 2*r(sd))


 
 ttest inhibitionnet_reactive_t0 == inhibitionnet_reactive_t1 if group == 1
 ttest inhibitionnet_reactive_t0 == inhibitionnet_reactive_t1 if group == 2
 
 ttest havardoxford_cortl_reactive_t0 == havardoxford_cortl_reactive_t1 if group == 1
 ttest havardoxford_cortl_reactive_t0 == havardoxford_cortl_reactive_t1 if group == 2
 
 ttest presma_l_reactive_t0 == presma_l_reactive_t1 if group == 1
 ttest presma_l_reactive_t0 == presma_l_reactive_t1 if group == 2
 
 ttest thalamus_r_reactive_t0 == thalamus_r_reactive_t1 if group == 1
 ttest thalamus_r_reactive_t0 == thalamus_r_reactive_t1 if group == 2
 
 ttest putamen_r_reactive_t0 == putamen_r_reactive_t1 if group == 1
 ttest putamen_r_reactive_t0 == putamen_r_reactive_t1 if group == 2
 
 ttest caudate_r_reactive_t0 == caudate_r_reactive_t1 if group == 1
 ttest caudate_r_reactive_t0 == caudate_r_reactive_t1 if group == 2
 
 ttest calcarine_r_reactive_t0 == calcarine_r_reactive_t1 if group == 1
 ttest calcarine_r_reactive_t0 == calcarine_r_reactive_t1 if group == 2
 
 ttest thalamus_l_reactive_t0 == thalamus_l_reactive_t1 if group == 1
 ttest thalamus_l_reactive_t0 == thalamus_l_reactive_t1 if group == 2
 
 ttest putamen_l_reactive_t0 == putamen_l_reactive_t1 if group == 1
 ttest putamen_l_reactive_t0 == putamen_l_reactive_t1 if group == 2
 
 
  
  
 pwcorr inhibitionnet_proactive_t0 havardoxford_cortl_proactive_t0 inhibitionnet_reactive_t0 havardoxford_cortl_reactive_t0 ssrtfmri_t0 ssrtnr_t0 change_ssrtnr change_ssrtfmri PBI_t0 ay_bx_t0 ,sig star(.05) obs
  
 /*recode some variables for CFA factor analysis - so positive reflects improvement*/
 
 gen REzssrtnr_t0 = -zssrtnr_t0
 gen REzssrtnr_t1 = -zssrtnr_t1
 gen REzssrtnr_t2 = -zssrtnr_t2
 gen REflankerswitch_t0 = -flankerswitch_t0
 gen REflankerswitch_t1 = -flankerswitch_t1
 gen REflankerinh_t0 = -flankerinh_t0
 gen REflankerinh_t1 = -flankerinh_t1
 gen REcogflex_t0 = -cogflex_t0
 gen REcogflex_t1 = -cogflex_t1
 gen REcogflex_t2 = -cogflex_t2
 gen RE_cf_switch_t0 = -wcf_switch_rt_t0
 gen RE_cf_switch_t1 = -wcf_switch_rt_t1
 gen RE_cf_switch_t2 = -wcf_switch_rt_t2
 gen REstroop_t0 = -stroop_t0
 gen REstroop_t1 = -stroop_t1
 
 
 
 /*some SDQ measures*/
 
sum t0_total_sdq
replace t0_total_sdq = . if t0_total_sdq > (r(mean) + 2*r(sd)) | t0_total_sdq < (r(mean) - 2*r(sd))
sum t1_total_sdq
replace t1_total_sdq = . if t1_total_sdq > (r(mean) + 2*r(sd)) | t1_total_sdq < (r(mean) - 2*r(sd))
sum t2_total_sdq
replace t2_total_sdq = . if t2_total_sdq > (r(mean) + 2*r(sd)) | t2_total_sdq < (r(mean) - 2*r(sd))

replace t0_hy_sdq = . if t0_hy_sdq > (r(mean) + 2*r(sd)) | t0_hy_sdq < (r(mean) - 2*r(sd))
sum t1_hy_sdq
replace t1_hy_sdq = . if t1_hy_sdq > (r(mean) + 2*r(sd)) | t1_hy_sdq < (r(mean) - 2*r(sd))
sum t2_hy_sdq
replace t2_hy_sdq = . if t2_hy_sdq > (r(mean) + 2*r(sd)) | t2_hy_sdq < (r(mean) - 2*r(sd))

sum t0_ext_sdq 
replace t0_ext_sdq = . if t0_hy_sdq > (r(mean) + 2*r(sd)) | t0_ext_sdq < (r(mean) - 2*r(sd))
sum t1_ext_sdq
replace t1_ext_sdq = . if t1_ext_sdq > (r(mean) + 2*r(sd)) | t1_ext_sdq < (r(mean) - 2*r(sd))
sum t2_ext_sdq
replace t2_ext_sdq = . if t2_ext_sdq > (r(mean) + 2*r(sd)) | t2_ext_sdq < (r(mean) - 2*r(sd))

 

 ttest  t0_hy_sdq ==  t1_hy_sdq if group == 1
 ttest  t0_hy_sdq ==  t1_hy_sdq if group == 2
 
 ttest  t0_ext_sdq ==  t1_ext_sdq if group == 1
 ttest  t0_ext_sdq ==  t1_ext_sdq if group == 2
 
  ttest t0_total_sdq ==  t1_total_sdq if group == 1
 ttest  t0_total_sdq ==  t1_total_sdq if group == 2
 
 /*adhd*/ 
 
sum  t0_adhd_in_casi
replace  t0_adhd_in_casi= . if t0_hy_sdq > (r(mean) + 2*r(sd)) |  t0_adhd_in_casi< (r(mean) - 2*r(sd))
sum t1_adhd_in_casi
replace  t1_adhd_in_casi= . if  t1_adhd_in_casi> (r(mean) + 2*r(sd)) |  t1_adhd_in_casi< (r(mean) - 2*r(sd))
sum t2_adhd_in_casi
replace  t2_adhd_in_casi= . if  t2_adhd_in_casi> (r(mean) + 2*r(sd)) |  t2_adhd_in_casi< (r(mean) - 2*r(sd))

sum  t0_adhd_hyp_casi
replace  t0_adhd_hyp_casi= . if t0_hy_sdq > (r(mean) + 2*r(sd)) |  t0_adhd_hyp_casi< (r(mean) - 2*r(sd))
sum t1_adhd_in_casi
replace  t1_adhd_hyp_casi= . if  t1_adhd_hyp_casi> (r(mean) + 2*r(sd)) |  t1_adhd_hyp_casi< (r(mean) - 2*r(sd))
sum t2_adhd_in_casi
replace  t2_adhd_hyp_casi= . if  t2_adhd_hyp_casi> (r(mean) + 2*r(sd)) |  t2_adhd_hyp_casi< (r(mean) - 2*r(sd))

sum  t0_adhd_tot_casi
replace  t0_adhd_tot_casi= . if t0_hy_sdq > (r(mean) + 2*r(sd)) |  t0_adhd_tot_casi< (r(mean) - 2*r(sd))
sum t1_adhd_in_casi
replace  t1_adhd_tot_casi= . if  t1_adhd_tot_casi> (r(mean) + 2*r(sd)) |  t1_adhd_tot_casi< (r(mean) - 2*r(sd))
sum t2_adhd_in_casi
replace  t2_adhd_tot_casi= . if  t2_adhd_tot_casi> (r(mean) + 2*r(sd)) |  t2_adhd_tot_casi< (r(mean) - 2*r(sd))
 
 ttest t0_adhd_in_casi==  t1_adhd_in_casi if group == 1
 ttest  t0_adhd_in_casi ==  t1_adhd_in_casi if group == 2

 ttest t0_adhd_hyp_casi==  t1_adhd_hyp_casi if group == 1
 ttest  t0_adhd_hyp_casi ==  t1_adhd_hyp_casi if group == 2
 
 gen adhd_in_casi_t0 = t0_adhd_in_casi
 gen adhd_in_casi_t1 = t1_adhd_in_casi
 gen adhd_in_casi_t2 = t2_adhd_in_casi
 
 gen adhd_hyp_casi_t0 = t0_adhd_hyp_casi
 gen adhd_hyp_casi_t1 = t1_adhd_hyp_casi
 gen adhd_hyp_casi_t2 = t2_adhd_hyp_casi
 
 gen adhd_tot_casi_t0 = t0_adhd_tot_casi
 gen adhd_tot_casi_t1 = t1_adhd_tot_casi
 gen adhd_tot_casi_t2 = t2_adhd_tot_casi
 
 gen hy_sdq_t0 = t0_hy_sdq
 gen hy_sdq_t1 = t1_hy_sdq
 gen hy_sdq_t2 = t2_hy_sdq
 
 gen ext_sdq_t0 = t0_ext_sdq
 gen ext_sdq_t1 = t1_ext_sdq
 gen ext_sdq_t2 = t2_ext_sdq
 
 gen total_sdq_t0 = t0_total_sdq
 gen total_sdq_t1 = t1_total_sdq
 gen total_sdq_t2 = t2_total_sdq
 
 
/*acad scores*/

gen compacad_t0 = english_score_t0  + maths_score_t0
/*replace compacad_t0 = english_score_t0 if maths_score_t0 == .
replace compacad_t0 = maths_score_t0 if english_score_t0 == . */

gen compacad_t1 = all_english_t1  + all_maths_t1
/*replace compacad_t1 = all_english_t1  if all_maths_t1 == .
replace compacad_t1 = all_maths_t1 if all_english_t1  == .*/


sum english_score_t0
replace english_score_t0 = . if english_score_t0 > (r(mean) + 2*r(sd)) | english_score_t0 < (r(mean) - 2*r(sd))
sum all_english_t1
replace all_english_t1 = . if all_english_t1 > (r(mean) + 2*r(sd)) | all_english_t1 < (r(mean) - 2*r(sd))

sum maths_score_t0
replace maths_score_t0 = . if maths_score_t0 > (r(mean) + 2*r(sd)) | maths_score_t0 < (r(mean) - 2*r(sd))
sum all_maths_t1
replace all_maths_t1 = . if all_maths_t1 > (r(mean) + 2*r(sd)) | all_maths_t1 < (r(mean) - 2*r(sd))

sum compacad_t0
replace compacad_t0 = . if compacad_t0 > (r(mean) + 2*r(sd)) | compacad_t0 < (r(mean) - 2*r(sd))
sum compacad_t1
replace compacad_t1 = . if compacad_t1 > (r(mean) + 2*r(sd)) | compacad_t1 < (r(mean) - 2*r(sd))



 ttest  english_score_t0 ==  all_english_t1 if group == 1
 ttest  english_score_t0 ==  all_english_t1 if group == 2
 
 ttest maths_score_t0 ==  all_maths_t1 if group == 1
 ttest  maths_score_t0 ==  all_maths_t1 if group == 2
 


gen engchange = all_english_t1 - english_score_t0
 
gen Ncompacad_t0 = english_score_t0  + maths_score_t0
gen Ncompacad_t1 = all_english_t1  + all_maths_t1


egen zNcompacad_t0 = std(Ncompacad_t0)
egen zNcompacad_t1 = std(Ncompacad_t1)
egen zacadtime_t1 =std(acadtime_t1)

 
/*gen baseline academic performance to merge with long file*/ 
 
 gen baselineacad1 = Ncompacad_t0
 gen baselineacad2 = compacad_t0
 
gen changenacad = Ncompacad_t1-Ncompacad_t0

 ttest Ncompacad_t0 ==  Ncompacad_t1 if group == 1
 ttest  Ncompacad_t0 ==  Ncompacad_t1 if group == 2
 
pwcorr changenacad changegort change_PBI ,sig star(.05) obs

sum newinvarea
replace newinvarea = . if newinvarea > (r(mean) + 2*r(sd)) | newinvarea < (r(mean) - 2*r(sd))

 pwcorr flankerswitch_t0 cogflex_t0 age, sig star(.05) obs
 

gen changetotADHD =  adhd_tot_casi_t1 - adhd_tot_casi_t0
gen changeinhibitionnet = inhibitionnet_t1 - inhibitionnet_t0
by group, sort: pwcorr  Ncompacad_t0 changenacad change_ssrtnr  lankerswitch_t0 flankerinh_t0 ssrtnr_t0 PBI_t0 stroop_t0 dprimeONEBACK_t0 dprimeTWOBACK_t0 corsi_max_wm_t0 cogflex_t0 change_PBI changetotADHD changeinhibitionnet changegort change_PBI ,sig star(.05) obs
 
 
gen x = Ncompacad_t0
replace x = Ncompacad_t1 if Ncompacad_t0 == .
drop if missing(x)
 
pwcorr Ncompacad_t0 changenacad change_ssrtnr  change_PBI ,sig star(.05) obs
pwcorr  inhibitionnet_t0 zssrtfmri_t0 meango_corrrt,sig star(.05) obs
 
twoway (scatter changenacad Ncompacad_t0, sort), by(group)
twoway (scatter change_ssrtnr Ncompacad_t0, sort), by(group)
twoway (scatter change_PBI Ncompacad_t0, sort), by(group)
 
 
 export delimited p_id baselineacad1 baselineacad2 using "C:\Users\keertana\Desktop\MAINSTUDY\T1\final output\BaselineAcad.csv", replace
  export delimited p_id group Ncompacad_t0 Ncompacad_t1 using "C:\Users\keertana\Documents\Latent Score Modelling\final output\DUMMY.csv", replace
 
 /*all data*/
 

 
 export delimited using "C:\Users\keertana\Desktop\MAINSTUDY\T1\final output\FULLEF.csv", replace
******************************************/*PRE_POST TRAINING*/*******************************************************
 
 /*keep p_id group age inhibitionnet_t0 inhibitionnet_t1 PBI_t0 PBI_t1 ay_bx_t0 ay_bx_t1 ssrtnr_t0 ssrtnr_t1 flankerswitch_t0 flankerswitch_t1 flankerinh_t0 flankerinh_t1 stroop_t0 stroop_t1 dprimeONEBACK_t0 dprimeONEBACK_t1 dprimeTWOBACK_t0 dprimeTWOBACK_t1 cogflex_t0 cogflex_t1 age corsi_acc_t0 corsi_acc_t1 corsi_max_wm_t1 corsi_max_wm_t0 cf_switchrt_t0 cf_switchrt_t1 presma_l_t0 presma_l_t1 havardoxford_cortl_t0 havardoxford_cortl_t1 calcarine_r_t0 calcarine_r_t1 caudate_r_t0 caudate_r_t1 putamen_r_t0 putamen_r_t1 thalamus_r_t0  thalamus_r_t1 
  */
  
 /***FINAL SSRT-FUNC file - outliers not excluded***/
 
 /*keep p_id group age inhibitionnet_t0 inhibitionnet_t1 Fssrtnr_t0 Fssrtnr_t1 Fssrtnr_t2 meango_corrrtnr_t0 meango_corrrtnr_t1 meango_corrrtnr_t2 trainingslope totalsessions
 */
 /*with mental health here t0/t1*/
 keep p_id group age adhd_in_casi_t0 adhd_in_casi_t1 adhd_in_casi_t2 adhd_hyp_casi_t0 adhd_hyp_casi_t1 adhd_hyp_casi_t2 adhd_tot_casi_t0 adhd_tot_casi_t1 adhd_tot_casi_t2 inhibitionnet_t0 inhibitionnet_t1 Fssrtnr_t0 Fssrtnr_t1 Fssrtnr_t2 meango_corrrtnr_t0 meango_corrrtnr_t1 meango_corrrtnr_t2 trainingslope totalsessions
 
 
 keep p_id group age flankerswitch_t flankerinh_t Fssrtnr_t PBI_t stroop_t dprimeONEBACK_t dprimeTWOBACK_t corsi_max_wm_t cogflex_t trainingslope totalsessions
 
  
/*reshape long meango_corrrtnr_t Fssrtnr_t inhibitionnet_t , i(p_id) j(session)*/
reshape long adhd_in_casi_t adhd_hyp_casi_t adhd_tot_casi_t meango_corrrtnr_t Fssrtnr_t inhibitionnet_t , i(p_id) j(session)
drop if missing(group)
export delimited using "C:\Users\keertana\Documents\Latent Score Modelling\SSRT-FUNC-outliersremoved.csv", replace
export delimited using "C:\Users\keertana\Documents\Latent Score Modelling\SSRT-FUNC-MentalHealth-outliersremovedTEST.csv", replace
export delimited using "C:\Users\keertana\Documents\Latent Score Modelling\SSRT-FUNC-MentalHealth-outliersremovedT2.csv", replace

*************************/*T0 EF FILE FOR FACTOR ANALYSIS - CREATE FOR R*/********************************************

/*pca flankerswitch_t0 flankerinh_t0 ssrt_t0 PBI_t0 stroop_t0 dprimeONEBACK_t0 dprimeTWOBACK_t0 corsi_max_wm_t0 cogflex_t0, mineigen(1)
pca flankerswitch_t0 flankerinh_t0 ssrt_t0 PBI_t0 stroop_t0 dprimeONEBACK_t0 dprimeTWOBACK_t0 corsi_max_wm_t0 cogflex_t0, mineigen(1) */
/*pca flankerswitch_t0 flankerinh_t0 ssrt_t0 PBI_t0 stroop_t0 dprimeONEBACK_t0 dprimeTWOBACK_t0 corsi_max_wm_t0  cf_switchrt_t0, mineigen(1)*/

/*export brain and SSRT*/
export delimited p_id group zacadtime_t1 compacad_t0 compacad_t1 using "C:\Users\keertana\Documents\Latent Score Modelling\ACADwithoutremovingoutliers.csv", replace
export delimited p_id cov_acad Fssrtnr_t0 Fssrtnr_t1 Fssrtnr_t2 meango_corrrtnr_t0 meango_corrrtnr_t1 PBI_t0 PBI_t1 zacadtime_t1 zNcompacad_t0 zNcompacad_t1 t0_adhd_in_casi t1_adhd_in_casi t0_adhd_hyp_casi t1_adhd_hyp_casi t0_adhd_tot_casi t1_adhd_tot_casi stroop_t0 stroop_t1 Ncompacad_t0 Ncompacad_t1 compacad_t0 compacad_t1 english_score_t0 all_english_t1 REzssrtnr_t0 REzssrtnr_t1 maths_score_t0 all_maths_t1 age group zssrtfmri_t0 zssrtfmri_t1 zssrtnr_t0 zssrtnr_t1 inhibitionnet_t0 inhibitionnet_t1 inhibitionnet_reactive_t0 inhibitionnet_reactive_t1 inhibitionnet_proactive_t0 inhibitionnet_proactive_t1 presma_l_t0 presma_l_t1 havardoxford_cortl_t0 havardoxford_cortl_t1 calcarine_r_t0 calcarine_r_t1 caudate_r_t0 caudate_r_t1 putamen_r_t0 putamen_r_t1 thalamus_r_t0  thalamus_r_t1  zssrtnr_t2 havardoxford_cortl_proactive_t0 havardoxford_cortl_proactive_t1 havardoxford_cortl_reactive_t0 havardoxford_cortl_reactive_t1 thalamus_l_proactive_t0 thalamus_l_proactive_t1 thalamus_l_reactive_t0 thalamus_l_reactive_t1 putamen_l_proactive_t0 putamen_l_proactive_t1 putamen_l_reactive_t0 putamen_l_reactive_t1 presma_l_proactive_t0 presma_l_proactive_t1 presma_l_reactive_t0 presma_l_reactive_t1 t0_total_sdq t1_total_sdq t0_hy_sdq t1_hy_sdq t0_ext_sdq t0_int_sdq t1_ext_sdq t1_int_sdq using "C:\Users\keertana\Documents\Latent Score Modelling\SSRTBRAINoutliersremoved-5.csv", replace
/*export delimited p_id group zssrtnr_t0 zssrtnr_t1 presma_l_t0 presma_l_t1 havardoxford_cortl_t0 havardoxford_cortl_t1   using "C:\Users\keertana\Documents\Latent Score Modelling\SSRTBRAINoutliersremoved.csv", replace
export delimited p_id group zssrt_t0 zssrt_t1 presma_l_t0 presma_l_t1 havardoxford_cortl_t0 havardoxford_cortl_t1   using "C:\Users\keertana\Documents\Latent Score Modelling\SSRTreplacedBRAINoutliersremoved.csv", replace*/
export delimited p_id group zssrtfmri_t0 zssrtfmri_t1 presma_l_t0 presma_l_t1 havardoxford_cortl_t0 havardoxford_cortl_t1 havardoxford_cortl_proactive_t0 havardoxford_cortl_proactive_t1 havardoxford_cortl_reactive_t0 havardoxford_cortl_reactive_t1 using "C:\Users\keertana\Documents\Latent Score Modelling\SSRTBRAINFMRIONLY.csv", replace


/*just t0*/
 export delimited p_id ssrtnr_t0 ay_bx_t0 PBI_t0 flankerinh_t0 flankerswitch_t0 stroop_t0 dprimeONEBACK_t0 dprimeTWOBACK_t0 cogflex_t0 cf_switchrt_t0 corsi_max_wm_t0 using "C:\Users\keertana\Desktop\MAINSTUDY\T0\EF Summary_t0.csv", replace

 /*both t0andt1*/
 
 export delimited p_id zssrtnr_t0 REzssrtnr_t0 REzssrtnr_t1 REzssrtnr_t2 REstroop_t0 REstroop_t1 REflankerswitch_t0 REflankerswitch_t1 REflankerinh_t0 REflankerinh_t1 REcogflex_t0 REcogflex_t1 REcogflex_t2 RE_cf_switch_t0 RE_cf_switch_t1 RE_cf_switch_t2 ay_bx_t0 PBI_t0 flankerinh_t0 flankerswitch_t0 stroop_t0 dprimeONEBACK_t0 dprimeTWOBACK_t0 cogflex_t0 zcorsi_max_wm_t0  PBI_t0 wcf_switch_rt_t0 PBI_t1 wcf_switch_rt_t1 zssrtnr_t1 ay_bx_t1 flankerinh_t1 flankerswitch_t1 stroop_t1 dprimeONEBACK_t1 dprimeTWOBACK_t1 cogflex_t1  zcorsi_max_wm_t1 zssrtnr_t2 ay_bx_t2 PBI_t2 cogflex_t2 zcorsi_max_wm_t2 using "C:\Users\keertana\Documents\Latent Score Modelling\TotalExample.csv", replace
 
 
 export delimited p_id zssrtnr_t0 age ay_bx_t0 flankerinh_t0 flankerswitch_t0 stroop_t0 dprimeONEBACK_t0 dprimeTWOBACK_t0 cogflex_t0 zcorsi_max_wm_t0 zssrtnr_t1 ay_bx_t1 flankerinh_t1 flankerswitch_t1 stroop_t1 dprimeONEBACK_t1 dprimeTWOBACK_t1 cogflex_t1  zcorsi_max_wm_t1 using "C:\Users\keertana\Documents\Latent Score Modelling\Examplegroup1.csv" if group == 1, replace 
 
 
 export delimited p_id zssrtnr_t0 age ay_bx_t0 flankerinh_t0 flankerswitch_t0 stroop_t0 dprimeONEBACK_t0 dprimeTWOBACK_t0 cogflex_t0 cf_switchrt_t0 corsi_max_wm_t0  zssrtnr_t1 ay_bx_t1 flankerinh_t1 flankerswitch_t1 stroop_t1 dprimeONEBACK_t1 dprimeTWOBACK_t1 cogflex_t1  corsi_max_wm_t1 using "C:\Users\keertana\Documents\Latent Score Modelling\Examplegroup2.csv" if group == 2, replace 
 
 
******************************************/*PRE_POST TRAINING*/*******************************************************
 
reshape long meango_corrrtnr_t Fssrtnr_t Ncompacad_t REflankerswitch_t RE_stroop_t RE_cf_switch_t REflankerinh_t REzssrtnr_t ssrtfmri_t inhibitionnet_t PBI_t ay_bx_t zssrtnr_t flankerinh_t flankerswitch_t stroop_t dprimeONEBACK_t dprimeTWOBACK_t corsi_acc_t cogflex_t cf_switchrt_t corsi_max_wm_t presma_l_t havardoxford_cortl_t calcarine_r_t caudate_r_t putamen_r_t thalamus_r_t havardoxford_cortl_proactive_t havardoxford_cortl_reactive_t inhibitionnet_proactive_t inhibitionnet_reactive_t calcarine_r_proactive_t caudate_r_proactive_t putamen_r_proactive_t thalamus_r_proactive_t calcarine_r_reactive_t caudate_r_reactive_t putamen_r_reactive_t thalamus_r_reactive_t thalamus_l_reactive_t thalamus_l_proactive_t putamen_l_proactive_t putamen_l_reactive_t presma_l_proactive_t presma_l_reactive_t, i(p_id) j(session)

keep p_id group age session trainingslope totalsessions Fzssrtnr_t corsi_max_wm_t flankerinh_t PBI_t flankerswitch_t cf_switchrt_t cogflex_t dprimeONEBACK_t dprimeTWOBACK_t stroop_t
 
 
 /*RT data*/
 reshape long meango_corrrtnr_t	corsi_max_wm_t	Fssrtnr_t	ay_bx_t	PBI_RT_t	cf_switchrt_t	dprimeONEBACK_t	dprimeTWOBACK_t	oneback_hits_av_rt_t	twoback_hits_av_rt_t	stroop_rt_t	flankerswitch_rt_t	flank_inh_rt_t, i(p_id) j(session)
 keep p_id	session	age	group	meango_corrrtnr_t	corsi_max_wm_t	totalsessions	trainingslope	Fssrtnr_t	ay_bx_t	PBI_RT_t	cf_switchrt_t	dprimeONEBACK_t	dprimeTWOBACK_t	oneback_hits_av_rt_t	twoback_hits_av_rt_t	stroop_rt_t	flankerswitch_rt_t	flank_inh_rt_t

 /* error data*/
 reshape long corsi_max_wm_t Fssrtnr_t PBI_error_t ay_bx_error_t corsi_acc_t cf_switch_acc_t dprimeONEBACK_t dprimeTWOBACK_t stroop_error_t flankerswitch_error_t flank_inh_error_t, i(p_id) j(session)
 keep p_id session age group trainingslope totalsessions corsi_max_wm_t Fssrtnr_t PBI_error_t ay_bx_error_t corsi_acc_t cf_switch_acc_t dprimeONEBACK_t dprimeTWOBACK_t stroop_error_t flankerswitch_error_t flank_inh_error_t
 

 
drop if missing(group)

export delimited using "C:\Users\keertana\Desktop\MAINSTUDY\T1\final output\EF Long.csv", replace
export delimited using "C:\Users\keertana\Documents\Latent Score Modelling\EF Long_prepforimpute.csv", replace


set matsize 800

anova ssrt_t group / p_id|group session group#session, repeated(session)
anova ssrt_t group / p_id|group session group#session c.age, repeated(session)

anova Fssrtnr_t group / p_id|group session group#session, repeated(session)
anova ssrtnr_t group / p_id|group session group#session c.age, repeated(session)
 
 
anova ay_bx_t group / p_id|group session group#session, repeated(session)
anova ay_bx_t group / p_id|group session group#session c.age, repeated(session)

 
anova PBI_t group / p_id|group session group#session, repeated(session)
anova PBI_t group / p_id|group session group#session c.age, repeated(session)

 
anova flankerinh_t group / p_id|group session group#session, repeated(session)
anova flankerinh_t group / p_id|group session group#session c.age, repeated(session)

anova flankerswitch_t group / p_id|group session group#session, repeated(session)
anova flankerswitch_t group / p_id|group session group#session c.age, repeated(session)


anova stroop_t group / p_id|group session group#session, repeated(session)
anova stroop_t group / p_id|group session group#session c.age, repeated(session)

anova cogflex_t group / p_id|group session group#session, repeated(session)
anova cogflex_t group / p_id|group session group#session c.age, repeated(session)

anova cf_switchrt_t group / p_id|group session group#session, repeated(session)
anova cf_switchrt_t group / p_id|group session group#session c.age, repeated(session)

anova dprimeONEBACK_t group / p_id|group session group#session, repeated(session)
anova dprimeONEBACK_t group / p_id|group session group#session c.age, repeated(session)


anova dprimeTWOBACK_t group / p_id|group session group#session, repeated(session)
anova dprimeTWOBACK_t group / p_id|group session group#session c.age, repeated(session)


anova corsi_max_wm_t group / p_id|group session group#session, repeated(session)
anova corsi_max_wm_t group / p_id|group session group#session c.age, repeated(session)

anova havardoxford_cortl_t group / p_id|group session group#session, repeated(session)
anova havardoxford_cortl_t group / p_id|group session group#session c.age ssrt_t, repeated(session)

anova inhibitionnet_t group / p_id|group session group#session, repeated(session)
anova inhibitionnet_cortl_t group / p_id|group session group#session c.age ssrt_t, repeated(session)

anova putamen_r_t group / p_id|group session group#session, repeated(session)

anova caudate_r_t group / p_id|group session group#session, repeated(session)

anova calcarine_r_t group / p_id|group session group#session, repeated(session)

anova thalamus_r_t group / p_id|group session group#session, repeated(session)
