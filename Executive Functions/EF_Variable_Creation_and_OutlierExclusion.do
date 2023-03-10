
/*insert data file here*/
use "C:\Users\keertana\Desktop\MAINSTUDY\T1\final output\NEWMASTER.dta" 


 
********VALIDATION FOR TASKS**********
/*ssrt*/

/*BIG FAT NOTE: SSRT for t2 have been accidentally labelled opposite. So use ssrt_t2 for non-replacement method and ssrtnr_t2 for replacement method*/



 pwcorr zssrtnr_t0 meango_corrrtnr_t0, sig star(.05) obs

******************************************/*SSRT*/*******************************************************

 
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
 

 
******************************************/*PRE_POST TRAINING*/*******************************************************
 /*all EF data*/
reshape long meango_corrrtnr_t Fssrtnr_t Ncompacad_t REflankerswitch_t RE_stroop_t RE_cf_switch_t REflankerinh_t REzssrtnr_t ssrtfmri_t inhibitionnet_t PBI_t ay_bx_t zssrtnr_t flankerinh_t flankerswitch_t stroop_t dprimeONEBACK_t dprimeTWOBACK_t corsi_acc_t cogflex_t cf_switchrt_t corsi_max_wm_t presma_l_t havardoxford_cortl_t calcarine_r_t caudate_r_t putamen_r_t thalamus_r_t havardoxford_cortl_proactive_t havardoxford_cortl_reactive_t inhibitionnet_proactive_t inhibitionnet_reactive_t calcarine_r_proactive_t caudate_r_proactive_t putamen_r_proactive_t thalamus_r_proactive_t calcarine_r_reactive_t caudate_r_reactive_t putamen_r_reactive_t thalamus_r_reactive_t thalamus_l_reactive_t thalamus_l_proactive_t putamen_l_proactive_t putamen_l_reactive_t presma_l_proactive_t presma_l_reactive_t, i(p_id) j(session)

keep p_id group age session trainingslope totalsessions Fzssrtnr_t corsi_max_wm_t flankerinh_t PBI_t flankerswitch_t cf_switchrt_t cogflex_t dprimeONEBACK_t dprimeTWOBACK_t stroop_t
 
 /*RT data*/
 reshape long meango_corrrtnr_t	corsi_max_wm_t	Fssrtnr_t	ay_bx_t	PBI_RT_t	cf_switchrt_t	dprimeONEBACK_t	dprimeTWOBACK_t	oneback_hits_av_rt_t	twoback_hits_av_rt_t	stroop_rt_t	flankerswitch_rt_t	flank_inh_rt_t, i(p_id) j(session)
 keep p_id	session	age	group	meango_corrrtnr_t	corsi_max_wm_t	totalsessions	trainingslope	Fssrtnr_t	ay_bx_t	PBI_RT_t	cf_switchrt_t	dprimeONEBACK_t	dprimeTWOBACK_t	oneback_hits_av_rt_t	twoback_hits_av_rt_t	stroop_rt_t	flankerswitch_rt_t	flank_inh_rt_t

 /* error data*/
 reshape long corsi_max_wm_t Fssrtnr_t PBI_error_t ay_bx_error_t corsi_acc_t cf_switch_acc_t dprimeONEBACK_t dprimeTWOBACK_t stroop_error_t flankerswitch_error_t flank_inh_error_t, i(p_id) j(session)
 keep p_id session age group trainingslope totalsessions corsi_max_wm_t Fssrtnr_t PBI_error_t ay_bx_error_t corsi_acc_t cf_switch_acc_t dprimeONEBACK_t dprimeTWOBACK_t stroop_error_t flankerswitch_error_t flank_inh_error_t
 
