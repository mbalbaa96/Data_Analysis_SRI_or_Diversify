clear all
cd "C:\Users\balbaa\Desktop\BalbaaJiaoWittZhu\Data\Main_Experiment"
*import excel "C:\Users\balbaa\Desktop\BalbaaJiaoWittZhu\Data\Main_Experiment\Prolific_Data_All.xlsx", sheet("Sheet1") firstrow clear 
import excel "C:\Users\balbaa\Desktop\BalbaaJiaoWittZhu\Data\Main_Experiment\Prolific_Data_All_with_time.xlsx", sheet("Sheet1") firstrow clear 
save Prolific_Data_without_Negative.dta, replace
import excel "C:\Users\balbaa\Desktop\BalbaaJiaoWittZhu\Data\Main_Experiment\Negative_Low_with_time.xlsx", sheet("Sheet1") firstrow clear 
save Negative_Low_All.dta, replace
use "C:\Users\balbaa\Desktop\BalbaaJiaoWittZhu\Data\Main_Experiment\Prolific_Data_without_Negative.dta"
append using "C:\Users\balbaa\Desktop\BalbaaJiaoWittZhu\Data\Main_Experiment\Negative_Low_All.dta", force nolabel
*Drop incomplete
drop if Earning==.
*Drop returners
drop if Prolific_ID=="61549a2caa92d84f392000e9"
drop if Prolific_ID=="5ec62902bb44c16b842128f9"
drop if Prolific_ID=="56b0ac5fe8b677000cdc0a34"
drop if Prolific_ID=="65dd365163f7579fba938dea"
drop if Prolific_ID=="5c2fcd716ea6880001dc8e3d"
drop if Prolific_ID=="63e51c6b462cfa8a42eba8b2"
drop if Prolific_ID=="6571af52592a29d0854f366e"
drop if Prolific_ID=="6234d77291179c1badffad36"
drop if Prolific_ID=="63c09186d75a9bbd3bec5652"
drop if Prolific_ID=="5ffa5b733bd20c42d0b40b87"
drop if Prolific_ID=="5ced23c585712e00190bd98a"
drop if Prolific_ID=="66154557c058526ce9f1cfe2"
drop if Prolific_ID=="63e51c6b462cfa8a42eba8b2"
drop if Prolific_ID=="6571af52592a29d0854f366e"
drop if Prolific_ID=="6234d77291179c1badffad36"

*Drop non-investors and not fluent in English 
drop if Fin_market==2  | English ==2
*Drop those with more than one mistake in total 
drop if correct_count==3 & P2_AttenCheck_2 !=5
gen id = _n
gen treatment_num =1 if treatment=="Baseline"
replace treatment_num =2 if treatment=="Positive_Higher"
replace treatment_num =3 if treatment=="Negative"
replace treatment_num =4 if treatment=="Positive_Lower"
**For diss version 
drop if treatment_num ==4

replace Gender=0 if Gender==1
replace Gender=1 if Gender==2
replace Gender=0 if Gender==3
replace Gender=0 if Gender==4
replace Intere_inves=0 if Intere_inves==2
replace Own_asset=0 if Own_asset==2
gen Trade_frequently= 0
replace Trade_frequently= 1 if Trade_freq < 4
gen High_wealth=0
replace High_wealth=1 if Inve_amount >=4 & Inve_amount !=7
gen High_CurrentSustainShare= 0
replace High_CurrentSustainShare= 1 if Sustain_share >=4 & Sustain_share !=7
gen Invest_in_SRI=0
replace Invest_in_SRI=1 if Sustain_share !=1 & Sustain_share !=7
*Define Social Preferences accoring to Falk et al. (2018)
egen Share_willing_std= std(Share_willing)
egen Donate_charity_std= std(Donate_charity)
gen Social_Preferences = 0.635*Share_willing_std + 0.365*Donate_charity_std

*Define optimal allocation based on a risk aversion coefficient of 4 
gen HighCorr_OptimalShare_lowreturn= 23.90
gen LowCorr_OptimalShare_lowreturn= 45.60

**#Graph Settings
grstyle clear
set scheme s2color
grstyle init
grstyle set plain, box
grstyle color background white
*grstyle set color Set1
grstyle yesno draw_major_hgrid yes
grstyle yesno draw_major_ygrid yes
grstyle color major_grid gs8
grstyle linepattern major_grid dot
grstyle set legend 4, box inside
grstyle color ci_area gs12%50

*Do subjects value sustainability?
*sum Sustainability_Value
*sum id if Sustainability_Value==0
*sum id if Sustainability_Value < 50
*egen median_Sustainability_Value= median(Sustainability_Value)
*gen High_Sustainability_Share=0
*replace High_Sustainability_Share=1 if  Sustainability_Value>= 50

*egen median_sustainshare = median(Sustain_share)
*gen high_sustainshare=0
*replace high_sustainshare=1 if Sustain_share >= median_sustainshare

*egen median_share_willing = median(Share_willing)
*gen high_share_willing=0 
*replace high_share_willing= 1 if Share_willing >= median_share_willing

*egen median_donation = median(Donation)
*gen high_donation=0
*replace high_donation = 1 if Donation >= median_donation 

*Enivornmental Literacy 
gen correct_Environ_Literacy1=0
gen correct_Environ_Literacy2=0
gen correct_Environ_Literacy3=0
gen correct_Environ_Literacy4=0
gen correct_Environ_Literacy5=0

replace correct_Environ_Literacy1=1 if Environ_Literacy1==2
replace correct_Environ_Literacy2=1 if Environ_Literacy2==2
replace correct_Environ_Literacy3=1 if Environ_Literacy3==1
replace correct_Environ_Literacy4=1 if Environ_Literacy4==1
replace correct_Environ_Literacy5=1 if Environ_Literacy5==4

gen correct_Environ_Literacy=correct_Environ_Literacy1+correct_Environ_Literacy2+correct_Environ_Literacy3+correct_Environ_Literacy4+correct_Environ_Literacy5


*Financial Literacy 
gen correct_Fin_lite_Q1= 0 
replace correct_Fin_lite_Q1= 1 if Fin_lite_Q1==2
gen correct_Fin_lite_Q2= 0 
replace correct_Fin_lite_Q2= 1 if Fin_lite_Q2==1
gen correct_Fin_lite_Q3= 0 
replace correct_Fin_lite_Q3= 1 if Fin_lite_Q3==3
gen correct_Fin_lite_Q4= 0 
replace correct_Fin_lite_Q4= 1 if Fin_lite_Q4==2

gen numcorrect_financialliteracy=correct_Fin_lite_Q2+correct_Fin_lite_Q3+correct_Fin_lite_Q4
gen numcorrect_financialliteracyall=correct_Fin_lite_Q1+correct_Fin_lite_Q2+correct_Fin_lite_Q3+correct_Fin_lite_Q4


*Comprehension Questions
gen correct_PosiRela1=0
gen correct_PosiRela2=0
gen correct_NegaRela1=0
gen correct_NegaRela2=0
replace correct_PosiRela1= 1 if PosiRela1=="drop"
replace correct_PosiRela2= 1 if PosiRela2=="rise"
replace correct_NegaRela1= 1 if NegaRela1=="rise"
replace correct_NegaRela2= 1 if NegaRela2=="drop"
gen correct_comprehension=correct_PosiRela1+correct_PosiRela2+correct_NegaRela1+correct_NegaRela2
sum correct_comprehension

*drop those who answer none of the directional questions correctly in the comprehension education
drop if correct_comprehension==0

*egen median_comprehension= median(correct_comprehension) 
*gen high_comprehension =0
*replace high_comprehension=1 if correct_comprehension >= median_comprehension


*Perception of Dependence 
*Probabilities
gen LowCorr_Relationship4=  P1_Relationship4 if P1_low_first==1
replace LowCorr_Relationship4=  P2_Relationship4 if P1_low_first==0

gen HighCorr_Relationship4=  P1_Relationship4 if P1_low_first==0
replace HighCorr_Relationship4=  P2_Relationship4 if P1_low_first==1

gen LowCorr_Relationship5=  P1_Relationship5 if P1_low_first==1
replace LowCorr_Relationship5=  P2_Relationship5 if P1_low_first==0

gen HighCorr_Relationship5=  P1_Relationship5 if P1_low_first==0
replace HighCorr_Relationship5=  P2_Relationship5 if P1_low_first==1

*Direction 
gen  P1_Relationship2_=0
replace  P1_Relationship2_= 1 if P1_Relationship2 =="rise" & P1_low_first==1
replace  P1_Relationship2_= 1 if P1_Relationship2 =="drop" & P1_low_first==0

gen  P1_Relationship3_=0
replace  P1_Relationship3_= 1 if P1_Relationship3 =="drop" & P1_low_first==1
replace  P1_Relationship3_= 1 if P1_Relationship3 =="rise" & P1_low_first==0

gen  P2_Relationship2=0
replace  P2_Relationship2= 1 if P2_Relationship2_2 =="drop" & P1_low_first==1
replace  P2_Relationship2= 1 if P2_Relationship2_2 =="rise" & P1_low_first==0

gen  P2_Relationship3=0
replace  P2_Relationship3= 1 if P2_Relationship3_2 =="drop" & P1_low_first==0
replace  P2_Relationship3= 1 if P2_Relationship3_2 =="rise" & P1_low_first==1

gen correct_dependence= P1_Relationship2_+ P1_Relationship3_+P2_Relationship2+P2_Relationship3


*Define low and high return assets 
gen P1_investmentAsset_highreturn= P1_investmentAsset if P1_high_return==1
replace P1_investmentAsset_highreturn= P1_investmentAsset2 if P1_high_return==0
gen P1_investmentAsset_lowreturn= P1_investmentAsset if P1_high_return==0
replace P1_investmentAsset_lowreturn= P1_investmentAsset2 if P1_high_return==1

gen P2_investmentAsset_highreturn= P2_investmentAsset if P1_high_return==1
replace P2_investmentAsset_highreturn= P2_investmentAsset2 if P1_high_return==0
gen P2_investmentAsset_lowreturn= P2_investmentAsset if P1_high_return==0
replace P2_investmentAsset_lowreturn= P2_investmentAsset2 if P1_high_return==1

*Define assets based on low and high correlations 
gen LowCorr_investmentAsset_highret= P1_investmentAsset_highreturn if P1_low_first ==1
replace LowCorr_investmentAsset_highret= P2_investmentAsset_highreturn if P1_low_first ==0
gen HighCorr_investmentAsset_highret= P1_investmentAsset_highreturn if P1_low_first ==0
replace HighCorr_investmentAsset_highret= P2_investmentAsset_highreturn if P1_low_first ==1

gen LowCorr_investmentAsset_lowret = P1_investmentAsset_lowreturn if P1_low_first == 1
replace LowCorr_investmentAsset_lowret = P2_investmentAsset_lowreturn if P1_low_first == 0
gen HighCorr_investmentAsset_lowret = P1_investmentAsset_lowreturn if P1_low_first == 0
replace HighCorr_investmentAsset_lowret = P2_investmentAsset_lowreturn if P1_low_first == 1

*Drop People who might have not understood the diversification potential of Asset 2 and invested more than 50%
*drop if P1_investmentAsset_lowreturn >5000 & treatment_num<4 | P2_investmentAsset_lowreturn > 5000 & treatment_num<4

*Translate Allocations to Percentages
replace LowCorr_investmentAsset_highret= (LowCorr_investmentAsset_highret/10000)*100
replace LowCorr_investmentAsset_lowret= (LowCorr_investmentAsset_lowret/10000)*100

replace HighCorr_investmentAsset_highret= (HighCorr_investmentAsset_highret/10000)*100
replace HighCorr_investmentAsset_lowret= (HighCorr_investmentAsset_lowret/10000)*100

*Define Correlation Neglect 
gen correlation_neglect=  HighCorr_investmentAsset_lowret - LowCorr_investmentAsset_lowret 

*Define Correlation Consideration
gen consider_corr=  LowCorr_investmentAsset_lowret - HighCorr_investmentAsset_lowret 

*Translate beliefs to returns
replace P1_belief_upper = (P1_belief_upper -10000)/100
replace P1_belief_lower = (P1_belief_lower -10000)/100
replace P1_Expect_value = (P1_Expect_value -10000)/100
replace P2_belief_upper = (P2_belief_upper -10000)/100
replace P2_belief_lower = (P2_belief_lower -10000)/100
replace P2_Expect_value = (P2_Expect_value -10000)/100

**# Label Variables 
label var Own_asset "Owns Equity" 
label var Intere_inves "Interested Financial Markets" 
label var Trade_frequently "Frequent Trader"
label var High_wealth "High Wealth"
label var High_CurrentSustainShare "High SRI Share"
label var Invest_in_SRI "Invest in SRI"
label var Risk_taking "Risk Preferences" 
label var Share_willing "Willingness to Share"
label var Donate_charity "Hypothetical Donation"
label var Social_Preferences "Social Preferences"
label var Donation "Incentivized Donation" 
label var correct_Environ_Literacy  "Environmental Literacy" 
label var Sustainability_Value  "Incentivized Sustainable Investment"
label var numcorrect_financialliteracyall "Financial Literacy" 
label var correct_comprehension "Correlation Comprehension" 
label var Stat_know "Statistics Knowledge" 
label var Number_think "Doesn't Think about Numbers" 
label var Number_inter "Consider Numbers Decisions"
label var P1_low_first "Correlation Order"
label var P1_high_return "Asset Order"
label define treatment_lbl 1 "Baseline" 2 "Green" 3 "Brown" 4 "Green Low"
label values treatment_num treatment_lbl

save All_processed.dta
**# Summary Statistics 
eststo clear

estpost tabstat Age Gender High_wealth Own_asset Intere_inves Trade_frequently numcorrect_financialliteracyall  Risk_taking Patient Share_willing Donate_charity Donation Sustainability_Value correct_Environ_Literacy  High_CurrentSustainShare Invest_in_SRI correct_comprehension Stat_know Number_think Number_inter, by(treatment_num)  c(stat) stat(mean sd min max n)

esttab using sumstat.tex, replace cells("count Mean(fmt(2)) SD(fmt(2)) Min(fmt(0)) Max(fmt(0))") nonumber nomtitle booktabs noobs title("Summary Statistics\label{tabsummarystat}") collabels("N" "Mean" "SD" "Min" "Max")  coeflabels(Own_asset "Owns Equity" Intere_inves "Interested in Financial Markets" Trade_frequently "Frequent Trader" High_wealth "High Wealth" High_CurrentSustainShare "High SRI Share" Risk_taking "Risk Preferences" Share_willing "Willingness to Share" Donate_charity "Hypothetical Donation" Donation "Incentivized Donation"  correct_Environ_Literacy  "Environmental Literacy" High_CurrentSustainShare "High SRI Share" Sustainability_Value  "Incentivized Sustainable Investment" Invest_in_SRI "Invest in SRI" numcorrect_financialliteracyall "Financial Literacy" correct_comprehension "Correlation Comprehension" Stat_know "Statistics Knowledge" Number_think "Doesn't Think about Numbers" Number_inter "Consider Numbers in Decisions")

*Main Effects
*Positive
*Allocations
ttest HighCorr_investmentAsset_lowret if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest HighCorr_investmentAsset_lowret if treatment_num==1 | treatment_num==3, by(treatment) reverse

ttest LowCorr_investmentAsset_lowret if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest LowCorr_investmentAsset_lowret if treatment_num==1 | treatment_num==3, by(treatment) reverse
*Correlation Neglect 
*Within Treatments 
ttest LowCorr_investmentAsset_lowret==HighCorr_investmentAsset_lowret if treatment_num==1
ttest LowCorr_investmentAsset_lowret==HighCorr_investmentAsset_lowret if treatment_num==2
ttest LowCorr_investmentAsset_lowret==HighCorr_investmentAsset_lowret if treatment_num==3
*Between Treatments
ttest correlation_neglect if treatment_num==1 | treatment_num==2 , by(treatment) reverse
ttest correlation_neglect if treatment_num==1 | treatment_num==3 , by(treatment) reverse
ttest consider_corr if treatment_num==1 | treatment_num==2 , by(treatment) reverse
ttest consider_corr if treatment_num==1 | treatment_num==3 , by(treatment) reverse

*Deviation from Optimal
ttest HighCorr_investmentAsset_lowret == HighCorr_OptimalShare_lowreturn if treatment_num==1 
ttest HighCorr_investmentAsset_lowret == HighCorr_OptimalShare_lowreturn if treatment_num==2
ttest HighCorr_investmentAsset_lowret == HighCorr_OptimalShare_lowreturn if treatment_num==3

ttest LowCorr_investmentAsset_lowret == LowCorr_OptimalShare_lowreturn if treatment_num==1 
ttest LowCorr_investmentAsset_lowret == LowCorr_OptimalShare_lowreturn if treatment_num==2
ttest LowCorr_investmentAsset_lowret == LowCorr_OptimalShare_lowreturn if treatment_num==3

eststo clear
***T-test tables Green
estpost ttest HighCorr_investmentAsset_lowret if treatment_num==1 | treatment_num==2, by(treatment)
eststo green_positivecorr

esttab using MainEffect_Green_Diversifcation_High.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Main Effects Allocation Decisions\label{test_MainEffect_Green_Allocation_Pos}") collabels("Baseline" "Green" "Difference" "t-statistic" ) addnote("Note:  Difference in Allocation to the Diversification Asset between Treatments") coeflabels(HighCorr_investmentAsset_lowret "Positive Correlation")

eststo clear
estpost ttest LowCorr_investmentAsset_lowret if treatment_num==1 | treatment_num==2, by(treatment)
eststo green_negativecorr

esttab using MainEffect_Green_Diversifcation_Low.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Main Effects Allocation Decisions\label{test_MainEffect_Green_Allocation_Neg}") collabels("Baseline" "Positive" "Difference" "t-statistic" ) addnote("Note: Difference in Allocation to the Diversification Asset between Treatments") coeflabels(LowCorr_investmentAsset_lowret "Negative Correlation")

eststo clear
estpost ttest correlation_neglect if treatment_num==1 | treatment_num==2 , by(treatment) 
esttab using MainEffect_Green_CorrelationNeglect.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Main Effects Correlation Neglect\label{test_MainEffect_Green_CorrelationNeglect}") collabels("Baseline" "Green" "Difference" "t-statistic" ) addnote("Note: Difference in Correlation Neglect between Treatments") coeflabels(correlation_neglect "Correlation Neglect")


eststo clear
***T-test tables Brown
estpost ttest HighCorr_investmentAsset_lowret if treatment_num==1 | treatment_num==3, by(treatment)

esttab using MainEffect_Brown_Diversifcation_High.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Main Effects Allocation Decisions\label{test_MainEffect_Brown_Allocation_Pos}") collabels("Baseline" "Brown" "Difference" "t-statistic") addnote("Note:  Difference in Allocation to the Diversification Asset between Treatments") coeflabels(HighCorr_investmentAsset_lowret "Positive Correlation")

eststo clear
estpost ttest LowCorr_investmentAsset_lowret if treatment_num==1 | treatment_num==3, by(treatment)

esttab using MainEffect_Brown_Diversifcation_Low.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Main Effects Allocation Decisions\label{test_MainEffect_Brown_Allocation_Neg}") collabels("Baseline" "Brown" "Difference" "t-statistic") addnote("Note: Difference in Allocation to the Diversification Asset between Treatments") coeflabels(LowCorr_investmentAsset_lowret "Negative Correlation")

eststo clear
estpost ttest correlation_neglect if treatment_num==1 | treatment_num==3 , by(treatment) 
esttab using MainEffect_Brown_CorrelationNeglect.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Main Effects Correlation Neglect\label{test_MainEffect_Brown_CorrelationNeglect}") collabels("Baseline" "Brown" "Difference" "t-statistic") addnote("Note: Difference in Correlation Neglect between Treatments") coeflabels(correlation_neglect "Correlation Neglect")



**#Bar graphs
**Green & Brown
mean LowCorr_investmentAsset_lowret if treatment_num<4, over(treatment_num) level(95) 
marginsplot, recast(bar) name(allocation_lowcorr_treatments, replace) xlabel( 1 "Baseline" 2 "Green"  3 "Brown") xtitle("") ytitle("Share") title("Allocation" "to the Diversification Asset in the Negative Correlation Scenario", color(black)) plotopts(barwidth(0.5) bcolor(gray) lcolor(black) lw(medium) lcolor(gray) mcolor(black) msymbol(O O) msize(small small))  ciopts(lcolor(black)) scale(0.8)
graph export "allocation_lowcorr_treatments.png", replace width(800) height(600)

mean HighCorr_investmentAsset_lowret if treatment_num<4, over(treatment_num) level(95) 
marginsplot, recast(bar) name(allocation_highcorr_treatments, replace) xlabel( 1 "Baseline" 2 "Green"  3 "Brown") xtitle("") ytitle("Share") title("Allocation" "to the Diversification Asset in the Positive Correlation Scenario", color(black)) plotopts(barwidth(0.5) bcolor(gray) lcolor(black) lw(medium) lcolor(gray) mcolor(black) msymbol(O O) msize(small small))  ciopts(lcolor(black)) scale(0.8)
graph export "allocation_highcorr_treatments.png", replace width(800) height(600)

mean correlation_neglect if treatment_num<4, over(treatment_num) level(95) 
marginsplot, recast(bar) name(CorrelationNeglect_treatments, replace) xlabel( 1 "Baseline" 2 "Green" 3 "Brown") xtitle("") ytitle("Share") title("Correlation Neglect", color(black)) plotopts(barwidth(0.5) bcolor(gray) lcolor(black) lw(medium) lcolor(gray) mcolor(black) msymbol(O O) msize(small small))  ciopts(lcolor(black)) scale(0.8)
graph export "CorrelationNeglect_treatments.png", replace width(800) height(600)


mean consider_corr if treatment_num<4, over(treatment_num) level(95) 
marginsplot, recast(bar) name(CorrelationConsider_treatments, replace) xlabel( 1 "Baseline" 2 "Green" 3 "Brown") xtitle("") ytitle("Share") title("Correlation Consideration", color(black)) plotopts(barwidth(0.5) bcolor(gray) lcolor(black) lw(medium) lcolor(gray) mcolor(black) msymbol(O O) msize(small small))  ciopts(lcolor(black)) scale(0.8)
graph export "CorrelationConsider_treatments.png", replace width(800) height(600)



**Order Effects in general for baseline
ttest LowCorr_investmentAsset_lowret if treatment_num==1, by(P1_low_first)
ttest HighCorr_investmentAsset_lowret if treatment_num==1, by(P1_low_first)
ttest correlation_neglect if treatment_num==1, by(P1_low_first)

**Order Effects in general for green
ttest LowCorr_investmentAsset_lowret if treatment_num==2, by(P1_low_first)
ttest HighCorr_investmentAsset_lowret if treatment_num==2, by(P1_low_first)
ttest correlation_neglect if treatment_num==2, by(P1_low_first)

**Order Effects in general for brown
ttest LowCorr_investmentAsset_lowret if treatment_num==3, by(P1_low_first)
ttest HighCorr_investmentAsset_lowret if treatment_num==3, by(P1_low_first)
ttest correlation_neglect if treatment_num==3, by(P1_low_first)

**# Correlation Order Effects
*Green
ttest correlation_neglect if treatment_num==1 & P1_low_first==0 | treatment_num==2 &  P1_low_first==0 , by(treatment)
ttest correlation_neglect if treatment_num==1 & P1_low_first==1 | treatment_num==2 &  P1_low_first==1 , by(treatment)
eststo clear
***Latex
estpost ttest correlation_neglect if treatment_num==1 & P1_low_first==0 | treatment_num==2 &  P1_low_first==0 , by(treatment)
esttab using OrderEffects_CorrelationNeglect_PosFirst_Green.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t count(fmt(0))") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Order Effects Correlation Neglect\label{test_OrderEffects_CorrelationNeglect_green}") collabels("Baseline" "Green" "Difference" "t-statistic" "N") addnote("Note: Difference in Correlation Neglect between Treatments Split by Correlation Order") coeflabels(consider_corr "Positive Correlation First")

eststo clear
***Latex
estpost ttest correlation_neglect if treatment_num==1 & P1_low_first==1 | treatment_num==2 &  P1_low_first==1 , by(treatment)
esttab using OrderEffects_CorrelationNeglect_NegFirst_Green.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t count(fmt(0))") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Order Effects Correlation Neglect\label{test_OrderEffects_CorrelationNeglect_green}") collabels("Baseline" "Green" "Difference" "t-statistic" "N") addnote("Note: Difference in Correlation Neglect between Treatments Split by Correlation Order") coeflabels(consider_corr "Negative Correlation First")

*Brown
ttest correlation_neglect if treatment_num==1 & P1_low_first==0 | treatment_num==3 &  P1_low_first==0 , by(treatment)
ttest correlation_neglect if treatment_num==1 & P1_low_first==1 | treatment_num==3 &  P1_low_first==1 , by(treatment)
eststo clear
***Latex
estpost ttest correlation_neglect if treatment_num==1 & P1_low_first==0 | treatment_num==3 &  P1_low_first==0 , by(treatment)
esttab using OrderEffects_CorrelationNeglect_PosFirst_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t count(fmt(0))") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Order Effects Correlation Neglect\label{test_OrderEffects_CorrelationNeglect_brown}") collabels("Baseline" "Brown" "Difference" "t-statistic" "N") addnote("Note: Difference in Correlation Neglect between Treatments Split by Correlation Order") coeflabels(consider_corr "Positive Correlation First")

eststo clear
***Latex
estpost ttest correlation_neglect if treatment_num==1 & P1_low_first==1 | treatment_num==3 &  P1_low_first==1 , by(treatment)
esttab using OrderEffects_CorrelationNeglect_NegFirst_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t count(fmt(0))") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Order Effects Correlation Neglect\label{test_OrderEffects_CorrelationNeglect_brown}") collabels("Baseline" "Brown" "Difference" "t-statistic" "N") addnote("Note: Difference in Correlation Neglect between Treatments Split by Correlation Order") coeflabels(consider_corr "Negative Correlation First")


**Learning 
*Do subjects allocate on average more to the diversification asset in the second choice?
ttest P2_investmentAsset_lowreturn==P1_investmentAsset_lowretur if treatment_num==1
ttest P2_investmentAsset_lowreturn==P1_investmentAsset_lowretur if treatment_num==2
ttest P2_investmentAsset_lowreturn==P1_investmentAsset_lowretur if treatment_num==3


gen learning =  P2_investmentAsset_lowreturn-P1_investmentAsset_lowreturn
ttest learning if treatment_num==1  | treatment_num==2, by (treatment)
ttest learning if treatment_num==1  | treatment_num==3, by (treatment)


**Order Effects Beliefs 
*Expected Value
gen LowCorr_Expect_value= P1_Expect_value if P1_low_first ==1 
replace LowCorr_Expect_value= P2_Expect_value if P1_low_first ==0

gen HighCorr_Expect_value= P1_Expect_value if P1_low_first ==0 
replace HighCorr_Expect_value= P2_Expect_value if P1_low_first ==1

gen Avg_Expect_value= (LowCorr_Expect_value+HighCorr_Expect_value)/2
gen Diff_Expect_value= LowCorr_Expect_value-HighCorr_Expect_value


*Lower Bound
gen LowCorr_belief_lower= P1_belief_lower if P1_low_first ==1 
replace LowCorr_belief_lower= P2_belief_lower if P1_low_first ==0

gen HighCorr_belief_lower= P1_belief_lower if P1_low_first ==0 
replace HighCorr_belief_lower= P2_belief_lower if P1_low_first ==1

gen Avg_belief_lower= (LowCorr_belief_lower+HighCorr_belief_lower)/2
gen Diff_belief_lower= LowCorr_belief_lower-HighCorr_belief_lower

*Upper Bound 
gen LowCorr_belief_upper= P1_belief_upper if P1_low_first ==1 
replace LowCorr_belief_upper= P2_belief_upper if P1_low_first ==0

gen HighCorr_belief_upper= P1_belief_upper if P1_low_first ==0 
replace HighCorr_belief_upper= P2_belief_upper if P1_low_first ==1

gen Avg_belief_upper= (LowCorr_belief_upper+HighCorr_belief_upper)/2
gen Diff_belief_upper= LowCorr_belief_upper-HighCorr_belief_upper

*Loss and Gain probabilities
gen LowCorr_Return_perception1= P1_Return_perception1 if P1_low_first ==1 
replace LowCorr_Return_perception1= P2_Return_perception1 if P1_low_first ==0

gen HighCorr_Return_perception1= P1_Return_perception1 if P1_low_first ==0 
replace HighCorr_Return_perception1= P2_Return_perception1 if P1_low_first ==1

gen LowCorr_Return_perception2= P1_Return_perception2 if P1_low_first ==1 
replace LowCorr_Return_perception2= P2_Return_perception2 if P1_low_first ==0

gen HighCorr_Return_perception2= P1_Return_perception2 if P1_low_first ==0 
replace HighCorr_Return_perception2= P2_Return_perception2 if P1_low_first ==1

gen LowCorr_Return_perception3= P1_Return_perception3 if P1_low_first ==1 
replace LowCorr_Return_perception3= P2_Return_perception3 if P1_low_first ==0

gen HighCorr_Return_perception3= P1_Return_perception3 if P1_low_first ==0 
replace HighCorr_Return_perception3= P2_Return_perception3 if P1_low_first ==1

gen Avg_Return_perception1= (LowCorr_Return_perception1+HighCorr_Return_perception1)/2
gen Avg_Return_perception2= (LowCorr_Return_perception2+HighCorr_Return_perception2)/2
gen Avg_Return_perception3= (LowCorr_Return_perception3+HighCorr_Return_perception3)/2

*Risk Perception 
gen LowCorr_range = LowCorr_belief_upper- LowCorr_belief_lower
gen HighCorr_range = HighCorr_belief_upper- HighCorr_belief_lower
gen Diff_range= LowCorr_range-HighCorr_range

gen LowCorr_Risk_perception1= P1_Risk_perception1 if P1_low_first ==1 
replace LowCorr_Risk_perception1= P2_Risk_perception1 if P1_low_first ==0

gen HighCorr_Risk_perception1= P1_Risk_perception1 if P1_low_first ==0 
replace HighCorr_Risk_perception1= P2_Risk_perception1 if P1_low_first ==1


**#Treatment Effects Beliefs Green
*Portfolio Value
eststo clear
estpost ttest Diff_Expect_value if treatment_num==1 | treatment_num==2, by(treatment)
esttab using Diff_Expect_value.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_Diff_Expect_value}") collabels("Baseline" "Green" "Difference" "t-statistic") addnote("Note: ") coeflabels(Diff_Expect_value "Difference")

eststo clear
estpost ttest LowCorr_Expect_value if treatment_num==1 | treatment_num==2, by(treatment)
esttab using Expect_value_Low.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_Expect_value_Low}") collabels("Baseline" "Green" "Difference" "t-statistic") addnote("Note: ") coeflabels(LowCorr_Expect_value "Low Correlation")

eststo clear
estpost ttest HighCorr_Expect_value if treatment_num==1 | treatment_num==2, by(treatment)
esttab using Expect_value_High.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_Expect_value_High}") collabels("Baseline" "Green" "Difference" "t-statistic" ) addnote("Note: ") coeflabels(HighCorr_Expect_value "High Correlation")

eststo clear
estpost ttest Diff_belief_lower if treatment_num==1 | treatment_num==2, by(treatment)
esttab using Diff_belief_lower.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_Diff_belief_lower}") collabels("Baseline" "Green" "Difference" "t-statistic" ) addnote("Note: ") coeflabels(Diff_belief_lower "Difference")

eststo clear
estpost ttest LowCorr_belief_lower if treatment_num==1 | treatment_num==2, by(treatment)
esttab using belief_lower_Low.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_belief_lower_Low}") collabels("Baseline" "Green" "Difference" "t-statistic") addnote("Note: ") coeflabels(LowCorr_belief_lower "Negative Correlation")

eststo clear
estpost ttest HighCorr_belief_lower if treatment_num==1 | treatment_num==2, by(treatment)
esttab using belief_lower_High.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_belief_lower_High}") collabels("Baseline" "Green" "Difference" "t-statistic") addnote("Note: ") coeflabels(HighCorr_belief_lower "Positive Correlation")

eststo clear
estpost ttest Diff_belief_upper if treatment_num==1 | treatment_num==2, by(treatment)
esttab using Diff_belief_upper.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_Diff_belief_upper}") collabels("Baseline" "Green" "Difference" "t-statistic" ) addnote("Note: ") coeflabels(Diff_belief_upper "Difference")

eststo clear
estpost ttest LowCorr_belief_upper if treatment_num==1 | treatment_num==2, by(treatment)
esttab using belief_upper_Low.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_belief_upper_Low}") collabels("Baseline" "Green" "Difference" "t-statistic" ) addnote("Note: ") coeflabels(LowCorr_belief_upper "Negative Correlation")

eststo clear
estpost ttest HighCorr_belief_upper if treatment_num==1 | treatment_num==2, by(treatment)
esttab using belief_upper_High.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_belief_upper_High}") collabels("Baseline" "Green" "Difference" "t-statistic") addnote("Note: ") coeflabels( HighCorr_belief_upper "Positive Correlation")

*Green
ttest HighCorr_belief_lower if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest LowCorr_belief_lower if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest LowCorr_belief_lower== HighCorr_belief_lower if treatment_num==1
ttest LowCorr_belief_lower== HighCorr_belief_lower if treatment_num==2
ttest Diff_belief_lower if treatment_num==1 | treatment_num==2, by(treatment) reverse

ttest HighCorr_belief_upper if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest LowCorr_belief_upper if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest LowCorr_belief_upper== HighCorr_belief_upper if treatment_num==1
ttest LowCorr_belief_upper== HighCorr_belief_upper if treatment_num==2
ttest Diff_belief_upper if treatment_num==1 | treatment_num==2, by(treatment) reverse

ttest HighCorr_Expect_value if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest LowCorr_Expect_value if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest LowCorr_Expect_value==HighCorr_Expect_value if treatment_num==1
ttest LowCorr_Expect_value==HighCorr_Expect_value if treatment_num==2
ttest Diff_Expect_value if treatment_num==1 | treatment_num==2, by(treatment) reverse

*Brown
ttest HighCorr_belief_lower if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest LowCorr_belief_lower if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest LowCorr_belief_lower== HighCorr_belief_lower if treatment_num==1
ttest LowCorr_belief_lower== HighCorr_belief_lower if treatment_num==3
ttest Diff_belief_lower if treatment_num==1 | treatment_num==3, by(treatment) reverse

ttest HighCorr_belief_upper if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest LowCorr_belief_upper if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest LowCorr_belief_upper== HighCorr_belief_upper if treatment_num==1
ttest LowCorr_belief_upper== HighCorr_belief_upper if treatment_num==3
ttest Diff_belief_upper if treatment_num==1 | treatment_num==3, by(treatment) reverse

ttest HighCorr_Expect_value if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest LowCorr_Expect_value if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest LowCorr_Expect_value==HighCorr_Expect_value if treatment_num==1
ttest LowCorr_Expect_value==HighCorr_Expect_value if treatment_num==3
ttest Diff_Expect_value if treatment_num==1 | treatment_num==3, by(treatment) reverse

*Non-incentivized 
ttest LowCorr_Return_perception1 if treatment_num==1 | treatment_num==2, by(treatment)
ttest HighCorr_Return_perception1 if treatment_num==1 | treatment_num==2, by(treatment)
ttest Avg_Return_perception1 if treatment_num==1 | treatment_num==2, by(treatment)

ttest LowCorr_Return_perception2 if treatment_num==1 | treatment_num==2, by(treatment)
ttest HighCorr_Return_perception2 if treatment_num==1 | treatment_num==2, by(treatment)
ttest Avg_Return_perception2 if treatment_num==1 | treatment_num==2, by(treatment)

ttest LowCorr_Return_perception3 if treatment_num==1 | treatment_num==2, by(treatment)
ttest HighCorr_Return_perception3 if treatment_num==1 | treatment_num==2, by(treatment)
ttest Avg_Return_perception3 if treatment_num==1 | treatment_num==2, by(treatment)

*Portfolio Risk 
ttest LowCorr_range if treatment_num==1 | treatment_num==2, by(treatment)
ttest HighCorr_range if treatment_num==1 | treatment_num==2, by(treatment)
ttest Diff_range if treatment_num==1 | treatment_num==2, by(treatment)

*Non-incentivized
ttest LowCorr_Risk_perception1 if treatment_num==1 | treatment_num==2, by(treatment)
ttest HighCorr_Risk_perception1 if treatment_num==1 | treatment_num==2, by(treatment)

*Difference in dependence between low and high correlation
gen Diff_Relationship4= LowCorr_Relationship4-HighCorr_Relationship4
gen Diff_Relationship5= LowCorr_Relationship5-HighCorr_Relationship5
*Dependence Probabilities 
eststo clear
estpost ttest LowCorr_Relationship4 if treatment_num==1 | treatment_num==2, by(treatment)
esttab using LowCorr_Relationship4.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Perception of Dependence between Treatments\label{test_LowCorr_PerceptionofDependence1}") collabels("Baseline" "Green" "Difference" "t-statistic" ) addnote("Note: ") coeflabels(LowCorr_Relationship4 "Negative Correlation")

eststo clear
estpost ttest HighCorr_Relationship4 if treatment_num==1 | treatment_num==2, by(treatment)
esttab using HighCorr_Relationship4.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Perception of Dependence between Treatments\label{test_HighCorr_PerceptionofDependence1}") collabels("Baseline" "Green" "Difference" "t-statistic") addnote("Note: ") coeflabels(HighCorr_Relationship4 "Positive Correlation")

eststo clear
estpost ttest LowCorr_Relationship5 if treatment_num==1 | treatment_num==2, by(treatment)
esttab using LowCorr_Relationship5.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Perception of Dependence between Treatments\label{test_LowCorr_PerceptionofDependence2}") collabels("Baseline" "Green" "Difference" "t-statistic") addnote("Note: ") coeflabels(LowCorr_Relationship5 "Negative Correlation")

eststo clear
estpost ttest HighCorr_Relationship5 if treatment_num==1 | treatment_num==2, by(treatment)
esttab using HighCorr_Relationship5.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Perception of Dependence between Treatments\label{test_HighCorr_PerceptionofDependence2}") collabels("Baseline" "Green" "Difference" "t-statistic" ) addnote("Note: ") coeflabels(HighCorr_Relationship5 "Positive  Correlation")

**Treatment Effects Beliefs Brown
*Portfolio Value
eststo clear
estpost ttest Diff_Expect_value if treatment_num==1 | treatment_num==3, by(treatment)
esttab using Diff_Expect_value_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_Diff_Expect_value_Brown}") collabels("Baseline" "Brown" "Difference" "t-statistic" ) addnote("Note:") coeflabels(Diff_Expect_value "Difference")

eststo clear
estpost ttest LowCorr_Expect_value if treatment_num==1 | treatment_num==3, by(treatment)
esttab using Expect_value_Low_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_Expect_value_Low_Brown}") collabels("Baseline" "Brown" "Difference" "t-statistic") addnote("Note:") coeflabels( LowCorr_Expect_value "Negative Correlation")

eststo clear
estpost ttest HighCorr_Expect_value if treatment_num==1 | treatment_num==3, by(treatment)
esttab using Expect_value_High_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_Expect_value_High_Brown}") collabels("Baseline" "Brown" "Difference" "t-statistic" ) addnote("Note:") coeflabels( HighCorr_Expect_value "Positive Correlation")

eststo clear
estpost ttest Diff_belief_lower if treatment_num==1 | treatment_num==3, by(treatment)
esttab using Diff_belief_lower_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_Diff_belief_lower_Brown}") collabels("Baseline" "Brown" "Difference" "t-statistic" ) addnote("Note:") coeflabels(Diff_belief_lower "Difference")

eststo clear
estpost ttest LowCorr_belief_lower if treatment_num==1 | treatment_num==3, by(treatment)
esttab using belief_lower_Low_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_belief_lower_Low_Brown}") collabels("Baseline" "Brown" "Difference" "t-statistic" ) addnote("Note:") coeflabels(LowCorr_belief_lower "Negative Correlation")

eststo clear
estpost ttest HighCorr_belief_lower if treatment_num==1 | treatment_num==3, by(treatment)
esttab using belief_lower_High_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_belief_lower_High_Brown}") collabels("Baseline" "Brown" "Difference" "t-statistic" ) addnote("Note:") coeflabels(HighCorr_belief_lower "Positive Correlation")

eststo clear
estpost ttest Diff_belief_upper if treatment_num==1 | treatment_num==3, by(treatment)
esttab using Diff_belief_upper_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_Diff_belief_upper_Brown}") collabels("Baseline" "Brown" "Difference" "t-statistic" ) addnote("Note:") coeflabels(Diff_belief_upper "Difference")

eststo clear
estpost ttest LowCorr_belief_upper if treatment_num==1 | treatment_num==3, by(treatment)
esttab using belief_upper_Low_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_belief_upper_Low_Brown}") collabels("Baseline" "Brown" "Difference" "t-statistic") addnote("Note:") coeflabels( LowCorr_belief_upper "Negative Correlation")

eststo clear
estpost ttest HighCorr_belief_upper if treatment_num==1 | treatment_num==3, by(treatment)
esttab using belief_upper_High_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t ") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("\label{test_belief_upper_High_Brown}") collabels("Baseline" "Brown" "Difference" "t-statistic") addnote("Note:") coeflabels(HighCorr_belief_upper "Positive Correlation")

ttest HighCorr_Relationship5  if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest LowCorr_Relationship5  if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest LowCorr_Relationship5 == HighCorr_Relationship5  if treatment_num==1 
ttest LowCorr_Relationship5 == HighCorr_Relationship5  if treatment_num==2 
ttest Diff_Relationship5 if treatment_num==1 | treatment_num==2, by(treatment) reverse

ttest HighCorr_Relationship4  if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest LowCorr_Relationship4  if treatment_num==1 | treatment_num==2, by(treatment) reverse
ttest LowCorr_Relationship4 == HighCorr_Relationship4  if treatment_num==1 
ttest LowCorr_Relationship4 == HighCorr_Relationship4  if treatment_num==2 
ttest Diff_Relationship4 if treatment_num==1 | treatment_num==2, by(treatment) reverse


ttest HighCorr_Relationship5  if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest LowCorr_Relationship5  if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest LowCorr_Relationship5 == HighCorr_Relationship5  if treatment_num==1 
ttest LowCorr_Relationship5 == HighCorr_Relationship5  if treatment_num==3 
ttest Diff_Relationship5 if treatment_num==1 | treatment_num==3, by(treatment) reverse

ttest HighCorr_Relationship4  if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest LowCorr_Relationship4  if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest LowCorr_Relationship4 == HighCorr_Relationship4  if treatment_num==1 
ttest LowCorr_Relationship4 == HighCorr_Relationship4  if treatment_num==3 
ttest Diff_Relationship4 if treatment_num==1 | treatment_num==3, by(treatment) reverse

*Non-incentivized
ttest LowCorr_Return_perception1 if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest HighCorr_Return_perception1 if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest Avg_Return_perception1 if treatment_num==1 | treatment_num==3, by(treatment) reverse

ttest LowCorr_Return_perception2 if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest HighCorr_Return_perception2 if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest Avg_Return_perception2 if treatment_num==1 | treatment_num==3, by(treatment) reverse

ttest LowCorr_Return_perception3 if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest HighCorr_Return_perception3 if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest Avg_Return_perception3 if treatment_num==1 | treatment_num==3, by(treatment) reverse

*Portfolio Risk 
ttest LowCorr_range if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest HighCorr_range if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest Diff_range if treatment_num==1 | treatment_num==3, by(treatment) reverse

*Non-incentivized
ttest LowCorr_Risk_perception1 if treatment_num==1 | treatment_num==3, by(treatment) reverse
ttest HighCorr_Risk_perception1 if treatment_num==1 | treatment_num==3, by(treatment) reverse


*Dependence Probabilities 
eststo clear
estpost ttest LowCorr_Relationship4 if treatment_num==1 | treatment_num==3, by(treatment)
esttab using LowCorr_Relationship4_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t count(fmt(0))") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Perception of Dependence between Treatments\label{test_LowCorr_PerceptionofDependence_Brown1}") collabels("Baseline" "Brown" "Difference" "t-statistic" "N") addnote("Note: ") coeflabels(LowCorr_Relationship4 "Negative Correlation")

eststo clear
estpost ttest HighCorr_Relationship4 if treatment_num==1 | treatment_num==3, by(treatment)
esttab using HighCorr_Relationship4_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t count(fmt(0))") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Perception of Dependence between Treatments\label{test_HighCorr_PerceptionofDependence_Brown1}") collabels("Baseline" "Brown" "Difference" "t-statistic" "N") addnote("Note: ") coeflabels(HighCorr_Relationship4 "Positive Correlation")

eststo clear
estpost ttest LowCorr_Relationship5 if treatment_num==1 | treatment_num==3, by(treatment)
esttab using LowCorr_Relationship5_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t count(fmt(0))") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Perception of Dependence between Treatments\label{test_LowCorr_PerceptionofDependence_Brown2}") collabels("Baseline" "Brown" "Difference" "t-statistic" "N") addnote("Note: ") coeflabels(LowCorr_Relationship5 "Negative Correlation")

eststo clear
estpost ttest HighCorr_Relationship5 if treatment_num==1 | treatment_num==3, by(treatment)
esttab using HighCorr_Relationship5_Brown.tex, replace cells("mu_1(fmt(2)) mu_2 b(star) t count(fmt(0))") star(* 0.1 ** 0.05 *** 0.01) nonumber booktabs noobs title("Perception of Dependence between Treatments\label{test_HighCorr_PerceptionofDependence_Brown2}") collabels("Baseline" "Brown" "Difference" "t-statistic" "N") addnote("Note: ") coeflabels(HighCorr_Relationship5 "Positive Correlation")

ttest Diff_Relationship4 if treatment_num==1 | treatment_num==3, by(treatment)
ttest Diff_Relationship5 if treatment_num==1 | treatment_num==3, by(treatment)


**# Regressions  
expand 2, gen(dup)
gen First_Round=1 if dup ==0
replace First_Round=0 if dup ==1
gen High_Corr = 1 if First_Round==1 & P1_low_first == 0
replace High_Corr = 0 if First_Round==1 & P1_low_first == 1
replace High_Corr = 1 if First_Round==0  & P1_low_first == 1
replace High_Corr = 0 if First_Round==0  & P1_low_first == 0

gen Low_Corr = 0 if First_Round==1 & P1_low_first == 0
replace Low_Corr = 1 if First_Round==1 & P1_low_first == 1
replace Low_Corr = 0 if First_Round==0  & P1_low_first == 1
replace Low_Corr = 1 if First_Round==0  & P1_low_first == 0 

gen Share_lowreturn= P1_investmentAsset_lowreturn if First_Round==1
replace Share_lowreturn= P2_investmentAsset_lowreturn if First_Round==0 
replace Share_lowreturn= (Share_lowreturn/10000)*100

label var First_Round "First Round"
labe var Share_lowreturn "Share Diversification Asset"
label var High_Corr "Positive Correlation"
label var Low_Corr "Negative Correlation"

*Green 
reg Share_lowreturn ib1.treatment_num##High_Corr if treatment_num<3,  cluster(id)
eststo green_diversif
*With controls
reg Share_lowreturn ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare if treatment_num<3, cluster(id) 

*With comprehension and dependence controls
reg Share_lowreturn ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence if treatment_num<3,  cluster(id)
eststo green_diversif_controls


*With controls and  correlation order effects
reg Share_lowreturn ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences  Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr if treatment_num<3, cluster(id)
eststo green_diversif_ordereffects

 
*With controls, correlation order effects, and asset order effects
reg Share_lowreturn ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr P1_high_return if treatment_num<3,  cluster(id)
eststo green_diversif_asseteffects


esttab green_diversif green_diversif_controls green_diversif_ordereffects green_diversif_asseteffects using Regression_Green_Main_table.tex, title ("") b(%9.2f) se(%9.2f) label replace booktabs alignment(c) mgroups("" "", pattern(1 1 )) mtitles("Share Diversification Asset" "Share Diversification Asset" "Share Diversification Asset" "Share Diversification Asset") star(* 0.10 ** 0.05 *** 0.01) r2


*Brown 
reg Share_lowreturn ib1.treatment_num##High_Corr if treatment_num==1 | treatment_num==3,  cluster(id)
eststo brown_diversif
*With controls
reg Share_lowreturn ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences  Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare  if treatment_num==1 | treatment_num==3,  cluster(id) 


*With comprehension and dependence controls
reg Share_lowreturn ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence if treatment_num==1 | treatment_num==3,  cluster(id)
eststo brown_diversif_controls

*With controls and  correlation order effects
reg Share_lowreturn ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences  Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy  Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr if treatment_num==1 | treatment_num==3,  cluster(id)
eststo brown_diversif_ordereffects

*With controls, correlation order effects, and asset order effects
reg Share_lowreturn ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr P1_high_return if treatment_num==1 | treatment_num==3, cluster(id)
eststo brown_diversif_asseteffects

esttab brown_diversif brown_diversif_controls brown_diversif_ordereffects brown_diversif_asseteffects using Regression_Brown_Main_table.tex, title ("") b(%9.2f) se(%9.2f) label replace booktabs alignment(c) mgroups("" "", pattern(1 1 )) mtitles("Share Diversification Asset" "Share Diversification Asset" "Share Diversification Asset" "Share Diversification Asset") star(* 0.10 ** 0.05 *** 0.01) r2

*Asymmetric Effects
*Focus on Negative Correlation Scenario
reg Share_lowreturn ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences  Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr P1_high_return  if treatment_num<3 
eststo green_asymmetrytest

reg Share_lowreturn ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences  Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr P1_high_return if treatment_num==1 | treatment_num==3
eststo brown_asymmetrytest

suest green_asymmetrytest brown_asymmetrytest, cluster(id)
test [green_asymmetrytest_mean]2.treatment_num = [brown_asymmetrytest_mean]3.treatment_num

test [green_asymmetrytest_mean]2.treatment_num#1.High_Corr = [brown_asymmetrytest_mean]3.treatment_num#1.High_Corr

*Focus on Average of Both Correlation Scenarios
*it's the same whether we control for high corr or not
reg Share_lowreturn ib1.treatment_num Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence P1_low_first P1_high_return if  treatment_num<3,  cluster(id)
eststo green_averageeffect

reg Share_lowreturn ib1.treatment_num Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence P1_low_first P1_high_return if treatment_num==1 | treatment_num==3,  cluster(id)
eststo brown_avergeeffect

*esttab green_averageeffect brown_avergeeffect using Regression_Asymmetric_GreenBrown_table.tex, title ("") b(%9.2f) se(%9.2f) label replace booktabs alignment(c) mgroups("" "", pattern(1 1 )) mtitles("Share Diversification Asset" "Share Diversification Asset" ) star(* 0.10 ** 0.05 *** 0.01) r2

reg Share_lowreturn ib1.treatment_num Age Gender High_wealth Patient Risk_taking Social_Preferences  Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence P1_low_first P1_high_return if  treatment_num<3 
eststo green_avgeffect_asymmetry

reg Share_lowreturn ib1.treatment_num Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence P1_low_first P1_high_return if treatment_num==1 | treatment_num==3 
eststo brown_avgeffect_asymmetry


suest green_avgeffect_asymmetry brown_avgeffect_asymmetry, cluster(id)
test [green_avgeffect_asymmetry_mean]2.treatment_num = [brown_avgeffect_asymmetry_mean]3.treatment_num

gen Expect_value= P1_Expect_value if First_Round==1
replace Expect_value= P2_Expect_value if First_Round==0 

gen Lower_Bound= P1_belief_lower if First_Round==1
replace Lower_Bound= P2_belief_lower if First_Round==0 

gen Upper_Bound= P1_belief_upper if First_Round==1
replace Upper_Bound= P2_belief_upper if First_Round==0 


*Green
reg Expect_value ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr P1_high_return if treatment_num<3, cluster(id)
eststo Expected_Value_Green

reg Lower_Bound ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr P1_high_return if treatment_num<3, cluster(id)
eststo Lower_Bound_Green

reg Upper_Bound ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr P1_high_return if treatment_num<3, cluster(id)
eststo Upper_Bound_Green

*Brown 
reg Expect_value ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr P1_high_return if treatment_num==1 | treatment_num==3, cluster(id)
eststo Expected_Value_Brown

reg Lower_Bound ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr P1_high_return if treatment_num==1 | treatment_num==3, cluster(id)
eststo Lower_Bound_Brown

reg Upper_Bound ib1.treatment_num##High_Corr Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence First_Round##High_Corr P1_high_return if treatment_num==1 | treatment_num==3, cluster(id)
eststo Upper_Bound_Brown

esttab Lower_Bound_Green Upper_Bound_Green Expected_Value_Green Lower_Bound_Brown Upper_Bound_Brown  Expected_Value_Brown using Beliefs_Regression.tex, title ("Beliefs about Portfolio Value") b(%9.2f) se(%9.2f) label replace booktabs alignment(c) mgroups("" "", pattern(1 1 )) mtitles("Lower" "Upper" "Expected" "Lower" "Upper" "Expected") star(* 0.10 ** 0.05 *** 0.01) r2

drop if dup==1

*Correlation Neglect 
reg correlation_neglect ib1.treatment_num if treatment_num<3,  vce(rob)
eststo corr_neglect_greennocontrol

reg correlation_neglect ib1.treatment_num Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence P1_low_first P1_high_return if treatment_num<3,  vce(rob)
eststo corr_neglect_green 

reg correlation_neglect ib1.treatment_num if treatment_num==1 | treatment_num==3,  vce(rob)
eststo corr_neglect_brownnocontrol

reg correlation_neglect ib1.treatment_num Age Gender High_wealth Patient Risk_taking Social_Preferences Donation Sustainability_Value numcorrect_financialliteracyall correct_Environ_Literacy Stat_know Number_think Number_inter Own_asset Intere_inves Trade_frequently High_CurrentSustainShare correct_comprehension correct_dependence P1_low_first P1_high_return if treatment_num==1 | treatment_num==3,  vce(rob)
eststo corr_neglect_brown

esttab corr_neglect_greennocontrol corr_neglect_green corr_neglect_brownnocontrol corr_neglect_brown using Correlation_Neglect_Regression.tex, title ("Correlation Neglect Across Treatments") b(%9.2f) se(%9.2f) label replace booktabs alignment(c) mgroups("" "", pattern(1 1 1 1)) mtitles("Correlation Neglect" "Correlation Neglect" "Correlation Neglect" "Correlation Neglect") star(* 0.10 ** 0.05 *** 0.01) r2
