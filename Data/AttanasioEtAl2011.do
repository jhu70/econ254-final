* This do file runs the logits and generates the descriptives presented in Attenasio et al 2011.
* It makes use of two data files:
*   - AttanasioEtAl2011Dyadic.dta
*   - AttanasioEtAl2011Vector.dta
* The do file does the following:
* 1. it generates the dyadic descriptives for Table 4
* 2. it runs the cross tab used to generate Figure 1
* 3. it runs the dyadic regressions presented in Table 5
* 4. it runs a variety of robustness checks for the dyadic regressions in Table 5
* 5. it constructs a variable from the dyadic data to be used in the group-levle analysis presented in Appendix Table A1
* 6. it generates the subject descriptives for Table 1
* 7. it generates the summary of the experimental data for Table 3
* 8. it runs the regressions presented in Appendix Table A1

capture log close
clear
clear matrix
set matsize 300
set mem 500m
set maxvar 20000
set more off

use AttanasioEtAl2011Dyadic.dta, clear

forvalues i = 1(1)70 {
	generate mdum`i'=1 if municode==`i'
	replace mdum`i'=0 if municode!=`i'
}

**************************************************************************
*				  Table 4 Dyadic characteristics
**************************************************************************

sum samegroup difchoice1 sumchoice1 frfam friend2 friendfamily family2 friend1 family1 frfamcl difurban diffemale difyage difysch difmar diftcons difcons difhhsize difwin1 sumurban sumfemale sumyage sumysch nummar sumcons sumtcons sumhhsize sumwin1 if inreg==1
sum samegroup difchoice1 sumchoice1 frfam friend2 friendfamily family2 friend1 family1 frfamcl difurban diffemale difyage difysch difmar diftcons difcons difhhsize difwin1 sumurban sumfemale sumyage sumysch nummar sumtcons sumcons sumhhsize sumwin1 if inreg==1 & frfamcl==1
sum samegroup difchoice1 sumchoice1 frfam friend2 friendfamily family2 friend1 family1 frfamcl difurban diffemale difyage difysch difmar diftcons difcons difhhsize difwin1 sumurban sumfemale sumyage sumysch nummar sumtcons sumcons sumhhsize sumwin1 if inreg==1 & frfamcl==0

**********************************************************************
*			      Figure 1
**********************************************************************

tab difchoice1 samegroup if inreg==1 & frfamcl==1, row chi
tab difchoice1 samegroup if inreg==1 & frfamcl==0, row chi

**********************************************************************
*			      Table 5
**********************************************************************

*Table 5 col 1
logit samegroup difchoice1 frfamcl dcfrfamcl mdum*, robust cluster(municode)
test difchoice1+dcfrfamcl=0
mfx, varlist(difchoice1 frfamcl dcfrfamcl)

*Table 5 col 2
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 frfamcl dcfrfamcl difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum*, robust cluster(municode)
mfx, varlist(difchoice1 frfamcl dcfrfamcl friend2 friendfamily family2 friend1 family1)

*Table 5 col 3
logit samegroup difchoice1 friend2 friendfamily family2 friend1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==1, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1)

*Table 5 col 4
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==0, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1 family1)

*Table 5 col 5 (adding friends and family options variables)
gen dcmaxgpffopt=maxgpffopt*difchoice1 
logit samegroup difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==0, robust cluster(municode)
mfx, var(difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1)

**********************************************************************
*      Table 5 rerun on dyads in which both members chose to group 
*             with at least one other person (footnote 13)
**********************************************************************

*col 1
logit samegroup difchoice1 frfamcl dcfrfamcl mdum* if grpmem>1, robust cluster(municode)
test difchoice1+dcfrfamcl=0
mfx, varlist(difchoice1 frfamcl dcfrfamcl)

*col 2
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 frfamcl dcfrfamcl difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if grpmem>1, robust cluster(municode)
mfx, varlist(difchoice1 frfamcl dcfrfamcl friend2 friendfamily family2 friend1 family1)

*col 3
logit samegroup difchoice1 friend2 friendfamily family2 friend1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==1 & grpmem>1, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1)

*col 4
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==0 & grpmem>1, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1 family1)

*col 5 (adding friends and family options variables)
logit samegroup difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==0 & grpmem>1, robust cluster(municode)
mfx, var(difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1)

**********************************************************************
*			      Table E (report on bias in odds)
**********************************************************************

*Table E col 1
logit samegroup difchoice1 frfamcl dcfrfamcl mdum* if win1a<=3000 & win1b<=3000, robust cluster(municode)
test difchoice1+dcfrfamcl=0
mfx, varlist(difchoice1 frfamcl dcfrfamcl)

*Table E col 2
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 frfamcl dcfrfamcl difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if win1a<=3000 & win1b<=3000, robust cluster(municode)
test difchoice1+dcfrfamcl=0
mfx, varlist(difchoice1 frfamcl dcfrfamcl friend2 friendfamily family2 friend1 family1)

*Table E col 3
logit samegroup difchoice1 friend2 friendfamily family2 friend1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==1 & win1a<=3000 & win1b<=3000, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1)

*Table E col 4
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==0 & win1a<=3000 & win1b<=3000, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1 family1)

*Table 5 col 5 (adding friends and family options variables)
logit samegroup difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==0 & win1a<=3000 & win1b<=3000, robust cluster(municode)
mfx, var(difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1)

**********************************************************************
*			      Table F (report on bias in odds)
**********************************************************************

*Table F col 1
logit samegroup difchoice1 frfamcl dcfrfamcl mdum* if probm==0, robust cluster(municode)
test difchoice1+dcfrfamcl=0
mfx, varlist(difchoice1 frfamcl dcfrfamcl)

*Table F col 2
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 frfamcl dcfrfamcl difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if probm==0, robust cluster(municode)
test difchoice1+dcfrfamcl=0
mfx, varlist(difchoice1 frfamcl dcfrfamcl friend2 friendfamily family2 friend1 family1)

*Table F col 3
logit samegroup difchoice1 friend2 friendfamily family2 friend1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==1 & probm==0, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1)

*Table F col 4
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==0 & probm==0, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1 family1)

*Table F col 5 (adding friends and family options variables)
logit samegroup difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1 mdum* if frfamcl==0 & probm==0, robust cluster(municode)
mfx, varlist(difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1)

**********************************************************************
*	  Rerunning Table 5 columns 2-5 excluding difwin1 and sumwin1 
*               (mentioned in report on biased odds)
**********************************************************************

*col 2
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 frfamcl dcfrfamcl difurban diffemale difyage difysch difmarried difcons difhhsize sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize mdum*, robust cluster(municode)
test difchoice1+dcfrfamcl=0
mfx, varlist(difchoice1 frfamcl dcfrfamcl friend2 friendfamily family2 friend1 family1)
*col 3
logit samegroup difchoice1 friend2 friendfamily family2 friend1 difurban diffemale difyage difysch difmarried difcons difhhsize sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize mdum* if frfamcl==1, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1)
*col 4
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize  mdum* if frfamcl==0, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1 family1)
*col 5 (adding friends and family options variables)
logit samegroup difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize mdum* if frfamcl==0, robust cluster(municode)
mfx, var(difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1)

**************************************************************************
*Dyadic analysis of whether ff are more similar in terms of risk attitudes
*                      (footnote 12)
**************************************************************************

regress difchoice1 frfamcl, robust cluster(municode)
regress difchoice1 friend2 friendfamily family2 friend1 family1, robust cluster(municode)
regress difchoice1 friend2 friendfamily family2 friend1 family1 frfamcl, robust cluster(municode)
regress difchoice1 friend2 friendfamily family2 friend1 family1 frfamcl mdum*, robust cluster(municode)
regress difchoice1 friend2 friendfamily family2 friend1 family1 frfamcl difurban diffemale difyage difysch difmarried difcons difhhsize mdum*, robust cluster(municode)

*Dyads in which both recognized a friendship are more similar in terms of round 1 gamble choices
*So, rerun of Table 5 excluding dyads in which both recognized a friendship

*col 1 excluding dyads in which both recognized a friendship
logit samegroup difchoice1 frfamcl dcfrfamcl mdum* if friend2==0, robust cluster(municode)
test difchoice1+dcfrfamcl=0
mfx, varlist(difchoice1 frfamcl dcfrfamcl)

*col 2 excluding dyads in which both recognized a friendship
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 frfamcl dcfrfamcl difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if friend2==0, robust cluster(municode)
mfx, varlist(difchoice1 frfamcl dcfrfamcl friend2 friendfamily family2 friend1 family1)

*col 3 excluding dyads in which both recognized a friendship
logit samegroup difchoice1 friend2 friendfamily family2 friend1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==1 & friend2==0, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1)

*col 4 excluding dyads in which both recognized a friendship
logit samegroup difchoice1 friend2 friendfamily family2 friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==0 & friend2==0, robust cluster(municode)
mfx, varlist(difchoice1 friend2 friendfamily family2 friend1 family1)

*col 5 excluding dyads in which both recognized a friendship
logit samegroup difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1 difurban diffemale difyage difysch difmarried difcons difhhsize difwin1 sumchoice1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize sumwin1  mdum* if frfamcl==0 & friend2==0, robust cluster(municode)
mfx, var(difchoice1 maxgpffopt dcmaxgpffopt difgpffopt friend2 family2 friendfamily friend1 family1)

**************************************************************************
* Gen grpFFcl = density of close friends & family network within each group
*               used in Table A1, group-level analysis of defaults
**************************************************************************

gen idgroup=grpid if samegroup==1
sort idgroup
by idgroup: egen grpFFcl=mean(frfamcl) 
replace grpFFcl=. if samegroup==0

keep idgroup grpFFcl
collapse grpFFcl, by(idgroup)
save grpFFcl, replace

**************************************************************************
*			Table 1: Subjects
**************************************************************************

use AttanasioEtAl2011Vector.dta, clear

sum female yage ysch head married urbr_2 tcons lcons survhhsz familyoutdeg friendsoutdeg 
sum female yage ysch head married urbr_2 tcons lcons survhhsz familyoutdeg friendsoutdeg if insample==1

**************************************************************************
*			Table 3: Experimental data
**************************************************************************

tab choice1 
tab choice2
sum blue1 win1 group cogrpmem blue2 renegade renegadewin win2
tab choice1 if insample==1
tab choice2 if insample==1
sum blue1 win1 group cogrpmem blue2 renegade renegadewin win2 if insample==1

**************************************************************************
*			Table A1: Group-level analysis of defaults
**************************************************************************

drop if group==0

sort idgroup
by idgroup: egen pdefault=mean(renegadewin)
by idgroup: egen gch2=mean(choice2)
by idgroup: egen gfem=mean(female)
by idgroup: egen gage=mean(yage)
by idgroup: egen gurb=mean(urbr_2)
by idgroup: egen gysch=mean(ysch)
by idgroup: egen gmar=mean(married)
by idgroup: egen glcons=mean(lcons)
by idgroup: egen ghhsz=mean(survhhsz)

keep idgroup pdefault grpmem gch2 gfem gage gurb gysch gmar glcons ghhsz municode 
collapse pdefault grpmem gch2 gfem gage gurb gysch gmar glcons ghhsz municode, by(idgroup)
sort idgroup
merge idgroup using grpFFcl.dta

sum  pdefault grpFFcl grpmem
pwcorr pdefault grpFFcl grpmem, sig star(10)

tab pdefault grpFFcl if grpmem<=2, col
tab pdefault grpFFcl if grpmem<=3, col

xi: regress pdefault grpFFcl grpmem gch2 gfem gage gurb gysch gmar glcons ghhsz i.municode if grpmem<=3, cluster(municode)
xi: regress pdefault grpFFcl grpmem gch2 gfem gage gurb gysch gmar glcons ghhsz i.municode, cluster(municode)
gen ffclmem=grpFFcl*grpmem 
xi: regress pdefault grpFFcl grpmem ffclmem gch2 gfem gage gurb gysch gmar glcons ghhsz i.municode, cluster(municode)
