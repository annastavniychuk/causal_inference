clear all
set matsize 11000
set maxvar 20000
* choose version 13 so that the randomization seeds are identical as when randomization was conducted
version 13

** Name: Randomization.do
** Date Created: 10/23/2017 by Vincent Pons (vpons@hbs.edu)
** Last Updated: 

* Data In: [base_randomization]
* Data Out: [randomization_precinct&municipality]

* Purpose of do-file: Randomization

* Organization :
/* 
*** PART I *** Prepare randomization database
*** PART II *** Precinct-level randomization
*** PART III *** Municipality-level randomization
*** PART IV *** Append precinct-level and municipality-level randomization
*/

* Setting file path

cd "${data}"

****************************************
*** PART I *** Prepare randomization database
****************************************

use "Original\base_randomization", clear

** define TA, target_ter number of registered citizens for each territory (used as threshold in randomization, see below)

* compute nb_registered_ter and nb_leftabstention_ter

so municipality_code 
by municipality_code: gen id = _n
gen temp = nb_registered_mun if id == 1
gen temp2 = nb_leftabstention_mun if id == 1
so departement_code territory
by departement_code territory: egen nb_registered_ter = total(temp) 
by departement_code territory: egen nb_leftabstention_ter = total(temp2)

* compute prop_leftabstention_ter

gen prop_leftabstention_ter = nb_leftabstention_ter / nb_registered_ter 

* define target number of registered citizens for each territory, proportional to total number of registered citizens in the territory and prop_leftabstention_ter (proxy for the potential to win votes)

egen nb_leftabstention_France = total(temp2)
gen target_ter = round(3/4 * 5000000 * prop_leftabstention_ter * nb_registered_ter / nb_leftabstention_France)
drop id temp* nb_leftabstention_France nb_leftabstention_ter prop_abstention prop_left prop_leftabstention_ter nb_leftabstention_mun
label var target_ter "target number of registered citizens in the territory"


** predict prop_leftabstention in precincts for which it is missing

* define additional predictors

gen avg_bldg_size_prim2 = avg_bldg_size_prim * avg_bldg_size_prim
gen avg_age_prim2 = avg_age_prim * avg_age_prim
ta departement_code, gen(dep_id)

* predict prop_leftabstention in precincts where it is missing and all voter rolls variables are known

reg prop_leftabstention dep_id1 - dep_id99 avg_bldg_size_prim avg_bldg_size_prim2 share_bldg_inf5reg_prim share_bldg_5to15reg_prim avg_age_prim avg_age_prim2 share_reg_inf25_prim share_reg_sup65_prim
predict prop_abstentiongauche1
replace prop_leftabstention = prop_abstentiongauche1 if prop_leftabstention == . & level_observation == 1
drop prop_abstentiongauche1

* predict prop_leftabstention in precincts where it is missing and a subset of voter rolls variables are known

reg prop_leftabstention dep_id1 - dep_id99 avg_bldg_size_prim avg_bldg_size_prim2 share_bldg_inf5reg_prim share_bldg_5to15reg_prim
predict prop_abstentiongauche2
replace prop_leftabstention = prop_abstentiongauche2 if prop_leftabstention == . & level_observation == 1
drop prop_abstentiongauche2 
drop dep_id* avg_bldg_size_prim avg_bldg_size_prim2 share_bldg_inf5reg_prim share_bldg_5to15reg_prim share_bldg_sup15reg_prim avg_age_prim avg_age_prim2 share_reg_inf25_prim share_reg_sup65_prim


** define whether randomization should be conducted at precinct or municipality level

/* To decide whether randomization should be conducted at precinct or municipality level, I count the number of registered citizens in precincts of the 
territory that were present in the PS' 2011 primary elections database. I randomize at the precinct level if this number is higher than the total number 
of registered citizens in the territory.*/

* define level of randomization

gen temp = nb_registered_prim if level_observation == 1
so departement_code territory
by departement_code territory: egen nb_registered_prim_ter = total(temp)
gen level_randomization = (nb_registered_prim_ter / nb_registered_ter > 0.5)
drop temp nb_registered_prim_ter nb_registered_ter
label var level_randomization "level at which the randomization will be conducted in the territory"
label define level_randomization 1 "precinct" 0 "municipality"
label values level_randomization level_randomization

* in territories randomized at municipality level, only keep 1 obs per municipality (relevant for municipalities for which precincts were present in the PS' 2011 primary elections database)

so municipality_code
by municipality_code: gen id = _n
drop if level_randomization == 0 & id ~= 1
drop id
replace level_observation = 0 if level_randomization == 0

* in territories randomized at municipality level, drop precinct-level variables

replace precinct_code = "" if level_randomization == 0
replace precinct = "" if level_randomization == 0
replace prop_leftabstention = . if level_randomization == 0
replace nb_registered_prim = . if level_randomization == 0

* in territories randomized at precinct level, drop municipalities for which voter registers had not been collected by the PS (and whose precincts are not present in the PS' 2011 primary elections database)

drop if level_observation == 0 & level_randomization == 1
drop level_observation


** count the number of precincts or municipalities in each territory

/* In territories counting a unique municipality, that municipality had to be allocated to the canvassers in any
case (so that they could participate in the door-to-door campaign); this territory was thus not included in the randomization*/

so departement_code territory
by departement_code territory: gen nb_units = _N
gen territory_excluded = (nb_units == 1)
ta level_randomization if territory_excluded == 1
label var territory_excluded "territory excluded from the randomization (only 1 municipality)"
drop nb_units

drop zip_code
order departement_code - precinct level_randomization territory_excluded target_ter nb_registered_mun prop_leftabstention_mun nb_registered_prim prop_leftabstention 

save "Intermediate\base_randomization_v2", replace


****************************************
*** PART II *** Precinct-level randomization
****************************************

use "Intermediate\base_randomization_v2", clear

** keep territories randomized at precinct level

keep if level_randomization == 1 


** stratification

/* The territory's first stratum comprised the five precincts with the highest PO, the second stratum
the five precincts ranked immediately below, and so on until the last stratum, composed
of the five or fewer remaining precincts*/

gsort departement_code territory -prop_leftabstention
by departement_code territory: gen rank = _n
gen stratum = ceil(rank / 5)
drop rank
label var stratum "stratum identifier within territory"

so departement_code territory stratum
by departement_code territory stratum: gen stratum_size = _N
by departement_code territory: egen nb_strata = max(stratum)


** generate random number

so municipality_code precinct_code
set seed 32446882
gen rand = uniform()
so departement_code territory stratum rand
by departement_code territory stratum: gen rand2 = _n


** define number of strata to include in the randomization: 

/* The first stratum of any territory was always included in the randomization*/

gen nb_strata_included = 1
label var nb_strata_included "number of strata included in the randomization"


** randomization of first stratum

/* When the first stratum included five precincts, exactly four
(80 percent) of these precincts were randomly assigned to the treatment group, and one
(20 percent) to the control group.*/

gen treatment = 0 if stratum == 1 & stratum_size == 5 & rand2 == 5
replace treatment = 1 if stratum == 1 & stratum_size == 5 & rand2 ~= 5

/* When the first stratum included fewer than five precincts, each
precinct was assigned with an 80 percent probability to the treatment group and with a 20
percent probability to the control group*/

replace treatment = 0 if stratum == 1 & stratum_size ~= 5 & rand <= 0.2
replace treatment = 1 if stratum == 1 & stratum_size ~= 5 & rand > 0.2
label var treatment "treatment assignment"


** decision to include additional strata, and randomization of these strata

forvalues i=2(1)15 {

* count the total number of registered citizens in the treatment precincts of strata already included in the randomization

gen nb_registered_treatment`i' = nb_registered_prim if treatment == 1
so departement_code territory 
by departement_code territory: egen temporary_count`i' = total(nb_registered_treatment`i')

* add a new stratum if this total remains lower than TA

replace nb_strata_included = `i' if temporary_count`i' < target_ter & nb_strata >= `i'

* randomize this new stratum if it is included

replace treatment = 0 if nb_strata_included == `i' & stratum == `i' & stratum_size == 5 & rand2 == 5
replace treatment = 1 if nb_strata_included == `i' & stratum == `i' & stratum_size == 5 & rand2 ~= 5
replace treatment = 0 if nb_strata_included == `i' & stratum == `i' & stratum_size ~= 5 & rand <= 0.2
replace treatment = 1 if nb_strata_included == `i' & stratum == `i' & stratum_size ~= 5 & rand > 0.2
drop nb_registered_treatment`i' temporary_count`i'
}
drop rand rand2


** define the list of precincts which canvassers will be asked to cover

* none of the control precinct is allocated to canvassers

gen allocated = 0 if treatment == 0
label var allocated "precinct or municipality allocated to canvassers"

* all treatment precincts of the inframarginal strata are allocated to canvassers

replace allocated = 1 if treatment == 1 & stratum < nb_strata_included

* marginal stratum: 

/* In this stratum, the treatment precinct with the largest potential PO was always allocated to the canvassers. 
If the number of citizens registered in this precinct and in treatment precincts of inframarginal strata was larger than TA, 
no other treatment precinct was allocated to the canvassers. If its number of registered citizens was lower than TA, 
the treatment precinct with the second largest PO was also allocated to the canvassers, etc.*/

* rank the treatment precincts of the marginal stratum by decreasing PO

gsort departement_code territory stratum treatment -prop_leftabstention
by departement_code territory stratum treatment: gen rank_treatment_marginal = _n if stratum == nb_strata_included & treatment == 1

* allocate the treatment precinct of the marginal stratum with the highest PO to the canvassers

replace allocated = 1 if treatment == 1 & stratum == nb_strata_included & rank_treatment_marginal == 1

* allocation decision for other treatment precincts of the marginal stratum

forvalues i=2(1)4 {

* count the total number of registered citizens in the treatment precincts already allocated to canvassers

gen nb_registered_treatment`i' = nb_registered_prim if allocated == 1
so departement_code territory 
by departement_code territory: egen temporary_count`i' = total(nb_registered_treatment`i')

* allocate the "next" treatment precinct (with the 2nd / 3rd / 4th largest PO) of the marginal stratum to the canvassers if and only if this total remains lower than TA

replace allocated = 1 if temporary_count`i' < target_ter & rank_treatment_marginal == `i'
replace allocated = 0 if temporary_count`i' >= target_ter & rank_treatment_marginal == `i'
drop nb_registered_treatment`i' temporary_count`i'
}
drop rank_treatment_marginal 


** identify the smallest set of strata (below: "smallest set") of each territory which, based on the randomization rule, would be included in the randomization under any possible treatment assignment in lower-numbered strata

/*The first stratum of each territory always falls in this set. The second stratum also falls in this set if,
in the event that the smallest precinct of the first stratum was assigned by chance to the control group, the
total number of registered citizens in the treatment precincts would remain lower than TA. And so on for
the subsequent strata.*/

* the first stratum of any territory always falls in the "smallest set"

gen min_nb_strata_included = 1

* identify other strata that fall in "smallest set"

/* When the last stratum that falls in the "smallest set" had fewer than 5 observations, it is by construction the last in the territory, and no other stratum can fall in this set.
I thus focus on cases where the last stratum that falls in "smallest set" had 5 observations */

* hypothetical assignment where the smallest precinct of the first stratum is assigned to control

gsort departement_code territory stratum -nb_registered_prim
by departement_code territory stratum: gen rank_size = _n

gen hypothetical_assignment = 0 if stratum == 1 & stratum_size == 5 & rank_size == 5
replace hypothetical_assignment = 1 if stratum == 1 & stratum_size == 5 & rank_size ~= 5

* identify other strata that fall in the "smallest set"

forvalues i=2(1)15 {

* count the total number of registered citizens in the precincts hypothetically allocated to treatment in strata already falling in the "smallest set"

gen nb_registered_hyp`i' = nb_registered_prim if hypothetical_assignment == 1
so departement_code territory 
by departement_code territory: egen temporary_count`i' = total(nb_registered_hyp`i')

* a new stratum falls in the "smallest set" if this total remains lower than TA

replace min_nb_strata_included = `i' if temporary_count`i' < target_ter & nb_strata >= `i'

* hypothetical allocation to treatment in this new stratum if it falls in the "smallest set"

replace hypothetical_assignment = 0 if min_nb_strata_included == `i' & stratum == `i' & stratum_size == 5 & rank_size == 5
replace hypothetical_assignment = 1 if min_nb_strata_included == `i' & stratum == `i' & stratum_size == 5 & rank_size ~= 5
drop nb_registered_hyp`i' temporary_count`i'
}
drop rank_size hypothetical_assignment  

* define the smallest set of strata of each territory which, based on the randomization rule, would be included in the randomization under any possible treatment assignment in lower-numbered strata

gen smallest_set_strata = (stratum <= min_nb_strata_included)
label var smallest_set_strata "smallest set of strata which would be included in the randomization"


** drop strata not included in the randomization

drop if stratum > nb_strata_included


** define sample: all remaining observations

gen sample = 1
label var sample "randomization sample"
drop stratum_size nb_strata min_nb_strata_included   
drop target_ter

save "Intermediate\randomization_precinct", replace


****************************************
*** PART III *** Municipality-level randomization
****************************************

use "Intermediate\base_randomization_v2", clear


** keep territories randomized at municipality level

keep if level_randomization == 0


** stratification

/* The territory's first stratum comprised the five municipalities with the highest PO, the second stratum
the five municipalities ranked immediately below, and so on until the last stratum, composed
of the five or fewer remaining municipalities*/

gsort departement_code territory -prop_leftabstention_mun
by departement_code territory: gen rank = _n
gen stratum = ceil(rank / 5)
drop rank
label var stratum "stratum identifier within territory"

so departement_code territory stratum
by departement_code territory stratum: gen stratum_size = _N
by departement_code territory: egen nb_strata = max(stratum)


** generate random number

so municipality_code
set seed 32446893
gen rand = uniform()
so departement_code territory stratum rand
by departement_code territory stratum: gen rand2 = _n


** define number of strata to include in the randomization: 

/* The first stratum of any territory was always included in the randomization*/
gen nb_strata_included = 1 if territory_excluded ~= 1
label var nb_strata_included "number of strata included in the randomization"


** randomization of first stratum

/* When the first stratum included five municipalities, exactly four
(80 percent) of these municipalities were randomly assigned to the treatment group, and one
(20 percent) to the control group.*/

gen treatment = 0 if territory_excluded ~= 1 & stratum == 1 & stratum_size == 5 & rand2 == 5
replace treatment = 1 if territory_excluded ~= 1 &stratum == 1 & stratum_size == 5 & rand2 ~= 5

/* When the first stratum included fewer than five municipalities, each
municipalitiy was assigned with an 80 percent probability to the treatment group and with a 20
percent probability to the control group*/

replace treatment = 0 if territory_excluded ~= 1 & stratum == 1 & stratum_size ~= 5 & rand <= 0.2
replace treatment = 1 if territory_excluded ~= 1 & stratum == 1 & stratum_size ~= 5 & rand > 0.2
label var treatment "treatment assignment"


** decision to include additional strata, and randomization of these strata

forvalues i=2(1)15 {

* count the total number of registered citizens in the treatment municipalities of strata already included in the randomization

gen nb_registered_treatment`i' = nb_registered_mun if treatment == 1
so departement_code territory 
by departement_code territory: egen temporary_count`i' = total(nb_registered_treatment`i')

* add a new stratum if this total remains lower than TA

replace nb_strata_included = `i' if territory_excluded ~= 1 & temporary_count`i' < target_ter & nb_strata >= `i'

* randomize this new stratum if it is included

replace treatment = 0 if territory_excluded ~= 1 & nb_strata_included == `i' & stratum == `i' & stratum_size == 5 & rand2 == 5
replace treatment = 1 if territory_excluded ~= 1 & nb_strata_included == `i' & stratum == `i' & stratum_size == 5 & rand2 ~= 5
replace treatment = 0 if territory_excluded ~= 1 & nb_strata_included == `i' & stratum == `i' & stratum_size ~= 5 & rand <= 0.2
replace treatment = 1 if territory_excluded ~= 1 & nb_strata_included == `i' & stratum == `i' & stratum_size ~= 5 & rand > 0.2
drop nb_registered_treatment`i' temporary_count`i'
}
drop rand rand2


** define the list of municipalities which canvassers would be asked to cover

* municipalities in territories counting a unique municipality are allocated to canvassers

gen allocated = 1 if territory_excluded == 1
label var allocated "precinct or municipality allocated to canvassers"

* none of the control municipality is allocated to canvassers

replace allocated = 0 if territory_excluded ~= 1 & treatment == 0

* all treatment municipalities of the inframarginal strata are allocated to canvassers

replace allocated = 1 if territory_excluded ~= 1 & treatment == 1 & stratum < nb_strata_included

* marginal stratum: 

/* In this stratum, the treatment municipality with the largest potential PO was always allocated to the canvassers. 
If the number of citizens registered in this municipality and in treatment municipalities of inframarginal strata was larger than TA, 
no other treatment municipality was allocated to the canvassers. If its number of registered citizens was lower than TA, 
the treatment municipality with the second largest PO was also allocated to the canvassers, etc.*/

* rank the treatment municipalities of the marginal stratum by decreasing PO

gsort departement_code territory stratum treatment -prop_leftabstention_mun
by departement_code territory stratum treatment: gen rank_treatment_marginal = _n if stratum == nb_strata_included & treatment == 1

* allocate the treatment municipality of the marginal stratum with the highest PO to the canvassers

replace allocated = 1 if territory_excluded ~= 1 & treatment == 1 & stratum == nb_strata_included & rank_treatment_marginal == 1

* allocation decision for other treatment municipalities of the marginal stratum

forvalues i=2(1)4 {

* count the total number of registered citizens in the treatment municipalities already allocated to canvassers

gen nb_registered_treatment`i' = nb_registered_mun if allocated == 1
so departement_code territory 
by departement_code territory: egen temporary_count`i' = total(nb_registered_treatment`i')

* allocate the "next" treatment municipality (with the 2nd / 3rd / 4th largest PO) of the marginal stratum to the canvassers if and only if this total remains lower than TA

replace allocated = 1 if territory_excluded ~= 1 & temporary_count`i' < target_ter & rank_treatment_marginal == `i'
replace allocated = 0 if territory_excluded ~= 1 & temporary_count`i' >= target_ter & rank_treatment_marginal == `i'
drop nb_registered_treatment`i' temporary_count`i'
}
drop rank_treatment_marginal 


** identify the smallest set of strata (below: "smallest set") of each territory which, based on the randomization rule, would be included in the randomization under any possible treatment assignment in lower-numbered strata

/*The first stratum of each territory always falls in this set. The second stratum also falls in this set if,
in the event that the smallest municipality of the first stratum was assigned by chance to the control group, the
total number of registered citizens in the treatment municipalities would remain lower than TA. And so on for
the subsequent strata.*/

* the first stratum of any territory always falls in the "smallest set"

gen min_nb_strata_included = 1 if territory_excluded ~= 1

* identify other strata that fall in "smallest set"

/* When the last stratum that falls in the "smallest set" had fewer than 5 observations, it is by construction the last in the territory, and no other stratum can fall in this set.
I thus focus on cases where the last stratum that falls in "smallest set" had 5 observations */

* hypothetical assignment where the smallest municipality of the first stratum is assigned to control

gsort departement_code territory stratum -nb_registered_mun
by departement_code territory stratum: gen rank_size = _n

gen hypothetical_assignment = 0 if territory_excluded ~= 1 & stratum == 1 & stratum_size == 5 & rank_size == 5
replace hypothetical_assignment = 1 if territory_excluded ~= 1 & stratum == 1 & stratum_size == 5 & rank_size ~= 5

* identify other strata that fall in the "smallest set"

forvalues i=2(1)15 {

* count the total number of registered citizens in the municipalities hypothetically allocated to treatment in strata already falling in the "smallest set"

gen nb_registered_hyp`i' = nb_registered_mun if hypothetical_assignment == 1
so departement_code territory 
by departement_code territory: egen temporary_count`i' = total(nb_registered_hyp`i')

* a new stratum falls in the "smallest set" if this total remains lower than TA

replace min_nb_strata_included = `i' if territory_excluded ~= 1 & temporary_count`i' < target_ter & nb_strata >= `i'

* hypothetical allocation to treatment in this new stratum if it falls in the "smallest set"

replace hypothetical_assignment = 0 if territory_excluded ~= 1 & min_nb_strata_included == `i' & stratum == `i' & stratum_size == 5 & rank_size == 5
replace hypothetical_assignment = 1 if territory_excluded ~= 1 & min_nb_strata_included == `i' & stratum == `i' & stratum_size == 5 & rank_size ~= 5
drop nb_registered_hyp`i' temporary_count`i'
}
drop rank_size hypothetical_assignment  

* define the smallest set of strata of each territory which, based on the randomization rule, would be included in the randomization under any possible treatment assignment in lower-numbered strata

gen smallest_set_strata = (territory_excluded ~= 1 & stratum <= min_nb_strata_included)
label var smallest_set_strata "smallest set of strata which would be included in the randomization"


** drop strata not included in the randomization

drop if stratum > nb_strata_included & nb_strata_included ~= .


** define sample: all remaining observations except for territories not included in the randomization (as a result of counting a unique municipality)

gen sample = (territory_excluded ~= 1)
label var sample "randomization sample"
drop stratum_size nb_strata min_nb_strata_included   
drop target_ter

save "Intermediate\randomization_municipality", replace

****************************************
*** PART IV *** Append precinct-level and municipality-level randomization
****************************************

use "Intermediate\randomization_precinct", clear

append using "Intermediate\randomization_municipality"
save "Intermediate\randomization_precinct&municipality", replace
