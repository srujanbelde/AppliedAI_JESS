(import nrc.fuzzy.*)
(import nrc.fuzzy.jess.*)
(load-package FuzzyFunctions)

(deftemplate Person
    (slot isvalidincome) 
    (slot isvalidsavings) 
    (slot isvaliddebt) 
    (slot area) 
    (slot income) 
    (slot savings)
    (slot debt)
    (slot bed)
    (slot bath)
    (slot minbalance)
    (slot loanelegibility)
)
(deftemplate area
    (slot isvalidarea)
)
(deftemplate bed
    (slot isvalidbed)
)
(deftemplate bath
    (slot isvalidbath)
)
(deftemplate Validity 
    (slot valid)
)

(defglobal 
    ?*Area* = (new FuzzyVariable "Area" 0 2 "count")
	?*BedNo* = (new FuzzyVariable "BedNo" 1 4 "count")
	?*BathNo* = (new FuzzyVariable "BathNo" 0 3 "count")
	?*Income* = (new FuzzyVariable "Income" 0 99999999 "number")
	?*Savings* = (new FuzzyVariable "Savings" 0 99999999 "number")
	?*Assets* = (new FuzzyVariable "Asset" 0 99999999 "number")
	?*Debt* = (new FuzzyVariable "Debt" 0 99999999 "number")
	?*Budget* = 5000
    ?*TotalSavings* = 0
    ?*Area1MinIncome* = 10000
    ?*Area2MinIncome* = 5000
    ?*Beds* = 0
    ?*Baths* = 0
    ?*AreaClass* = 0
    ?*IncomeClass* =0
    ?*DebtClass* = 0
    ?*SavingsClass* = 0
    ?*AssetsClass* = 0
)

(defrule initialize-fuzzy-variables
    =>
    (assert(Validity(valid 1)))
    (?*Area* addTerm "Downtown" (new ZFuzzySet 0 1))
    (?*Area* addTerm "Suburb" (new ZFuzzySet 1 2))
    (?*Area* addTerm "Wrong" "not Downtown and (not Suburb)")
    (?*BedNo* addTerm "1BHK" (new ZFuzzySet 1 1))
    (?*BedNo* addTerm "2BHK" (new ZFuzzySet 2 2))
    (?*BedNo* addTerm "3BHK" (new ZFuzzySet 3 3))
    (?*BedNo* addTerm "4BHK" (new ZFuzzySet 4 4))
    (?*BedNo* addTerm "Wrong" "not 1BHK and (not 2BHK and (not 3BHK and (not 4BHK)))")
    (?*BathNo* addTerm "1Bath" (new ZFuzzySet 1 1))
    (?*BathNo* addTerm "2Bath" (new ZFuzzySet 2 2))
    (?*BathNo* addTerm "3Bath" (new ZFuzzySet 3 3))
    (?*BathNo* addTerm "Wrong" "not 1Bath and (not 2Bath and (not 3Bath))")
	(?*Income* addTerm "poor" (new ZFuzzySet 5000 99999))
    (?*Income* addTerm "rich" (new ZFuzzySet 99999 99999999))
    (?*Income* addTerm "insufficient" (new ZFuzzySet 0 5000))
    (?*Income* addTerm "veryrich" "extremely rich")
    (?*Income* addTerm "Wrong" "not poor and (not rich and (not insufficient and (not veryrich)))")
    (?*Savings* addTerm "low_savings" (new ZFuzzySet 0 999))
    (?*Savings* addTerm "high_savings" (new ZFuzzySet 999 9999))
    (?*Savings* addTerm "Wrong" "not low_savings and (not high_savings)")
    (?*Assets* addTerm "low_assets" (new ZFuzzySet 0 999))
    (?*Assets* addTerm "high_assets" (new ZFuzzySet 999 9999))
    (?*Assets* addTerm "Wrong" "not low_assets and (not high_assets)")
	(?*Debt* addTerm "low_debt" (new ZFuzzySet 0 999))
    (?*Debt* addTerm "high_debt" (new ZFuzzySet 999 9999))    
    (?*Debt* addTerm "Wrong" "not low_debt and (not high_debt)")
    
    ; In the below code change the parameters to get the desired result
;==========================================================================================================================================================    
    (assert (TheArea (new FuzzyValue ?*Area* "Suburb")))				;can enter the parameters as "Downtown" or "Suburb"
    (assert (TheBedNo (new FuzzyValue ?*BedNo* "1BHK")))				;can enter the parameters as "1BHK" "2BHK" "3BHK" "4BHK"
    (assert (TheBathNo (new FuzzyValue ?*BathNo* "1Bath")))				;can enter the parameters as "1Bath" "2Bath" "3Bath"
    (assert (TheIncome (new FuzzyValue ?*Income* "poor")))				;can enter the parameters as "rich" "poor" "very rich"
    (assert (TheSavings (new FuzzyValue ?*Savings* "low_savings")))		;can enter the parameters as "low_savings" or "high_savings"
    (assert (TheAsset (new FuzzyValue ?*Assets* "low_assets")))		;can enter the parameters as "low_asserts" or "high_asserts"
    (assert (TheDebt (new FuzzyValue ?*Debt* "high_debt")))				;can enter the parameters as "low_debts" or "high_debts"

;======================================================================================================================================================================================================================    
    
)

(defrule Area-Validity
    (TheArea ?ti&:(fuzzy-match ?ti "Wrong"))
    =>
    (printout t "You have selected the wrong input for area please verify" crlf)
    (assert(Validity(valid 0)))
    (assert(area(isvalidarea 0)))
)
(defrule Area-Setter1
    (TheArea ?ti&:(fuzzy-match ?ti "Downtown"))
    =>
    (bind ?*Area* 1)
)
(defrule Area-Setter2
    (TheArea ?ti&:(fuzzy-match ?ti "Suburb"))
    =>
    (bind ?*Area* 2)
)
(defrule BedNo-Validity
    (TheBedNo ?ti&:(fuzzy-match ?ti "Wrong"))
    =>
    (printout t "You have selected the wrong input for Number of Bedrooms please verify" crlf)
    (assert(Validity(valid 0)))
    (assert(bed(isvalidbed 0)))
)
(defrule BedNo-Setter1
    (TheBedNo ?ti&:(fuzzy-match ?ti "1BHK"))
    =>
    (bind ?*Beds* 1)
    (bind ?*Budget* 1000)
)
(defrule BedNo-Setter2
    (TheBedNo ?ti&:(fuzzy-match ?ti "2BHK"))
    =>
    (bind ?*Budget* 1200)
)
(defrule BedNo-Setter3
    (TheBedNo ?ti&:(fuzzy-match ?ti "3BHK"))
    =>
    (bind ?*Beds* 3)
    (bind ?*Budget* 1500)
)
(defrule BedNo-Setter4
    (TheBedNo ?ti&:(fuzzy-match ?ti "4BHK"))
    =>
    (bind ?*Beds* 4)
    (bind ?*Budget* 1750)
)
(defrule BathNo-Validity
    (TheBathNo ?ti&:(fuzzy-match ?ti "Wrong"))
    =>
    (printout t "You have selected the wrong input for Number of BathRooms please verify" crlf)
    (assert(Validity(valid 0)))
    (assert(bath(isvalidbath 0)))
)
(defrule BathNo-Setter1
    (TheBathNo ?ti&:(fuzzy-match ?ti "1Bath"))
    =>
    (bind ?*Baths* 1)
)
(defrule BathNo-Setter2
    (TheBathNo ?ti&:(fuzzy-match ?ti "2Bath"))
    =>
    (bind ?*Baths* 2)
)
(defrule BathNo-Setter3
    (TheBathNo ?ti&:(fuzzy-match ?ti "3Bath"))
    =>
    (bind ?*Baths* 3)
)
(defrule Income-Validity
    (TheIncome ?ti&:(fuzzy-match ?ti "Wrong"))
    =>
    (printout t "You have selected the wrong input for Income please verify" crlf)
    (assert(Validity(valid 0)))
    (assert(Person(isvalidincome 0)))
)
(defrule Income-setter1
    (TheIncome ?ti&:(fuzzy-match ?ti "poor"))
    =>
	(bind ?*IncomeClass* 1)
)
(defrule Income-setter2
    (TheIncome ?ti&:(fuzzy-match ?ti "rich"))
    =>
	(bind ?*IncomeClass* 2)
)
(defrule Income-setter3
    (TheIncome ?ti&:(fuzzy-match ?ti "veryrich"))
    =>
	(bind ?*IncomeClass* 3)
)
(defrule Savings-Validity
    (TheSavings ?ti&:(fuzzy-match ?ti "Wrong"))
    =>
    (printout t "You have selected the wrong input for savings please verify" crlf)
    (assert(Validity(valid 0)))
    (assert(Person(isvalidsavings 0)))
)
(defrule Savings-setter1
    (TheSavings ?ti&:(fuzzy-match ?ti "low_savings"))
    =>
    (bind ?*SavingsClass* 1)
    (assert(Person(isvalidsavings 1)))
)
(defrule Savings-setter2
    (TheSavings ?ti&:(fuzzy-match ?ti "high_savings"))
    =>
	(bind ?*SavingsClass* 2)
    (assert(Person(isvalidsavings 1)))
)
(defrule Asset-Validity
    (TheAsset ?ti&:(fuzzy-match ?ti "Wrong"))
    =>
    (printout t "You have selected the wrong input for assets please verify" crlf)
    (assert(Validity(valid 0)))
)
(defrule Asset-setter1
    (TheAsset ?ti&:(fuzzy-match ?ti "low_assets"))
    =>
	(bind ?*AssetsClass* 1)
)
(defrule Asset-setter2
    (TheAsset ?ti&:(fuzzy-match ?ti "high_assets"))
    =>
    (bind ?*AssetsClass* 2)
)
(defrule Debt-Validity
    (TheDebt ?ti&:(fuzzy-match ?ti "Wrong"))
    =>
    (printout t "You have selected the wrong input for debt please verify" crlf)
    (assert(Validity(valid 0)))
)
(defrule Debt-setter1
    (TheDebt ?ti&:(fuzzy-match ?ti "low_debt"))
    =>
	(bind ?*DebtClass* 1)
)
(defrule Debt-setter2
    (TheDebt ?ti&:(fuzzy-match ?ti "high_debt"))
    =>
    (bind ?*DebtClass* 2)
)
(defrule Validity
    (Validity{valid == 0})
    =>
    (printout t "Because of these errors the system is not able to process the rate." crlf)
)
(defrule FinalPrice
	(Validity{valid == 1})
    =>
    (bind ?*Budget*(* ?*Budget* (* ?*IncomeClass* ?*Area*)))
    ;(printout t ?*Budget* crlf)
	(bind ?*Budget* (+ ?*Budget* (* ?*Beds* 1000)))
    ;(printout t ?*Budget* crlf)
    (bind ?*Budget* (+ ?*Budget* (* ?*Baths* 500)))
    ;(printout t ?*Budget* crlf)
    (printout t "Your friendly budget will be:" ?*Budget* crlf)
)

(defrule LoanEligibility
	(Validity{valid == 1})
    =>
    (bind ?*TotalSavings* (+ ?*SavingsClass* ?*AssetsClass*))
    (bind ?*TotalSavings* (- ?*TotalSavings* ?*DebtClass*))
    ;(printout t  ?*SavingsClass* ?*AssetsClass* ?*DebtClass* ?*TotalSavings*  crlf)
    (
        if(> ?*TotalSavings* 1) then
        (printout t "Due to good savings you will be eligible for loan" crlf)
    )
    (
        if(< ?*TotalSavings* 2) then
        (printout t "Due to bad savings you will not be eligible for loan" crlf)
    )
)

(reset)
(run)