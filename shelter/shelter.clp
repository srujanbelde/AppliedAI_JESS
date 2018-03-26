(deftemplate person
    (slot name) 
    (slot isvalidincome) 
    (slot isvalidsavings) 
    (slot isvaliddebt) 
    (slot area) 
    (slot income) 
    (slot savings) 
    (slot bed) 
    (slot bath)
    (slot minbalance)
    (slot loanelegibility)
    (slot incomeclass)
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
(deftemplate norepeat (slot repeat))
(defglobal 
    ?*Name* = nil
    ?*Area* = 0
    ?*AreaVerify* = nil
	?*BedNo* = 0
	?*BathNo* = 0
	?*Income* = 0
	?*Savings* = 0
	?*Asset* = 0
	?*Debt* = 0
	?*TotalSavings* = 0
	?*Budget* = 5000
    ?*Area1MinIncome* = 10000
    ?*Area2MinIncome* = 5000
    ?*BasePrice* = 0
    ?*temp1* = 0
    ?*temp2* = 0
)
	
(defrule takeInput
    =>
    (printout t "Please enter your first name: " crlf)
    (bind ?*Name* (read t))
    (printout t "Please enter 1 if you are looking to buy a house in downtown area or close by, " crlf)
    (printout t "2 if you are looking to buy a house in suburbs close to downtown" crlf)
	(bind ?*Area* (read t))
    (printout t "How many bedrooms are you looking for (1/2/3)?" crlf)
	(bind ?*BedNo* (read t))
	(printout t "How many bathrooms are you looking for (1/2)?" crlf)
	(bind ?*BathNo* (read t))
	(printout t "What is your Anual Income?" crlf)
	(bind ?*Income* (read t))
	(printout t "What is your overall asset value?" crlf)
	(bind ?*Asset* (read t))
	(printout t "How much are your Savings?" crlf)
	(bind ?*Savings* (read t))
	(printout t "How much is your outstanding debt per month (if any)" crlf)
	(bind ?*Debt* (read t))
	(assert (person (name ?*Name*)(bed ?*BedNo*)(bath ?*BathNo*)(income ?*Income*)(savings (+ ?*Savings* ?*Asset*))))
	(bind ?*TotalSavings*(+ ?*Savings* ?*Asset*))
    (AreaChecking ?*Area*)
	(BedCheck ?*BedNo*)
	(BathCheck ?*BathNo*)
	(IncomeAreaCheck ?*Income* ?*Area*)
	(IncomeAreaCheck1 ?*Income* ?*Area*)
    (IncomeCheck ?*Income*)
	(SavingsCheck ?*Savings* ?*Asset*)
	(DebtCheck ?*Debt*)
    (LoanEligibility ?*TotalSavings* ?*Debt*)
    (IncomeClasification ?*Income*)
    (Baseprice ?*Income* ?*BedNo* ?*BathNo* ?*Area*)
    (assert (norepeat(repeat 0)))
)

;;;Functions to help us resolve and check input

(deffunction AreaChecking(?Area)
    (if(or (> ?Area 2)(< ?Area 0)) then
	(assert (area (isvalidarea 0)))
	else
	(assert (area (isvalidarea 1)))
)
)

(deffunction BedCheck(?BedNo)
    (if(or (> ?BedNo 3) (< ?BedNo 0) ) then
	(assert (bed (isvalidbed 0)))
	else
	(assert (bed (isvalidbed 1)))
)
)

(deffunction BathCheck(?BathNo)
    (if(or (> ?BathNo 2) (< ?BathNo 0) ) then
	(assert (bath (isvalidbath 0)))
	else
	(assert (bath (isvalidbath 1)))
)
)

(deffunction IncomeCheck(?Income)
	(if (< ?Income 0) then
        (assert(person(isvalidincome 0)))
    else
        (assert(person(isvalidincome 1)))
    )    
)

(deffunction IncomeAreaCheck(?Income ?Area)
    (
    if(eq ?Area 1) then
    (
        if (< ?Income 10000) then
            (assert (person (minbalance 0)))
        else
         	(assert (person (minbalance 1)))
    )
    )
)
(deffunction IncomeAreaCheck1(?Income ?Area)
    (
    if(eq ?Area 2)then
    (
        if (< ?Income 5000) then
        	(assert (person (minbalance 0)))
        else
			(assert (person (minbalance 1)))
    )
	)
)
(deffunction SavingsCheck(?Savings ?Asset)
	(if(< (+ ?Savings ?Asset) 0) then
	(assert (person (isvalidsavings 0)))
	else
	(assert (person (isvalidsavings 1)))
)
)

(deffunction DebtCheck(?Debt)
    (if(< ?Debt 0) then
	(assert (person (isvaliddebt 0)))
	else
	(assert (person (isvaliddebt 1)))
)
)

(deffunction LoanEligibility (?*TotalSavings* ?*Debt*)
	(if (> ?*TotalSavings* (* ?*Debt* 3))then
        (assert (person (loanelegibility 1)))
        else
        (assert (person (loanelegibility 0))))
)

(deffunction IncomeClasification(?Income)
    (if(and (> ?Income 60000)(< ?Income 120000)) then
        (assert (person(incomeclass 2)))
    elseif(and (> ?Income 10000)(< ?Income 60000)) then
        (assert (person(incomeclass 1)))
    elseif(> ?Income 120000) then
        (assert (person(incomeclass 3)))
    )
)

(deffunction Baseprice(?Income ?BedNo ?BathNo ?Area)
   (bind  ?*BasePrice* (/ ?Income 5))
   (if(eq ?Area 1)then
        (bind  ?*BasePrice* (* ?*BasePrice* 2)))
    (bind ?*Temp1* (* 450 ?BedNo))
    (bind ?*Temp2* (* 220 ?BedNo))
    (bind ?*BasePrice* (+ ?*BasePrice* ?*Temp1*))
    (bind ?*BasePrice* (+ ?*BasePrice* ?*Temp2*))
)

;;;rules to check and assign appropriate results

(defrule invalidCheck1
	(and (norepeat{repeat == 0})(or (area{isvalidarea == 0})
    (person {minbalance == 0})
	(bed{isvalidbed == 0})
	(bath{isvalidbath == 0})
	(person{isvalidincome ==0})
	(person{isvalidsavings ==0})
	(person{isvaliddebt ==0})))
    =>
	(assert (Validity (valid 0)))
    (assert(norepeat(repeat 1)))
	(printout t "Your request cannot be processed because of the these reasons" crlf)
)

(defrule minbalancecheck
    (person {minbalance == 0})
    =>
	(printout t "you do not have sufficient income to get a house in  the selected area, minimun income for downtown area is 10k $ and for suburbs is 5k $" crlf)
)

(defrule invalidCheck2
	(and (area{isvalidarea == 1})
	(bed{isvalidbed == 1})
	(bath{isvalidbath == 1})
	(person{isvalidincome == 1})
	(person{isvalidsavings == 1})
	(person{isvaliddebt == 1}))
    (person{minbalance ==1})
    =>
	(assert (Validity (valid 1)))
)

(defrule invalidAreaRule
	(area{isvalidarea == 0})
    =>
    (printout t "Incorrect input for area selection:" ?*Area* " is an Invaled area number, please enter 1/2  for downtown/suburb respectively" crlf)
)

(defrule invalidBedRule
	(bed{isvalidbed == 0})
    =>
    (printout t "Incorrect input for bed selection:" ?*BedNo* " bed rooms is a wrong input, please enter 1/2/3" crlf)
)

(defrule invalidBathRule
	(bath{isvalidbath == 0})
    =>
    (printout t "Incorrect input for bath selection:" ?*BedNo* " bath rooms is a wrong input, please enter 1/2"crlf)
)

(defrule invalidIncomeRule
	(person{isvalidincome == 0})
	=>
	(printout t "Incorrect input for Income:" ?*Income* " , please enter a positive income or 0 if no income"crlf)
)

(defrule invalidSavingsRule
	(person{isvalidsavings == 0})
	=>
	(printout t "Incorrect input for Savings/Asset:" ?*Savings* "    " ?*Asset " , please enter a positive values or 0 if no savings"crlf)
)

(defrule invalidDebtRule
	(person{isvaliddebt == 0})
	=>
	(printout t "Incorrect input for Debt:" ?*Debt* " , please enter a positive Debt or 0 if no Debt"crlf)
)

(defrule loansanctionrule1
	(person{loanelegibility == 1})(Validity{valid ==1})
    =>
    (printout t "You are elegible for financing becasue of good value of total savings, this could help you spend money wisely" crlf)
    (printout t "You can get a loan of "(/ ?*TotalSavings* 3)" $" crlf)
)

(defrule loansanctionrule2
	(person{loanelegibility == 0})(Validity{valid ==1})
    =>
    (printout t "You are not elegible for financing becasue of your debts" crlf)
)

(defrule FinalPrice1
	(Validity{valid == 1})
    =>
    (printout t "The beat price you can put in to get a house which satisfies your requirements and also is financially safe is " (+ ?*BasePrice* 5000) crlf)
)

(reset)
(facts)
(run)