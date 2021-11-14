;; Reflector B and rotor β are compatible if ring setting and
;; starting position of rotor β is one.
(configure (make-enigma "M4") :umkehrwalze "B"
			      :walzenlage '("β" "I" "IV" "III")
			      :ringstellung '(1 16 26 8)
			      :grundstellung "ARTZ"
			      :steckerverbindungen '("AD" "CN" "ET" "FL" "GI"
						     "JV" "KZ" "PU" "QY" "WX"))
