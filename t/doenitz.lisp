;; The message from Grand Admiral Karl Dönitz sent on May 1, 1945.
;; See https://www.cryptomuseum.com/crypto/enigma/msg/p1030681.htm.
(let ((enigma (make-enigma "M4")))
  (configure enigma :umkehrwalze "C"
		    :walzenlage '("β" "V" "VI" "VIII")
		    :ringstellung "EPEL"
		    :grundstellung "NAEM"
		    :steckerverbindungen '("AE" "BF" "CM" "DQ" "HU"
					   "JN" "LX" "PR" "SZ" "VW"))
  ;; Apply the message key ‘CDSZ’.
  (configure enigma :grundstellung (operate enigma nil "QEOB")))
