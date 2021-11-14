;;; enigma-machine.lisp --- an Enigma machine simulator

;; Copyright (C) 2021 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage :enigma-machine
  (:use :common-lisp
	:iterate)
  (:export #:make-enigma
	   #:reset
	   #:configure
	   #:operate
	   #:rotor-positions)
  (:documentation "An Enigma machine simulator.

The Enigma machine is a cipher device used to secure German
military communications during World War II.

This simulator provides the following features:

   * You have the choice between the Enigma models Enigma I,
     Enigma-M3, Enigma-M4, and Enigma-G.

   * The plugboard connections, entry wheel, wheel order,
     ring settings, starting positions, and reflector can
     be configured.

   * The double-stepping anomaly of the ratchet and pawl
     mechanism is implemented.  This does not apply to the
     Enigma-G model since it has a gear drive.

   * Operating an Enigma is compatible with Common Lisp's
     stream concept.

Suppose your radio operator has received this encrypted message:

     2220 - 204 - QWE EWG -

     XYOWN LJPQH SVDWC LYXZQ FXHIU VWDJO BJNZX RCWEO TVNJC IONTF
     QNSXW ISXKH JDAGD JVAKU KVMJA JHSZQ QJHZO IAVZO WMSCK ASRDN
     XKKSR FHCXC MPJGX YIJCC KISYY SHETX VVOVD QLZYT NJXNU WKZRX
     UJFXM BDIBR VMJKR HTCUJ QPTEE IYNYN JBEAQ JCLMU ODFWM ARQCF
     OBWN

The Enigma session to decipher the message is described below.

     ;; Create the Enigma machine.
     (setf enigma (make-enigma \"I\"))

     ;; Apply the daily key settings from the code book.
     (configure enigma :umkehrwalze \"B\"
                       :walzenlage '(\"I\" \"IV\" \"III\")
                       :ringstellung '(16 26 8) ;P, Z, and H
                       :steckerverbindungen '(\"AD\" \"CN\" \"ET\" \"FL\" \"GI\"
                                              \"JV\" \"KZ\" \"PU\" \"QY\" \"WX\"))

     ;; Decode the message key.
     (configure enigma :grundstellung \"QWE\")
     (operate enigma nil \"EWG\")
     ; ⇒ \"RTZ\"

     ;; Apply the message key.
     (configure enigma :grundstellung \"RTZ\")

     ;; Decode the message text (ignoring the first block).
     (operate enigma nil \"
           LJPQH SVDWC LYXZQ FXHIU VWDJO BJNZX RCWEO TVNJC IONTF
     QNSXW ISXKH JDAGD JVAKU KVMJA JHSZQ QJHZO IAVZO WMSCK ASRDN
     XKKSR FHCXC MPJGX YIJCC KISYY SHETX VVOVD QLZYT NJXNU WKZRX
     UJFXM BDIBR VMJKR HTCUJ QPTEE IYNYN JBEAQ JCLMU ODFWM ARQCF
     OBWN\")
     ; ⇒ \"
     ;      DASOB ERKOM MANDO DERWE HRMAQ TGIBT BEKAN NTXAA CHENX
     ;AACHE NXIST GERET TETXD URQGE BUEND ELTEN EINSA TZDER HILFS
     ;KRAEF TEKON NTEDI EBEDR OHUNG ABGEW ENDET UNDDI ERETT UNGDE
     ;RSTAD TGEGE NXEIN SXAQT XNULL XNULL XUHRS IQERG ESTEL LTWER
     ;DENX\"

Reformatting the plain text:

     Das Oberkommando der Wehrmacht gibt bekannt: Aachen
     ist gerettet. Durch gebündelten Einsatz der Hilfskräfte
     konnte die Bedrohung abgewendet und die Rettung der Stadt
     gegen 18:00 Uhr sichergestellt werden.

See ‘https://en.wikipedia.org/wiki/Enigma_machine’ for more details
about the Enigma machine."))

(in-package :enigma-machine)

(defmacro defconst (name value &optional doc)
  "Define a constant variable.

This is like ‘defconstant’ except that the initially set value
is reused when the ‘defconst’ form is evaluated again."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro defsubst (name arg-list &body body)
  "Define an inline function.

This is like ‘defun’ except that the function is globally marked
for inline expansion by the compiler."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,arg-list
       ,@body)))

(defconst beta (string (code-char #x03B2))
  "Greek small letter beta as a string.")

(defconst gamma (string (code-char #x03B3))
  "Greek small letter gamma as a string.")

(defparameter *alphabet*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "The default alphabet.")

(defparameter *wheels*
  `(;; Enigma I and Enigma-M.
    ;;
    ;;              ABCDEFGHIJKLMNOPQRSTUVWXYZ
    (i-i    "I"    "EKMFLGDQVZNTOWYHXUSPAIBRCJ" :turnover "Q")
    (i-ii   "II"   "AJDKSIRUXBLHWTMCQGZNPYFVOE" :turnover "E")
    (i-iii  "III"  "BDFHJLCPRTXVZNYEIWGAKMUSQO" :turnover "V")
    (i-iv   "IV"   "ESOVPZJAYQUIRHXLNFTGKDCMWB" :turnover "J")
    (i-v    "V"    "VZBRGITYUPSDNHLXAWMJQOFECK" :turnover "Z")
    (i-vi   "VI"   "JPGVOUMFYQBENHZRDKASXLICTW" :turnover "MZ")
    (i-vii  "VII"  "NZJHGRCXMYSWBOUFAIVLPEKQDT" :turnover "MZ")
    (i-viii "VIII" "FKQHTLXOCBJSPDZRAMEWNIUYGV" :turnover "MZ")
    (i-a    "A"    "EJMZALYXVBWFCRQUONTSPIKHGD")
    (i-b    "B"    "YRUHQSLDPXNGOKMIEBFZCWVJAT")
    (i-c    "C"    "FVPJIAOYEDRZXWGCTKUQSBNMHL")
    ;;
    ;; Enigma-M4 (thin wheels).
    ;;
    ;;                 ABCDEFGHIJKLMNOPQRSTUVWXYZ
    (m4-beta   ,beta  "LEYJVCNIXWPBQMDRTAKZGFUHOS" :position 1 :static t)
    (m4-gamma  ,gamma "FSOKANUERHMBTIYCWLQPZXVGJD" :position 1 :static t)
    (m4-bruno  "B"    "ENKQAUYWJICOPBLMDXZVFTHRGS")
    (m4-caesar "C"    "RDOBJNTKVEHMLFCWZAXGYIPSUQ")
    ;;
    ;; Enigma-G.
    ;;
    ;;             ABCDEFGHIJKLMNOPQRSTUVWXYZ
    (g-etw  "ETW" "QWERTZUIOASDFGHJKPYXCVBNML")
    (g-i    "I"   "LPGSZMHAEOQKVXRFYBUTNICJDW" :turnover "ABCEFGIKLOPQSUVWZ")
    (g-ii   "II"  "SLVGBTFXJQOHEWIRZYAMKPCNDU" :turnover "ACDFGHKMNQSTVYZ")
    (g-iii  "III" "CJGDPSHKTURAWZXFMYNQOBVLIE" :turnover "AEFHKMNRUWX")
    (g-ukw  "UKW" "IMETCGFRAYSQBZXWLHKDVUPOJN" :position 1)
    ;;
    ;; Dummy wheel.
    ;;
    ;;            ABCDEFGHIJKLMNOPQRSTUVWXYZ
    (identity "" "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  "Alist of Enigma wheels.
List elements are cons cells of the form

     (KEY . (LABEL WIRING &key TURNOVER POSITION STATIC))

where KEY is a unique identifier (a symbol) and the rest is a lambda
list with the following meaning.

First argument LABEL is the name of the wheel (a string).
Second argument WIRING defines the internal wiring of the wheel in
 forward direction, i.e. the cipher alphabet of the wheel (a string).
Keyword argument TURNOVER defines the turnover positions (a string).
 A turnover at Q means, that the turnover happens when the wheel moves
 from Q to R.
Keyword argument POSITION defines the exclusive positions of a rotor
 in the wheel order.  Value is either a single integer or a list of
 integers.  If omitted, the rotor can be placed anywhere in the wheel
 order.  A wheel order position is one-based and the first rotor is
 the one next to the reflector.
Keyword argument STATIC is a generalized boolean.  True means that the
 rotor never moves.

Forward direction is from the entry wheel to the reflector and
backward direction is from the reflector to the entry wheel.")

(defun wheel-set (&rest keys)
  "Build a wheel set."
  (iter (for key :in keys)
	(for cell = (assoc key *wheels*))
	(when (null cell)
	  (error 'program-error))
	(adjoining (rest cell))))

(defun wheel-position (mark &optional (alphabet *alphabet*))
  "Return the zero-based wheel position associated with MARK."
  (etypecase mark
    ((integer 1)
     (when (<= mark (length alphabet))
       (1- mark)))
    (character
     (position mark alphabet))
    ((string 1)
     (position (aref mark 0) alphabet))))

(defparameter *models*
  `((enigma-i
     :model "Enigma I"
     :rotors ,(wheel-set 'i-i 'i-ii 'i-iii 'i-iv 'i-v)
     :reflectors ,(wheel-set 'i-a 'i-b 'i-c)
     :plugboard-characters t ;equal to the alphabet
     :rotors-in-series 3
     :reflector "B"
     :wheel-order ("I" "II" "III"))
    (enigma-m3
     :model "Enigma-M3"
     :rotors ,(wheel-set 'i-i 'i-ii 'i-iii 'i-iv 'i-v 'i-vi 'i-vii 'i-viii)
     :reflectors ,(wheel-set 'i-a 'i-b 'i-c)
     :plugboard-characters t
     :rotors-in-series 3
     :reflector "B"
     :wheel-order ("I" "II" "III"))
    (enigma-m4
     :model "Enigma-M4"
     :rotors ,(wheel-set 'i-i 'i-ii 'i-iii 'i-iv 'i-v 'i-vi 'i-vii 'i-viii 'm4-beta 'm4-gamma)
     :reflectors ,(wheel-set 'm4-bruno 'm4-caesar)
     :plugboard-characters t
     :rotors-in-series 4
     :reflector "B"
     :wheel-order (,beta "I" "II" "III"))
    ;; The Zählwerk Enigma and its descendants, the Enigma-G
    ;; models, have two notable features: a gear drive and a
    ;; moving reflector with ring setting.
    (enigma-g
     :model "Enigma-G"
     :entry-wheels ,(wheel-set 'g-etw)
     :rotors ,(wheel-set 'g-i 'g-ii 'g-iii 'g-ukw)
     :rotors-in-series 4
     :features (:single-stepping :moving-reflector)
     :entry-wheel "ETW"
     :wheel-order ("UKW" "I" "II" "III")))
  "Alist of Enigma models.")

(defclass enigma ()
  ((model
    :initarg :model
    :initform "Enigma"
    :type string
    :documentation "Model name.")
   ;; Characteristics.
   (alphabet
    :initarg :alphabet
    :initform *alphabet*
    :type string
    :documentation "Set of characters in the order on a wheel.")
   (plugboard-characters
    :initarg :plugboard-characters
    :initform nil
    :type (or null string (eql t))
    :documentation "Set of plugboard characters.")
   (entry-wheels
    :initarg :entry-wheels
    :initform nil
    :documentation "Set of entry wheels.")
   (rotors
    :initarg :rotors
    :initform nil
    :documentation "Set of rotors.")
   (reflectors
    :initarg :reflectors
    :initform nil
    :documentation "Set of reflectors.")
   (rotors-in-series
    :initarg :rotors-in-series
    :initform 0
    :type fixnum
    :documentation "Number of rotors during operation.")
   (features
    :initarg :features
    :initform nil
    ;; :single-stepping
    ;;      Omit the double-stepping anomaly.  Only the original
    ;;      Zählwerk Enigma and the derived Enigma-G models have
    ;;      this feature.
    ;; :moving-reflector
    ;;      The reflector is involved in the rotor stepping.
    :documentation "Machine features.")
   ;; Configuration settings.
   (plugboard-connections
    :initform nil
    :documentation "Pairs of letters connected on the plugboard.")
   (entry-wheel
    :initarg :entry-wheel
    :initform nil
    :documentation "Choice of stator.")
   (wheel-order
    :initarg :wheel-order
    :initform nil
    :documentation "Choice of rotors from left to right.")
   (ring-settings
    :initform nil
    :documentation "Position of the alphabet relative to the rotor wiring.")
   (starting-positions
    :initform nil
    :documentation "Starting position of the rotors.")
   (reflector
    :initarg :reflector
    :initform nil
    :documentation "Choice of reflector.")
   (reflector-wiring
    :initform nil
    :documentation "Wiring of the reconfigurable reflector.")
   ;; State variables.
   (length*
    :initform 0
    :documentation "Number of characters in the alphabet.")
   (alphabet*
    :initform nil
    :documentation "Compiled alphabet.")
   (plugboard*
    :initform nil
    :documentation "Compiled plugboard.")
   (stator*
    :initform nil
    :documentation "Compiled stator.")
   (rotors*
    :initform nil
    :documentation "Compiled rotors.")
   (reflector*
    :initform nil
    :documentation "Compiled reflector.")
   (dirty
    :initform (list :alphabet t :plugboard t :stator t :rotors t :reflector t :rotor-positions t)
    :documentation "Plist of reconfigured items."))
  (:documentation "Enigma machine."))

(defun make-enigma (model)
  "Create an Enigma machine.

Argument MODEL is a string designator identifying the Enigma model.
 Known models are the Enigma I, Enigma-M3, and Enigma-M4.  Case is
 not significant when specifying the model.

Return value is an Enigma object."
  (check-type model alexandria:string-designator)
  (apply #'make-instance 'enigma
	 (rest (assoc (let ((string (string-upcase (string model))))
			(cond ((member string '("ENIGMA I" "ENIGMA-I" "I")
				       :test #'string=)
			       'enigma-i)
			      ((member string '("ENIGMA-M3" "ENIGMA M3" "M3")
				       :test #'string=)
			       'enigma-m3)
			      ((member string '("ENIGMA-M4" "ENIGMA M4" "M4")
				       :test #'string=)
			       'enigma-m4)
			      ((or (member string '("ENIGMA-G" "ENIGMA G" "G")
					   :test #'string=)
				   (member string '("ENIGMA-G31" "ENIGMA G31" "G31")
					   :test #'string=))
			       'enigma-g)
			      (t
			       (error "Unknown Enigma model ‘~A’." model))))
		      *models*))))

(defgeneric reset (enigma)
  (:documentation "Reset an Enigma machine, i.e. remove all configuration settings.

Argument ENIGMA is an Enigma object.

Return value is the Enigma object."))

(defmethod reset ((enigma enigma))
  (setf (slot-value enigma 'plugboard-connections) nil
	(slot-value enigma 'entry-wheel) nil
	(slot-value enigma 'wheel-order) nil
	(slot-value enigma 'ring-settings) nil
	(slot-value enigma 'starting-positions) nil
	(slot-value enigma 'reflector) nil
	(slot-value enigma 'reflector-wiring) nil)
  (setf (slot-value enigma 'length*) 0
	(slot-value enigma 'alphabet*) nil
	(slot-value enigma 'plugboard*) nil
	(slot-value enigma 'stator*) nil
	(slot-value enigma 'rotors*) nil
	(slot-value enigma 'reflector*) nil)
  (iter (for key :on (slot-value enigma 'dirty) :by #'cddr)
	(setf (second key) t))
  enigma)

(defgeneric configure (enigma &rest options &key &allow-other-keys)
  (:documentation "Configure an Enigma machine, i.e. apply configuration settings.

First argument ENIGMA is an Enigma object.
Keyword argument PLUGBOARD-CONNECTIONS, PLUGBOARD-CONNECTION,
 or STECKERVERBINDUNGEN specifies the plugboard connections.
 Value is a sequence of character pairs.  Each pair is either
 a cons cell of the form ‘(CHAR1 . CHAR2)’ or a string with
 two characters.
Keyword argument ENTRY-WHEEL or EINTRITTSWALZE specifies the
 entry wheel.  Value is the wheel label of the entry wheel.
 Most Enigma models have a fixed entry wheel which can't be
 configured.
Keyword argument WHEEL-ORDER or WALZENLAGE specifies the wheel
 order or the rotors from left to right.  Value is a sequence
 of wheel labels.
Keyword argument RING-SETTINGS, RING-SETTING, or RINGSTELLUNG
 specifies the ring settings of the rotors in wheel order.
 Value is a sequence of wheel position indicators.  Default
 is 1 for all rotors.
Keyword argument STARTING-POSITIONS, STARTING-POSITION, or
 GRUNDSTELLUNG specifies the starting position of the rotors in
 wheel order.  Value is a sequence of wheel position indicators.
 Default is 1 for all rotors.
Keyword argument REFLECTOR or UMKEHRWALZE specifies the reflector.
 Value is the wheel label of the reflector.
Keyword argument REFLECTOR-WIRING or UMKEHRWALZENVERDRAHTUNG
 specifies the wiring of a reconfigurable reflector.  Value is
 a sequence of character pairs.  Each pair is either a cons cell
 of the form ‘(CHAR1 . CHAR2)’ or a string with two characters.

A wheel label is a string designator (i.e. a string, a character,
 or a symbol).  Case is not significant for wheel labels.
A wheel position indicator is either a one-based integer or a
 character designator (i.e. a character or a string of length
 one).  Case is significant for wheel position indicators.

All configuration settings can be addressed by multiple keyword
arguments.  If more than one such keyword argument matches, the
leftmost keyword argument is used (no error).

The values of the keyword arguments are not checked.  This is
done when the Enigma machine is going to be operated.  Thus,
you can force argument checking by executing a no-operation
command, e.g. ‘(operate ENIGMA t nil)’.

Multiple calls to ‘configure’ accumulate.  Call ‘reset’ to remove
all configuration settings.

Return value is the Enigma object."))

(defmethod configure ((enigma enigma) &rest options &key &allow-other-keys)
  (let (key value slot dirty seen)
    (loop
      (when (null options)
	(return))
      (setf key (first options)
	    value (second options))
      (multiple-value-setq (slot dirty)
	(ecase key
	  ((:plugboard-connections
	    :plugboard-connection
	    :steckerverbindungen)
	   (values 'plugboard-connections
		   :plugboard))
	  ((:entry-wheel
	    :eintrittswalze)
	   (values 'entry-wheel
		   :stator))
	  ((:wheel-order
	    :walzenlage)
	   (values 'wheel-order
		   :rotors))
	  ((:ring-settings
	    :ring-setting
	    :ringstellung)
	   (values 'ring-settings
		   :rotors))
	  ((:starting-positions
	    :starting-position
	    :grundstellung)
	   (values 'starting-positions
		   :rotor-positions))
	  ((:reflector
	    :umkehrwalze)
	   (values 'reflector
		   :reflector))
	  ((:reflector-wiring
	    :umkehrwalzenverdrahtung)
	   (values 'reflector-wiring
		   :reflector))))
      ;; §3.4.1.4 says: The keyword parameter specifiers are, like
      ;; all parameter specifiers, effectively processed from left to
      ;; right.  For each keyword parameter specifier, if there is an
      ;; argument pair whose name matches that specifier's name (that
      ;; is, the names are ‘eq’), then the parameter variable for that
      ;; specifier is bound to the second item (the value) of that
      ;; argument pair.  If more than one such argument pair matches,
      ;; the leftmost argument pair is used.
      (unless (member slot seen)
	(setf (slot-value enigma slot) value)
	;; Mark the reconfigured item as dirty.
	(setf (getf (slot-value enigma 'dirty) dirty) t)
	;; Ignore successive matching keywords.
	(push slot seen))
      ;; Next pair.
      (setf options (cddr options))))
  enigma)

(defun %compile-alphabet (enigma)
  "The compiled alphabet is a vector of zero-based wheel positions.
The first character of the alphabet is located at wheel position 0."
  (let* ((alphabet (with-slots (alphabet) enigma
		     (unless (stringp alphabet)
		       (setf alphabet (coerce alphabet 'string)))
		     (iter (for char :in-string alphabet)
			   (for pos = (position char alphabet))
			   (adjoining pos :result-type 'vector))))
	 (length (length alphabet)))
    (when (zerop length)
      (error "The alphabet is empty."))
    (when (/= length (length (slot-value enigma 'alphabet)))
      (error "The characters in the alphabet are not unique."))
    (setf (slot-value enigma 'length*) length
	  (slot-value enigma 'alphabet*) alphabet))
  (setf (getf (slot-value enigma 'dirty) :alphabet) nil))

(defun %compile-plugboard (enigma)
  "The compiled plugboard is a copy of the compiled alphabet
where the connected character pairs are exchanged."
  (with-slots (alphabet plugboard-characters plugboard-connections) enigma
    (if (null plugboard-characters)
	(unless (null plugboard-connections)
	  (error "This Enigma has no plugboard."))
      (let ((plugboard (copy-seq (slot-value enigma 'alphabet*)))
	    (valid-characters (if (eq plugboard-characters t)
				  alphabet
				(coerce plugboard-characters 'string)))
	    (connected-characters ()))
	(labels ((ensure-character (char)
		   (when (null (position char valid-characters :test #'char=))
		     (error "The character ‘~A’ is not in the plugboard character set." char))
		   (let ((pos (position char alphabet :test #'char=)))
		     (when (null pos)
		       (error "The plugboard character ‘~A’ is not in the alphabet." char))
		     (when (member pos connected-characters)
		       (error "The character ‘~A’ is already connected on the plugboard." char))
		     pos))
		 (swap-characters (a b)
		   (let ((i (ensure-character a))
			 (j (ensure-character b)))
		     (when (= i j)
		       (error "The two characters of a pair must be different."))
		     (rotatef (aref plugboard i)
			      (aref plugboard j))
		     (push i connected-characters)
		     (push j connected-characters))))
	  (iter (for pair :in-sequence plugboard-connections)
		(etypecase pair
		  ((cons character character)
		   (swap-characters (car pair) (cdr pair)))
		  ((string 2)
		   (swap-characters (aref pair 0) (aref pair 1))))))
	(setf (slot-value enigma 'plugboard*) plugboard))))
  (setf (getf (slot-value enigma 'dirty) :plugboard) nil))

(defun %compile-wheel (enigma wheel &optional (ring-setting 0) (wheel-position 0))
  "A compiled wheel is a list of the form

     (POSITION (FORWARD . BACKWARD) PROPERTIES)

First element POSITION is the current wheel position.
Second element is a cons cell where FORWARD and BACKWARD are the
 compiled cipher alphabets of the wheel in forward and backward
 direction respectively.  The effect of the ring setting is
 already applied to the compiled cipher alphabets.
Third element PROPERTIES is a property list."
  (with-slots (alphabet (length length*)) enigma
    (destructuring-bind (label wiring &key turnover position static) wheel
      (declare (ignore position))
      (let ((forward (make-array length))
	    (backward (make-array length)))
	(iter (for wire-in :from 0 :below length)
	      (for wire-out = (position (aref wiring wire-in) alphabet))
	      (for wheel-in = (mod (+ wire-in ring-setting) length))
	      (for wheel-out = (mod (+ wire-out ring-setting) length))
	      (setf (aref forward wheel-in) wheel-out))
	(iter (for wire-out :from 0 :below length)
	      (for wire-in = (position (aref alphabet wire-out) wiring))
	      (for wheel-out = (mod (+ wire-out ring-setting) length))
	      (for wheel-in = (mod (+ wire-in ring-setting) length))
	      (setf (aref backward wheel-out) wheel-in))
	(setf turnover (iter (for char :in-string (coerce turnover 'string))
			     (adjoining (position char alphabet) :result-type 'vector)))
	(list wheel-position
	      (cons forward backward)
	      (list :turnover turnover
		    :static (not (null static))
		    :label label))))))

(defun %compile-stator (enigma)
  "The compiled entry wheel, if any, is a wheel that doesn't move."
  (with-slots (entry-wheels entry-wheel) enigma
    (if (null entry-wheels)
	(unless (null entry-wheel)
	  (error "This Enigma has no configurable entry wheel."))
      (let ((wheel (assoc (string entry-wheel) entry-wheels :test #'string-equal)))
	(when (null wheel)
	  (error "Unknown entry wheel ‘~A’." entry-wheel))
	(setf (slot-value enigma 'stator*) (%compile-wheel enigma wheel)))))
  (setf (getf (slot-value enigma 'dirty) :stator) nil))

(defun %compile-rotors (enigma)
  "The compiled rotor assembly is a vector of wheels in wheel order.
We use a vector so that we can access each rotor easily."
  (with-slots (alphabet rotors rotors-in-series wheel-order ring-settings) enigma
    (when (null wheel-order)
      (error "Wheel order is not configured."))
    (when (/= rotors-in-series (length wheel-order))
      (error "Wrong number of rotors."))
    (when (null ring-settings)
      (setf ring-settings (iter (repeat rotors-in-series) (collecting 1))))
    (when (/= rotors-in-series (length ring-settings))
      (error "Wrong number of ring settings."))
    (let ((wheels (make-array rotors-in-series)))
      (iter (for label :in-sequence wheel-order)
	    (for wheel = (assoc (string label) rotors :test #'string-equal))
	    (when (null wheel)
	      (error "Unknown rotor ‘~A’." label))
	    ;; Check assembly position of the wheel.
	    (for assembly-position :from 1)
	    (alexandria:when-let ((exclusive-positions (getf wheel :position)))
	      (unless (listp exclusive-positions)
		(setf exclusive-positions (list exclusive-positions)))
	      (unless (member assembly-position exclusive-positions)
		(error "The rotor ‘~A’ can not be placed at position ~A." label assembly-position)))
	    #-(and)
	    (progn
	      (with assembled-wheels = ())
	      (when (member wheel assembled-wheels)
		(error "The rotor ‘~A’ is already assembled." label))
	      (push wheel assembled-wheels))
	    (for mark :in-sequence ring-settings)
	    (for ring-setting = (wheel-position mark alphabet))
	    (when (null ring-setting)
	      (error "Invalid ring setting ‘~A’." mark))
	    (setf (aref wheels (1- assembly-position))
		  (%compile-wheel enigma wheel ring-setting)))
      (setf (slot-value enigma 'rotors*) wheels)))
  (setf (getf (slot-value enigma 'dirty) :rotors) nil))

(defun %compile-reflector (enigma)
  "The compiled reflector is a cipher alphabet."
  (with-slots (features reflectors reflector) enigma
    ;; Theoretically it is possible to simulate/build an Enigma
    ;; machine without a reflector but with a static exit wheel.
    ;; Thus, eliminating one of the major weaknesses, i.e. that
    ;; no character can be enciphered to itself.  Removing this
    ;; check should do the trick.
    (when (and (null reflector) (not (member :moving-reflector features)))
      (error "Reflector is not configured."))
    (if (null reflectors)
	(unless (null reflector)
	  (error "This Enigma has no reflector."))
      (let ((wheel (assoc (string reflector) reflectors :test #'string-equal)))
	(when (null wheel)
	  (error "Unknown reflector ‘~A’." reflector))
	(let ((tem (%compile-wheel enigma wheel)))
	  (setf (slot-value enigma 'reflector*) (car (second tem)))))))
  (setf (getf (slot-value enigma 'dirty) :reflector) nil))

(defun %apply-rotor-positions (enigma rotor-positions)
  "Adjust the current position of the rotors."
  (with-slots (alphabet rotors-in-series) enigma
    (when (/= rotors-in-series (length rotor-positions))
      (error "Wrong number of rotor positions."))
    (iter (for mark :in-sequence rotor-positions)
	  (for rotor-position = (wheel-position mark alphabet))
	  (when (null rotor-position)
	    (error "Invalid rotor position ‘~A’." mark))
	  (for wheel :in-vector (slot-value enigma 'rotors*))
	  (setf (first wheel) rotor-position))))

(defun %compile (enigma)
  "Compile the state variables."
  (with-slots (dirty) enigma
    (when (getf dirty :alphabet)
      (%compile-alphabet enigma))
    (when (getf dirty :plugboard)
      (%compile-plugboard enigma))
    (when (getf dirty :stator)
      (%compile-stator enigma))
    (when (getf dirty :rotors)
      (%compile-rotors enigma))
    (when (getf dirty :reflector)
      (%compile-reflector enigma))
    (when (getf dirty :rotor-positions)
      ;; Apply the starting positions.
      (with-slots (alphabet rotors-in-series starting-positions) enigma
	(when (null starting-positions)
	  (setf starting-positions (iter (repeat rotors-in-series) (collecting 1))))
	(%apply-rotor-positions enigma starting-positions))
      (setf (getf dirty :rotor-positions) nil)))
  enigma)

(defun %rotor-stepping (enigma)
  "Rotate the rotors for a single key press."
  (with-slots (features length* rotors*) enigma
    (iter (with step = nil)
	  (with turnoverp = nil)
	  (for wheel :in-vector rotors* :downto 0)
	  (for left = wheel)
	  (for middle :previous left :initially nil)
	  (for right :previous middle :initially nil)
	  (destructuring-bind (&key turnover static &allow-other-keys) (third wheel)
	    (setf step (when (not static)
			 (if (null middle)
			     t ;the right rotor always moves
			   turnoverp)))
	    (setf turnoverp (when (not static)
			      (position (first wheel) turnover)))
	    (when step
	      (setf (first wheel) (mod (1+ (first wheel)) length*))
	      (with single-stepping = (member :single-stepping features))
	      (unless single-stepping
		;; If the middle wheel is in turnover position, move
		;; it, too.  This is the double-stepping anomaly of
		;; the Enigma machine.
		(when (and (not (null right))
			   (let ((prop (third middle)))
			     (when (not (getf prop :static))
			       (position (first middle) (getf prop :turnover)))))
		  (setf (first middle) (mod (1+ (first middle)) length*)))))))))

(defun %route (char-pos wheel key)
  "Route a signal (a character of the alphabet) through a wheel.

First argument CHAR-POS is the character position at wheel entry,
 i.e. the right side or left side of the wheel depending on the
 signal direction.
Second argument WHEEL is the compiled wheel.
Third argument KEY is the function to access the compiled cipher
 alphabet of the wheel.  Value is either ‘car’ or ‘cdr’ for the
 forward or backward direction respectively.

Return value is the character position at the other side of the
wheel."
  (let ((wheel-pos (first wheel))
	(map (funcall key (second wheel))))
    (if (zerop wheel-pos)
	(aref map char-pos)
      (let ((len (length map)))
	(mod (- (aref map (mod (+ char-pos wheel-pos) len)) wheel-pos) len)))))

(defun %forward (char-pos wheel)
  "Like ‘%route’ but in forward direction."
  (%route char-pos wheel #'car))

(defun %backward (char-pos wheel)
  "Like ‘%route’ but in backward direction."
  (%route char-pos wheel #'cdr))

(defun %flow (enigma char-pos)
  "Route the signal (a character of the alphabet) through the machine.
That is, simulate the flow of electrical current from the keyboard to
the display."
  (with-slots (features plugboard* stator* rotors* reflector*) enigma
    ;; Plugboard.
    (when (not (null plugboard*))
      (setf char-pos (aref plugboard* char-pos)))
    ;; Entry wheel.
    (when (not (null stator*))
      (setf char-pos (%forward char-pos stator*)))
    ;; Rotors in reverse wheel order.
    (iter (for wheel :in-vector rotors* :downto 0)
	  (setf char-pos (%forward char-pos wheel)))
    ;; Reflector.  See comment in ‘%compile-reflector’.
    (when (or reflector* (member :moving-reflector features))
      (when (not (null reflector*))
	(setf char-pos (aref reflector* char-pos)))
      ;; Rotors in wheel order, but omit a moving reflector.
      (iter (for wheel :in-vector rotors* :from (if (member :moving-reflector features) 1 0))
	    (setf char-pos (%backward char-pos wheel)))
      ;; Entry wheel.
      (when (not (null stator*))
	(setf char-pos (%backward char-pos stator*)))
      ;; Plugboard.
      (when (not (null plugboard*))
	(setf char-pos (aref plugboard* char-pos)))))
  ;; Display.
  char-pos)

(defun %key (enigma char-pos)
  "Perform a single key press.
Return the displayed character."
  (%rotor-stepping enigma)
  (%flow enigma char-pos))

(defgeneric operate (enigma destination source)
  (:documentation "Operate an Enigma machine.

The ‘operate’ method produces output by entering characters read from
the source into the Enigma machine.  The displayed characters are then
printed to the destination.  Characters not in the alphabet of the
Enigma machine are printed unprocessed.

First argument ENIGMA is an Enigma object.
Second argument DESTINATION is the output object.  Value is either a
 stream, a pathname, or a string.  The special value ‘t’ is equal to
 ‘*standard-output*’ and ‘nil’ means to return a string.
Third argument SOURCE is the input object.  Value is either a stream,
 a pathname, or a string.  The special value ‘t’ is equal to
 ‘*standard-input*’ and ‘nil’ is equal to an empty string.

If DESTINATION is a stream, a pathname, a string, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a string containing the
output."))

(defmethod operate ((enigma enigma) (output stream) (input stream))
  (%compile enigma)
  (with-slots (alphabet) enigma
    (iter (for char = (read-char input nil input))
	  (until (eql char input))
	  (for in = (position char alphabet))
	  (when (not (null in))
	    (for out = (%key enigma in))
	    (setf char (aref alphabet out)))
	  (write-char char output)))
  nil)

(defmethod operate ((enigma enigma) (output (eql t)) input)
  (operate enigma *standard-output* input))

(defmethod operate ((enigma enigma) output (input (eql t)))
  (operate enigma output *standard-input*))

(defmethod operate ((enigma enigma) (pathname pathname) input)
  (with-open-file (output pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (operate enigma output input)))

(defmethod operate ((enigma enigma) output (pathname pathname))
  (with-open-file (input pathname :direction :input :if-does-not-exist :error)
    (operate enigma output input)))

(defmethod operate ((enigma enigma) (string string) input)
  (with-output-to-string (output string)
    (operate enigma output input)))

(defmethod operate ((enigma enigma) output (string string))
  (with-input-from-string (input string)
    (operate enigma output input)))

(defmethod operate ((enigma enigma) (output (eql nil)) input)
  (with-output-to-string (output)
    (operate enigma output input)))

(defmethod operate ((enigma enigma) output (input (eql nil)))
  (with-input-from-string (input "")
    (operate enigma output input)))

(defun rotor-positions (enigma &key (result-type 'list) (element-type 'character))
  "Return the current position of the rotors in wheel order.

First argument ENIGMA is an Enigma object.
Keyword arguments RESULT-TYPE and ELEMENT-TYPE specify the sequence
 type and element type of the result.

Value is a sequence of wheel position indicators."
  (check-type enigma enigma)
  (with-slots (alphabet rotors-in-series starting-positions rotors* dirty) enigma
    (let ((result (if (getf dirty :rotor-positions)
		      (iter (for mark :in-sequence starting-positions)
			    (collecting (wheel-position mark) :result-type 'vector))
		    (iter (for wheel :in-vector rotors*)
			  (collecting (first wheel) :result-type 'vector)))))
      (when (/= rotors-in-series (length result))
	(error "Wrong number of rotors."))
      (alexandria:eswitch (element-type :test #'subtypep)
	('character
	 (iter (for pos :in-vector result :with-index index)
	       (setf (aref result index) (aref alphabet pos))))
	('(integer 1)
	  (iter (for pos :in-vector result :with-index index)
		(setf (aref result index) (1+ pos))))
	('(integer 0))) ;no-op
      (coerce result result-type))))

;;; enigma-machine.lisp ends here
