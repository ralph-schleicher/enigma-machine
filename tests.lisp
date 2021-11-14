;;; tests.lisp --- test procedure

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

(ql:quickload :enigma-machine)
(ql:quickload :lisp-unit)

(defpackage :enigma-machine-tests
  (:use :common-lisp
	:lisp-unit
	:iterate
	:enigma-machine))

(in-package :enigma-machine-tests)

(defun double-stepping (model)
  (let ((enigma (make-enigma model)) first-wheel)
    (ecase model
      ((:I :M3)
       (configure enigma :umkehrwalze "B"
			 :walzenlage '("I" "II" "III")
			 :grundstellung "ADU")
       (setf first-wheel 0))
      ((:M4)
       (configure enigma :umkehrwalze "B"
			 :walzenlage '("β" "I" "II" "III")
			 :grundstellung "ZADU")
       (setf first-wheel 1)))
    (assert-true
     (every (lambda (expected)
	      (operate enigma nil "N")
	      (string= expected
		       (subseq (rotor-positions enigma :result-type 'string) first-wheel)))
	    '("ADV" ;first key press
	      "AEW" ;second key press
	      "BFX" ;third key press, double-stepping
	      "BFY")))))

(define-test double-stepping-tests
  (double-stepping :I)
  (double-stepping :M3)
  (double-stepping :M4))

(defun aachen (model)
  (let ((enigma (make-enigma model)))
    (ecase model
      ((:I :M3)
       (configure enigma :umkehrwalze "B"
			 :walzenlage '("I" "IV" "III")
			 :ringstellung '(16 26 8)
			 :grundstellung "RTZ"))
      ((:M4)
       ;; Reflector B and rotor β are compatible if ring setting and
       ;; starting position of rotor β is one.
       (configure enigma :umkehrwalze "B"
			 :walzenlage '("β" "I" "IV" "III")
			 :ringstellung '(1 16 26 8)
			 :grundstellung "ARTZ")))
    (configure enigma :steckerverbindungen '("AD" "CN" "ET" "FL" "GI"
					     "JV" "KZ" "PU" "QY" "WX"))
    (assert-equal "
      DASOB ERKOM MANDO DERWE HRMAQ TGIBT BEKAN NTXAA CHENX
AACHE NXIST GERET TETXD URQGE BUEND ELTEN EINSA TZDER HILFS
KRAEF TEKON NTEDI EBEDR OHUNG ABGEW ENDET UNDDI ERETT UNGDE
RSTAD TGEGE NXEIN SXAQT XNULL XNULL XUHRS IQERG ESTEL LTWER
DENX"
     (operate enigma nil "
      LJPQH SVDWC LYXZQ FXHIU VWDJO BJNZX RCWEO TVNJC IONTF
QNSXW ISXKH JDAGD JVAKU KVMJA JHSZQ QJHZO IAVZO WMSCK ASRDN
XKKSR FHCXC MPJGX YIJCC KISYY SHETX VVOVD QLZYT NJXNU WKZRX
UJFXM BDIBR VMJKR HTCUJ QPTEE IYNYN JBEAQ JCLMU ODFWM ARQCF
OBWN"))))

(define-test aachen-tests
  (aachen :I)
  (aachen :M3)
  (aachen :M4))

(defun looks (model)
  "Message from Kapt. Lt. Hartwig Looks, U-264."
  (let ((enigma (make-enigma model)))
    (ecase model
      ((:M4)
       (configure enigma :umkehrwalze "B"
			 :walzenlage '("β" "II" "IV" "I")
			 :ringstellung "AAAV"
			 :grundstellung "VJNA"
			 :steckerverbindungen '("AT" "BL" "DF" "GJ" "HM"
						"NW" "OP" "QY" "RZ" "VX"))
       (assert-equal "
VONV ONJL OOKS JHFF TTTE INSE INSD REIZ WOYY QNNS
NEUN INHA LTXX BEIA NGRI FFUN TERW ASSE RGED RUEC
KTYW ABOS XLET ZTER GEGN ERST ANDN ULAC HTDR EINU
LUHR MARQ UANT ONJO TANE UNAC HTSE YHSD REIY ZWOZ
WONU LGRA DYAC HTSM YSTO SSEN ACHX EKNS VIER MBFA
ELLT YNNN NNNO OOVI ERYS ICHT EINS NULL"
        (operate enigma nil "
NCZW VUSX PNYM INHZ XMQX SFWX WLKJ AHSH NMCO CCAK
UQPM KCSM HKSE INJU SBLK IOSX CKUB HMLL XCSJ USRR
DVKO HULX WCCB GVLI YXEO AHXR HKKF VDRE WEZL XOBA
FGYU JQUK GRTV UKAM EURB VEKS UHHV OYHA BCJW MAKL
FKLM YFVN RIZR VVRT KOFD ANJM OLBG FFLE OPRG TFLV
RHOW OPBE KVWM UQFM PWPA RMFH AGKX IIBG"))))))

(define-test looks-tests
  (looks :M4))

(defun t-dir (file-name)
  "Run test FILE-NAME in directory ‘t’."
  (let* ((directory '(:relative "t"))
	 (cipher (make-pathname :directory directory
				:name file-name
				:type "cipher"))
	 (plain (make-pathname :directory directory
			       :name file-name
			       :type "plain"))
	 (lisp (make-pathname :directory directory
			      :name file-name
			      :type "lisp"))
	 (enigma (with-open-file (stream lisp)
		   (eval (read stream))))
	 (start (rotor-positions enigma)))
    ;; Encoding.
    (configure enigma :grundstellung start)
    (assert-equal
     (alexandria:read-file-into-string cipher)
     (operate enigma nil plain))
    ;; Decoding.
    (configure enigma :grundstellung start)
    (assert-equal
     (alexandria:read-file-into-string plain)
     (operate enigma nil cipher))))

(define-test t-dir-tests
  (t-dir "looks"))

(let ((lisp-unit:*print-errors* t)
      (lisp-unit:*print-failures* t)
      (lisp-unit:*print-summary* t))
  (run-tests))

;;; tests.lisp ends here
