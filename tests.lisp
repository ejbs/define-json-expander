(in-package :define-json-expander)
;; TODO: Replace this with usage of test library


(assert
 (equal (macroexpand
         '(define-json-expander hello ()
           ((hello))))
        '(PROGN
          (DEFCLASS HELLO NIL
            ((REST :DOCUMENTATION "Unknown flags put here" :INITARG :REST
                   :ACCESSOR REST-OF)
             (HELLO :INITARG :HELLO)))
          (DEFINE-JSON-EXPANDER::DEFINE-JSON-DECODER HELLO (:REST :REST)
           (:HELLO :HELLO)))))
(assert
 (equal (macroexpand-1
         '(DEFINE-JSON-EXPANDER::DEFINE-JSON-DECODER HELLO (:REST :REST)
           (:HELLO :HELLO)))
        '(DEFUN DECODE-HELLO (HELLO)
          "Takes a JSON document in list form and decodes it into a CLOS HELLO object"
          (FLET ((DEFINE-JSON-EXPANDER::P (DEFINE-JSON-EXPANDER::KEY)
                   (CDR (ASSOC DEFINE-JSON-EXPANDER::KEY HELLO))))
            (MAKE-INSTANCE 'HELLO :REST (DEFINE-JSON-EXPANDER::P :REST) :HELLO
                           (DEFINE-JSON-EXPANDER::P :HELLO) :REST
                           (REMOVE-IF
                            (LAMBDA (ELT)
                              (MEMBER (CAR ELT) '((:REST :REST) (:HELLO :HELLO)) :KEY
                                      #'CADR :TEST #'EQ))
                            HELLO)))) ))
