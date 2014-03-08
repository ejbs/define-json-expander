(in-package :define-json-expander)
;; TODO: Replace this with usage of test library

;; Test 1
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
                            HELLO))))))
;; Test 2
(assert (equal
         (macroexpand
          '(define-json-expander hello (x y z)
            ((x :json-prop :foo)
             (y :json-prop :bar :json-decoder #'foobar))))
         '(PROGN
           (DEFCLASS HELLO (X Y Z)
             ((REST :DOCUMENTATION "Unknown flags put here" :INITARG :REST
                    :ACCESSOR REST-OF)
              (X :INITARG :X) (Y :INITARG :Y)))
           (DEFINE-JSON-EXPANDER::DEFINE-JSON-DECODER HELLO (:REST :REST) (:X :FOO)
            (:Y :Y #'FOOBAR)))))
(assert (equal
         (macroexpand-1
           '(DEFINE-JSON-EXPANDER::DEFINE-JSON-DECODER HELLO (:REST :REST) (:X :FOO)
             (:Y :Y #'FOOBAR)))
         '(DEFUN DECODE-HELLO (HELLO)
           "Takes a JSON document in list form and decodes it into a CLOS HELLO object"
           (FLET ((DEFINE-JSON-EXPANDER::P (DEFINE-JSON-EXPANDER::KEY)
                    (CDR (ASSOC DEFINE-JSON-EXPANDER::KEY HELLO))))
             (MAKE-INSTANCE 'HELLO :REST (DEFINE-JSON-EXPANDER::P :REST) :X
                            (DEFINE-JSON-EXPANDER::P :FOO) :Y
                            (FUNCALL #'FOOBAR (DEFINE-JSON-EXPANDER::P :Y)) :REST
                            (REMOVE-IF
                             (LAMBDA (ELT)
                                (MEMBER (CAR ELT)
                                        '((:REST :REST) (:X :FOO) (:Y :Y #'FOOBAR)) :KEY
                                        #'CADR :TEST #'EQ))
                             HELLO))))))

;; Test 3 - Make sure that it adheres to DEFCLASS's behavior when a single slot occurs
(assert
 (equal
  (macroexpand
   '(define-json-expander x ()
     (z)))
  '(PROGN
    (DEFCLASS x ()
      ((REST :DOCUMENTATION "Unknown flags put here" :INITARG :REST
             :ACCESSOR REST-OF)
       (z :initarg :z)))
    (DEFINE-JSON-EXPANDER::DEFINE-JSON-DECODER x (:REST :REST) (:z :z)))))
