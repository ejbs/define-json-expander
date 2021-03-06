(in-package :define-json-expander)

(defparameter *accessor-prefix* "DECODE-~A")

(defun flatten-n-times (tree &optional (n 2))
  "Flatten a tree by n levels of subtrees. n = 1 doesn't flatten at all."
  (let (list)
    (labels ((traverse (subtree n)
               (when subtree
                 (if (and (consp subtree) (> n 0))
                     (progn
                       (traverse (car subtree) (1- n))
                       (traverse (cdr subtree) n))
                     (push subtree list)))))
      (traverse tree n))
    (nreverse list)))

(defun group (source n)
  (declare (type list source) (type integer n))
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
		(let ((rest (nthcdr n source)))
		  (if (consp rest)
		      (rec rest (cons (subseq source 0 n) acc))
		    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun clean-option (direct-slot option)
  "Returns two values, the first being the `direct-slot' with the `option' and its value stripped and the second being a list consisting of the name of the slot and the found prop and value. If such a prop isn't found then the second element is NIL."
  (declare (type (or symbol list) direct-slot)
           (type symbol option))
  (when (eq (type-of direct-slot) 'symbol)
    (return-from clean-option (values direct-slot nil)))
  (let* ((grouped (group (cdr direct-slot) 2))
         (found-prop (find option grouped :key #'car :test #'eq))
         (final (append (list (first direct-slot))
                        (flatten-n-times
                         (remove-if #'(lambda (x) (eq option x))
                                    grouped :key #'car) 2))))
    (values final (list (first direct-slot) found-prop))))

(defun clean-options (slots &rest options)
  "Cleans several options from several slots."
  (declare (type list slots options))
  (labels ((rec (dslots options &optional prop-accum)
             (when options
                 (let (slots
                       props)
                   (loop for slot in dslots
                      do (multiple-value-bind (slot prop)
                             (clean-option slot (car options))
                           (push slot slots)
                           ;; If such a prop was even found, then push
                           (if (cadr prop) (push prop props))))
                   (if (cdr options)        
                       (rec slots (cdr options) props)
                       (values slots (concatenate 'list props prop-accum)))))))
    (rec slots options nil)))

(defun get-prop (symbol list)
  (nth (1+ (position symbol list)) list))

(defmacro define-json-expander (name direct-superclasses direct-slots &rest options)
  (declare (type symbol name) (type list direct-superclasses direct-slots options))
  ;; Preparation
  (multiple-value-bind (cleaned-direct-slots collected-properties)
      (clean-options direct-slots :json-prop :json-decoder)

    ;; Was a slot provided by its symbol alone? If so, wrap it up in a list
    (setf cleaned-direct-slots
          (loop for s in cleaned-direct-slots
             collect (if (listp s) s (list s))))
    
    ;; Automatically store all unknown json-properties + values in this slot
    ;; Note: This is used by define-json-decoder
    (unless  (find 'rest cleaned-direct-slots
                   :test (lambda (term slot)
                           (find term slot)))
      (push `(rest :documentation "Unknown flags put here" :initarg :rest :accessor ,(intern (string-upcase "rest-of") *package*)) cleaned-direct-slots))
    ;; Append :initarg to all slots in case it is not present yet
    (setf cleaned-direct-slots
          (mapcar
           (lambda (slot)
             (if (and (listp slot) (find :initarg slot))
                 slot
                 (append slot
                         (list :initarg (intern (symbol-name (car slot)) 'keyword)))))
           cleaned-direct-slots))
    
    ;; Expansion
    (flet ((get-json-prop (json-prop default prop-slot slot)
             (if (find json-prop (cdr prop-slot) :key #'car)
                 (cdr (find json-prop (cdr prop-slot) :key #'car))
                 (when default (list (get-prop default slot))))))
      `(progn
         ;; This remove feels like a small kludge
         ;; Basically it's there to remove the NIL (replace by gensym) that occurs if there are no options provided
         ;; It's a way to say "Replace with nothing" instead of "Replace with NIL"
         ,(let ((g (gensym)))
               (remove g
                       `(defclass ,name ,direct-superclasses
                          ,cleaned-direct-slots
                          ,@(or options (list g)))))
         (define-json-decoder ,name
             ,@(loop
                  for slot in cleaned-direct-slots
                  collect
                    (let ((prop-slot (find (car slot) collected-properties :test #'eq :key #'car)))
                      (append (list (get-prop :initarg slot))
                              ;; If a :json-prop has not been supplied, assume same as :initarg; otherwise take json-prop
                              (get-json-prop :json-prop :initarg prop-slot slot)
                              ;; If a :json-decoder has not been supplied then none shall be given
                              (get-json-prop :json-decoder nil prop-slot slot)))))))))

(defmacro define-json-decoder
    (name &rest slot/json-list)
  #.(format nil "Generates a decoder function with the name decode-`name'~
~%The `slot/json-list' is of the form (slot-name json-property-name &optional decoder-function) where~
~%slot-name refers to the slot's initarg~
~%decoder-function refers to the function which will be called upon a structure if so provided
~%The decoder automatically takes all unused JSON-properties and puts them into the rest slot~
~2%Example definition and expansion:~
~%(define-4chan-decoder post
  (:no :no)
  (:name :name)
  (:e-mail :email)
  (:body :com)
  (:date :time))~
~%(DEFUN DECODE-POST (POST)
  \"Takes a JSON document in list form and decodes it into a CLOS POST object\"
  (FLET ((P (KEY)
           (CDR (ASSOC KEY POST))))
    (MAKE-INSTANCE 'POST :NO (P :NO) :NAME (P :NAME) :E-MAIL (P :EMAIL) :BODY
                   (P :COM) :DATE (P :TIME) :REST
                   (REMOVE-IF
                    (LAMBDA (ELT)
                      (MEMBER (CAR ELT)
                              '((:NO :NO) (:NAME :NAME) (:E-MAIL :EMAIL)
                                (:BODY :COM) (:DATE :TIME))
                              :KEY #'CADR :TEST #'EQ))
                    POST))))")
  (declare (type symbol name))
  ;; These flets are outside because they're not needed at run-time
  (flet ((slot-name (prop) (first prop))
         (json-name (prop) (second prop))
         (decoder-function (prop) (third prop)))
    `(defun ,(intern (format nil *accessor-prefix* (symbol-name name)))
         (,name)
       ,(format nil "Takes a JSON document in list form and decodes it into a CLOS ~A object" name)
       ;; This flet is inside the macro-expansion because the json-data  is only available at run-time
       (flet ((p (key) (cdr (assoc key ,name))))
         (make-instance ',name
                        ,@(apply #'append
                                 (mapcar
                                  (lambda (prop)
                                    `(,(slot-name prop) ,(if (decoder-function prop)
                                                             `(funcall ,(decoder-function prop) (p ,(json-name prop)))
                                                             `(p ,(json-name prop)))))
                                  slot/json-list))
                        :rest (remove-if
                               (lambda (elt)
                                 (member (car elt) ',slot/json-list :key #'cadr :test #'eq))
                               ,name))))))
