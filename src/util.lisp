(in-package :clip8)


;;TODO: REDO "SAFE" STUFF! My lack of knowledge makes it very unsafe.

(deftype u4 () '(unsigned-byte 4))
(deftype u8 () '(unsigned-byte 8))
(deftype u12 () '(unsigned-byte 12))
(deftype u16 () '(unsigned-byte 16))

(defun limit-of-type (val)
  (if (typep val 'u4) 15
      (if (typep val 'u8) 255
           (if (typep val 'u12) 4095
                (if (typep val 'u16) 65535 9999999999)))))

(defun undeflowp (val) (if (equalp val 0) 't ))
(defun overflowp (val)
  "Checks to see if val is about to overflow on increment."
  ;Was going to allow large inc amounts, but possible YAGNI.
  (if (equalp val (limit-of-type val)) 't 'nil))
            
(defun safedec (val)
  "Safely decrement a given value. If it is zero, it wraps around."
  ;;(if (undeflowp val) (setf val (limit-of-type val)) (decf val)))
  (decf val))

(defun safeinc (val)
  "Safely increment a given value. If it's above type_limit, return zero."
  ;;(if (overflowp val) (setf val 0) (incf val)))
  (incf val))

;;TODO: Actually implement this.
;;Abstract from safein
(defun safe-add (l r)
  "Safely add two numbers. If it's result is above type_limit, return zero."
  (+ l r))

;;TODO: Actually implement this.
(defun safe-sub (l r)
  "Safely subtact two numbers. If it's result is <= 0, wrap around."
  (- l r))

;;TODO: MAKE THIS A MACRO PLS
;;
;;Given a 16 bit number, and a position, this should return only
;; the position value.
;;The intended use of this function is to be able to staple
;; multiple mask positions into specific values.
;; Mask size -> in bits
;; shift size -> in bits
;; Ex.  a := fn(0xFF01, 4, 2) -> 0x0
(defun get-bitmask (number mask_size shift_size)
  "Given a number, and the size of the bit mask, this function returns a slice of a number. Sort of like substring for numbers, but with more thinking."
  (declare (type (unsigned-byte 16) number)
           (type (unsigned-byte 8) mask_size shift_size))
  (let ((mask (- (ASH 1 mask_size) 1)))
    (ASH (LOGAND number (ASH mask shift_size)) (* -1 shift_size))))

;;This works by grabbing higher, then lower than the digit.
;;Ex. (get-digit (1234567 2)) => 6
;;    1234567 % 100 => 67
;;    67 / 10 => 6.7 (floor) => 6
(defun get-digit (number digit)
  "Extracts the digit value of a decimal number. 
Digit is right-to-left, so digit 1 is the furthest right.
If the digit exceeds the size of the number, a zero is returned, so watch out! 
(ex. 1234567 and you want the hundredths place)"
  (when (or (equalp number 0) (equalp digit 0)) 'NIL)
  (let ((digit-higher (expt 10 digit))
        (digit-lower (expt 10 (- digit 1))))
    (floor (mod number digit-higher) digit-lower)))

(defun getx (op)
  "Gets the 4bit X value from a chip8 opcode."
  (get-bitmask op 4 8))

(defun gety (op)
  "Gets the 4bit Y value from a chip8 opcode."
  (get-bitmask op 4 4))

(defun byte-to-bool-list (val)
  "Converts an integer byte (8 bits or less!) to a boolean list (T T T T).
   Has a max limit of 8 numbers! This is just a hack of the original function, 
     because in many cases (flipped bit in highest number), the int might have one
     extra 0 on top!
  (0b11111111) => (T T T T T T T T)."
  (let ((bl (int-to-bool-list val))) 
    (let ((bllen (length bl)))
      (if (<= (length bl) 8) bl
        (subseq bl (- bllen 8) bllen)))))

(defun int-to-bool-list (val)
  "Converts an integer (0b1111) to a boolean list (T T T T)."
  (loop for I downfrom (integer-length val) to 0 collect (logbitp I val)))
