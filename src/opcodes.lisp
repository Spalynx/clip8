(in-package :clip8)

(defun op_wombocombo ())

(defun op_NONE (op debug)
  "Throw error on an invalid OP being passed."
  (let ((message (format t "OPCODE DOES NOT EXIST. ~@X" op)))
    (if debug (print message) (error message))))

(defun op_00E0 ()
  #'(lambda (chip)
      "op_00E0 -> Clears the screen."
      (setf (chip-gfx chip)
            (make-array 2048 :element-type 'boolean :initial-element 't))))

(defun op_00EE ()
  #'(lambda (chip)
      "op_00EE -> Returns from a subroutine."
      (when (eq 0 (chip-sp chip))
        (error (format t "Stack... underflow?")))
      
      (decf (chip-sp chip))
      ;;Return from subroutine.
      (setf (chip-pc chip) (aref (chip-stack chip) (chip-sp chip)))))

(defun op_0NNN ()
  "op_00NN -> SYS JUMP, ignored in most impl."
  (op_1NNN))

(defun op_1NNN ()
  #'(lambda (chip)
      "op_1NNN -> Jumps to address NNN."
      (setf (chip-pc chip) (get-bitmask (chip-opcode chip) 12 0))))

(defun op_2NNN ()
  #'(lambda (chip)
      "op_2NNN -> Calls subroutine at NNN."
      (setf (aref (chip-stack chip) (chip-sp chip)) (chip-pc chip))
      (setf (chip-sp chip) (incf (chip-sp chip)))
      (setf (chip-pc chip) (get-bitmask (chip-opcode chip) 12 0))))
      
(defun op_3XKK ()
  #'(lambda (chip)
      "op_3XKK -> Skip next instruction if VX equals KK."
      (let ((X (getx (chip-opcode chip))))
        (when (equalp (aref (chip-v chip) X) (get-bitmask (chip-opcode chip) 8 0))
          (setf (chip-pc chip) (safe-add 2 (chip-pc chip)))))))

(defun op_4XKK ()
  #'(lambda (chip)
      "op_4XKK -> Skips next instruction if VX DOES NOT equal KK."
      (let ((X (getx (chip-opcode chip))))
        (when (equalp (aref (chip-v chip) X) (get-bitmask (chip-opcode chip) 8 0))
              (setf (chip-pc chip) (safe-add 2 (chip-pc chip)))))))

(defun op_5XY0 ()
  #'(lambda (chip)
      "op_5XY0 -> Skips next instruction if VX equals VY."
      (let ((X (getx (chip-opcode chip)))
            (Y (gety (chip-opcode chip))))
        (when (equalp (aref (chip-v chip) X) (aref (chip-v chip) Y))
          (setf (chip-pc chip) (safe-add 2 (chip-pc chip)))))))

(defun op_6XKK ()
  #'(lambda (chip)
      "op_6XKK -> Sets VX to KK."
      (let ((X (getx (chip-opcode chip)))
            (KK (get-bitmask (chip-opcode chip) 8 0)))
        (chip-setf-v chip X KK))))

;I can't find an easy way to simplify array access and such.
;Probably gonna regret some of this haha
(defun op_7XKK ()
  #'(lambda (chip)
      "op_7XKK -> Adds KK to VX. (Carry not changed)"
      (let ((X (getx (chip-opcode chip))))
        (let ((new_val (safe-add (aref (chip-v chip) X)
                                 (get-bitmask (chip-opcode chip) 8 0))))
          (chip-setf-v chip X new_val)))))

(defun op_8XY0 ()
  #'(lambda (chip)
      "op_8XY0 -> Sets VX to the value of VY."
      (let ((X (getx (chip-opcode chip)))
            (Y (gety (chip-opcode chip))))
        (chip-setf-v chip X Y))))

(defun op_8XY1 ()
  #'(lambda (chip)
      "op_8XY1 -> Sets VX to VX or VY (bitwise OR)."
      (let ((X (getx (chip-opcode chip)))
            (Y (gety (chip-opcode chip))))
        (let ((new_val (LOGIOR (aref (chip-v chip) X) (aref (chip-v chip) Y))))
          (chip-setf-v chip X new_val)))))

(defun op_8XY2 ()
  #'(lambda (chip)
      "op_8XY2 -> Sets VX to VX and VY (bitwise and)." 
      (let ((X (getx (chip-opcode chip)))
            (Y (gety (chip-opcode chip))))
        (let ((new_val (LOGAND (aref (chip-v chip) X) (aref (chip-v chip) Y))))
          (chip-setf-v chip X new_val)))))

(defun op_8XY3 ()
  #'(lambda (chip)
      "op_8XY3 -> Sets VX to VX xor VY." 
      (let ((X (getx (chip-opcode chip)))
            (Y (gety (chip-opcode chip))))
        (let ((new_val (LOGXOR (aref (chip-v chip) X) (aref (chip-v chip) Y))))
          (chip-setf-v chip X new_val)))))

(defun op_8XY4 ()
  #'(lambda (chip)
      "op_8XY4 -> Sets VY to VX.
       VF set to 1 when theres a carry, and 0 when there isnt."
      (let ((X (getx (chip-opcode chip)))
            (Y (gety (chip-opcode chip))))
        (let ((new_val (+ (aref (chip-v chip) X) (aref (chip-v chip) Y))))
          (chip-setf-v chip X new_val))
        (if (> (+ (aref (chip-v chip) X) (aref (chip-v chip) Y)) 255)
          (chip-setf-v chip #xF 1)
          (chip-setf-v chip #xF 0)))))

(defun op_8XY5 ()
  #'(lambda (chip)
      "op_8XY5 -> VY is subtracted from VX.
       VF is set to 0 when there's a barrow, and 1 when there isnt."
      (let ((X (getx (chip-opcode chip)))
            (Y (gety (chip-opcode chip))))
        (let ((flag (if (> (aref (chip-v chip) X) (aref (chip-v chip) Y)) 1 0)))
          (chip-setf-v chip #xF flag))
        (let ((new_val (+ (aref (chip-v chip) X) (aref (chip-v chip) Y))))
          (chip-setf-v chip X new_val)))))
      
(defun op_8XY6 ()
  #'(lambda (chip)
      "op_8XY6 -> Stores the least significant bit of VX in VF and shifts VX right by 1."
      (let ((X (getx (chip-opcode chip))))
        (let ((flag (if (equalp 1 (get-bitmask (aref (chip-v chip) X) 1 0))
                        1 0)))
          (chip-setf-v chip #xF flag))
        (chip-setf-v chip X (floor (aref (chip-v chip) X) 2)))))
     
(defun op_8XY7 ()
  #'(lambda (chip)
      "op_8XY7 -> Sets VX to VY minus VX. 
       VF is set to 0 when theres a borrow, and 1 when there isnt."
      (let ((X (getx (chip-opcode chip)))
            (Y (gety (chip-opcode chip))))
        (let ((flag (if (> (aref (chip-v chip) Y) (aref (chip-v chip) X)) 1 0)))
          (chip-setf-v chip #xF flag))
        (chip-setf-v chip X (- (aref (chip-v chip) Y) (aref (chip-v chip) X))))))
      
(defun op_8XYE ()
  #'(lambda (chip)
      "op_8XYE -> Stores most sigificant bit of VX in VF and shifts VX to the left by 1."
      (let ((X (getx (chip-opcode chip))))
        (let ((flag (if (equalp 1 (get-bitmask (aref (chip-v chip) X) 1 15))
                        1 0)))
          (chip-setf-v chip #xF flag))
        (chip-setf-v chip X (* 2 (aref (chip-v chip) X))))))
      
(defun op_9XY0 ()
  #'(lambda (chip)
      "op_9XY0 -> Skips next instruction if VX doesn't equal VY."
      (let ((X (getx (chip-opcode chip)))
            (Y (gety (chip-opcode chip))))
        (when (not (equalp (aref (chip-v chip) X) (aref (chip-v chip) Y)))
          (setf (chip-pc chip) (+ 2 (chip-pc chip)))))))

(defun op_ANNN ()
  #'(lambda (chip)
      "op_ANNN -> Sets I to the address NNN"
        ;self.op_9XY0(); ;;I have no clue why this was here?
        (setf (chip-i chip) (get-bitmask (chip-opcode chip) 12 0))))

(defun op_BNNN ()
  #'(lambda (chip)
      "op_BNNN -> Jumps to the address NNN plus V0"
      (setf (chip-pc chip)
            (+ (get-bitmask (chip-opcode chip) 12 0) (aref (chip-v chip) 0)))))
      
(defun op_CXKK ()
  #'(lambda (chip)
      "op_CXKK -> Sets VX to the result of a bitwise and operation on a random number."
      (let ((X (get-bitmask (chip-opcode chip) 4 8)))
        (chip-setf-v chip X (LOGAND (random 256)
                                    (get-bitmask (chip-opcode chip) 8 0))))))

(defun op_DXYN ()
  #'(lambda (chip)
      "op_DXYN -> Draw an 8*N sprite in memory to gfx.
       Bytes memory[I, ..., I+N] are drawn top to bottom, with a bit rep. a pixel."
      (let ((X (get-Vx chip))
            (Y (get-Vx chip (gety (chip-opcode chip))))
            (N (1- (get-bitmask  (chip-opcode chip) 4 0))))
        (draw-sprite chip X Y N))))

(defun op_EX9E ()
  #'(lambda (chip)
      "op_DXYN -> Skips the next instruction if the key stored in VX is pressed."
      (when (get-key-pressed chip) (incf (chip-pc chip) 2))))

(defun op_EXA1 () 
  #'(lambda (chip)
      "op_EXA1 -> Skips the next instruction if the key stored in VX isn't pressed."
      (unless (get-key-pressed chip) (incf (chip-pc chip) 2))))

(defun op_FX07 ()
  #'(lambda (chip)
      "op_FX07 -> Sets VX to the value of the delay timer."
      (let ((X (get-bitmask (chip-opcode chip) 4 8)))
        (chip-setf-v chip X (time-get-dt chip)))))

(defun op_FX0A () 
  #'(lambda (chip)
      "op_FX0A -> A key press is awaited and then stored in VX."
      (op_NONE (chip-opcode chip) (get-option-debug chip))))

(defun op_FX15 ()
  #'(lambda (chip)
      "op_FX15 -> Sets the delay timer to VX."
      (let ((X (get-bitmask (chip-opcode chip) 4 8)))
        (time-set-dt chip (aref (chip-v chip) X)))))

(defun op_FX18 ()
  #'(lambda (chip)
      "op_FX18 -> Sets the sound timer to VX."
      (let ((X (get-bitmask (chip-opcode chip) 4 8)))
        (time-set-st chip (aref (chip-v chip) X)))))

(defun op_FX1E ()
  #'(lambda (chip)
      "op_FX1E -> Adds VX to I. VF is not affected."
      (let ((X (get-bitmask (chip-opcode chip) 4 8)))
        (setf (chip-i chip)
              (+ (chip-i chip) (aref (chip-v chip) X))))))

(defun op_FX29 ()
  #'(lambda (chip)
      "op_FX29 -> Sets I to the location of the sprite for the character in VX."
      (let ((X (get-bitmask (chip-opcode chip) 4 8)))
        (setf (chip-i chip) (* (aref (chip-v chip) X) 5)))))
        ;;(setf (chip-i chip) (aref (chip-v chip) (* X 5))))))
              
(defun op_FX33 () 
  #'(lambda (chip)
      "op_FX33 -> Store the digits of Vx in memory locations I, I+1, and I+2.
      123 = [1, 2, 3]"
      (let ((I (chip-i chip)) (VX (aref (chip-v chip) (getx (chip-opcode chip)))))
        (setf (aref (chip-memory chip) I) (get-digit VX 3))
        (setf (aref (chip-memory chip) (+ I 1)) (get-digit VX 2))
        (setf (aref (chip-memory chip) (+ I 2)) (get-digit VX 1)))))

;;For the next two:
;;The offset from I is increased by 1 for each value written, but I itself is left unmodified.
(defun op_FX55 ()
  #'(lambda (chip)
      "op_FX55 -> Stores V0 to VX (including VX) in memory starting at address I."
      (let ((X (get-bitmask (chip-opcode chip) 4 8)))
        (loop for N from 0 to (+ X 1) do
          (setf (aref (chip-memory chip) (+ N (chip-i chip)))
                (aref (chip-v chip) N))))))

(defun op_FX65 ()
  #'(lambda (chip)
      "op_FX65 -> Fills V0 to VX (including VX) with values from memory starting at address I."
      (let ((X (get-bitmask (chip-opcode chip) 4 8)))
        (loop for N from 0 to (+ X 1) do
          (chip-setf-v chip N (aref (chip-memory chip) (+ N (chip-i chip))))))))
