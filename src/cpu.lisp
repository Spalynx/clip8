(in-package :clip8)


(defstruct chip
  "Chip 8 CPU written in Common Lisp."
  (running t :type boolean)
  (ignore 'NIL :type boolean)
  (rom-filename "")
  (opcode 0 :type u16)
  (memory (make-array 4096 :element-type 'u8)
       :type (simple-array u8 (4096)))
  (v (make-array 16 :element-type 'u8)
     :type (simple-array u8 (16))
     :read-only t)
  (i 0 :type u16)
  (pc 0 :type u16)
  (gfx (make-array 2048 :element-type 'boolean :initial-element 'NIL)
     :type (simple-vector 2048))
  (stack (make-array 16 :element-type 'u16)
     :type (simple-array u16 (16)))
  (sp 0 :type u16)
  (key (make-array 16 :element-type 'boolean :initial-element 'NIL)
   :type (simple-vector 16))
  ;; [DEBUG, X-overflow, Y-overflow]
  (option (make-array 3 :element-type 'boolean :initial-element 'NIL)
   :type (simple-vector 3))
  (cycle-delay 0.0025 :type real)
  (dt 0 :type real) ;Delay counter
  (st 0 :type real) ;Sound counter 
  (last-dt 0 :type real) ;Last (UNIX) time the counter timer was set.
  (last-st 0 :type real) ;Last (UNIX) time the counter timer was set.
  )



;(load "util.lisp")
;(load "opcodes.lisp")
;(directory (make-pathname :name :wild :type :wild))

(defun start (&optional (chip (make-chip)))
  "(CLI) Boot the chip, then run it in an auto cycle loop."
  (boot chip)
  (run chip (get-option-debug chip)))

(defun boot (chip)
  "Fully boot the chip8 emulator."
  (load-rom chip
            (namestring (truename "~/stash/Dropbox/dev/clip8/roms/pong.c8")))
  (print "Starting Chip8.")
  (fill (chip-memory chip) 0)

  (setf (chip-running chip) 't
        (aref (chip-option chip) 0) 't ;;set DEBUG
        (chip-pc chip) #x200)
        ;(chip-v chip) 0)

  ;;Fill Memory.
  (load-hex-sprites chip)
  (read-romfile chip (chip-rom-filename chip)))

(defun load-rom (chip filename)
    (print "Setting ROM filename.")
    (setf (chip-rom-filename chip) filename))


;;Get read-file-into-byte-vector is from alexandria
(defun read-romfile (chip filename)
  (print "Loading ROM into memory.")
  (replace (chip-memory chip)
           (alexandria:read-file-into-byte-vector filename)
           :start1 #x200))

(defun load-hex-sprites (chip)
  "Load a const list of sprites into memory.
   Chip8 has a library of sprite shapes by default in the first 80 values.
   These will be drawn @SEE op_DXYN by pointing to these with @SEE op_FX29.
  "
  (let ((sprites ;;Possibly move to const if it's an eyesore.
          '(#xF0 #x90 #x90 #x90 #xF0 ;0
            #x20 #x60 #x20 #x20 #x70 ;1
            #xF0 #x10 #xF0 #x80 #xF0 ;2
            #xF0 #x10 #xF0 #x10 #xF0 ;3
            #x90 #x90 #xF0 #x10 #x10 ;4
            #xF0 #x80 #xF0 #x10 #xF0 ;5
            #xF0 #x80 #xF0 #x90 #xF0 ;6
            #xF0 #x10 #x20 #x40 #x40 ;7
            #xF0 #x90 #xF0 #x90 #xF0 ;8
            #xF0 #x90 #xF0 #x10 #xF0 ;9
            #xF0 #x90 #xF0 #x90 #x90 ;A
            #xE0 #x90 #xE0 #x90 #xE0 ;B
            #xF0 #x80 #x80 #x80 #xF0 ;C
            #xE0 #x90 #x90 #x90 #xE0 ;D
            #xF0 #x80 #xF0 #x80 #xF0 ;E
            #xF0 #x80 #xF0 #x80 #x80))) ;F
    ;;Assign the sprites starting with first val in mem.
    (map-into (chip-memory chip) #'(lambda (-) -) sprites)))

(defun run (chip debug)
  "Loops a FDE and various increments. Ends only on input/program itsself ends."

  ;;Shutdown sequence
  ;Possibly might need? (>= (length (chip-memory chip)) (chip-pc chip))
  (when (not (chip-running chip))
    (progn (print "Stopping Chip8")
           (return-from run 'NIL)))

  ;;Don't do anything when we're ignoring.
  (when (not (chip-ignore chip))
    (let ((operation (decode (fetch chip))))
      (progn
        (if 'debug (format t "~t~t~s" (documentation operation 'function)))
        (funcall operation chip)
        (if 'debug (debug-print-screen chip))
        (incf (chip-pc chip) 2)))) ;;Two u8 values for the opcode.

  ;;Recurse
  (time-update-counters chip)
  (sleep (chip-cycle-delay chip))
  (run chip debug))

(defun fetch (chip)
  "Fetch the next opcode from mem, place in CPU and return it."
  (let ((new-op (LOGIOR (ASH (aref (chip-memory chip) (chip-pc chip)) 8)
                        (aref (chip-memory chip) (+ 1 (chip-pc chip))))))
    (setf (chip-opcode chip) new-op))
  
  ;(print (format t "Fetched OPCode. ~@X" (chip-opcode chip)))
  (chip-opcode chip))

(defun decode (op)
  "Given a 16-bit opcode, returns an Operation function."
  ;;(when (not (equalp op 0)) (print (format t "~%Decoding OPCode. ~@X" op)))
  (format t "~%Decoding OPCode. ~@X" op)
    (case (get-bitmask op 4 12)
      (#x0 (case op ;Return from subroutine.
            (#xE0 (op_00E0))
            (#xEE (op_00EE))
            (otherwise (op_0NNN))))
        (#x1 (op_1NNN))
        (#x2 (op_2NNN))
        (#x3 (op_3XKK))
        (#x4 (op_4XKK))
        (#x5 (op_5XY0))
        (#x6 (op_6XKK))
        (#x7 (op_7XKK))
        ;;TODO THIS AND 0xE are wrong
        (0x8 (case (get-bitmask op 4 0) ;#b1111
            (#x0 (op_8XY0))
            (#x1 (op_8XY1))
            (#x2 (op_8XY2))
            (#x3 (op_8XY3))
            (#x4 (op_8XY4))
            (#x5 (op_8XY5))
            (#x6 (op_8XY6))
            (#x7 (op_8XY7))
            (#xE (op_8XYE))
            (otherwise (op_NONE op 'NIL))))
        (#x9 (op_9XY0))
        (#xA (op_ANNN))
        (#xB (op_BNNN))
        (#xC (op_CXKK))
        (#xD (op_DXYN))
        (#xE (case (get-bitmask op 8 0)
            (0x9E (op_EX9E))
            (0xA1 (op_EXA1))
            (otherwise (op_NONE op 'NIL))))
        (#xF (case (get-bitmask op 8 0) ;#b11111111
            (#x07 (op_FX07))
            (#x0A (op_FX0A))
            (#x15 (op_FX15))
            (#x18 (op_FX18))
            (#x1E (op_FX1E))
            (#x29 (op_FX29))
            (#x33 (op_FX33))
            (#x55 (op_FX55))
            (#x65 (op_FX65))
            (otherwise (op_NONE op 'NIL))))
        (otherwise (op_NONE op 'NIL))))


(defun key-event (chip key pressed :type 'boolean)
  (setf (aref (chip-key chip) key) pressed))

(defun get-option-debug (chip)
  "Get the cpu debug flag."
  (aref (chip-option chip) 0))

(defun get-option-x-overflow (chip)
  "Get the X overflow debug flag, @SEE op_DXYN."
  (aref (chip-option chip) 1))
(defun get-option-y-overflow (chip)
  "Get the Y overflow debug flag, @SEE op_DXYN."
  (aref (chip-option chip) 2))

(defun chip-setf-v (chip index new_val)
  "Set the value of an index in V."
  (setf (aref (chip-v chip) index) new_val))

(defun get-Vx (chip &optional (x (getx (chip-opcode chip))))
  "Gets chip.v[X], a common operation. See clip8:getx. "
  (aref (chip-v chip) x))

(defun get-key-pressed (chip)
  "Returns the boolean pressed value of the keycode stored in Vx."
  (aref (chip-key chip) (get-Vx chip)))

(defun get-memory-byte (chip index)
  "Returns the value of an index in memory. Just a code clean-up function."
  (aref (chip-memory chip) index))

(defun debug-print-screen (chip)
  "Print the (gfx) screen to standard output. Great for figuring out draws."
  (format t "~%")
  (let ((gfx (chip-gfx chip)))
    (loop for y from 0 to 31 do
        (loop for x from 0 to 63 do
          (format t "~:[█~; ~]" (aref gfx (+ x (* 64 y)))))
        (format t "~%")))
  (format t "~%"))

(defun draw-sprite (chip X Y N)
  "
  Essentially, the functionality of op_DXYN by proxy: This function draws sprites.
  Personally, I kind of want to rewrite this to just receive a sprite and draw it,
   but sunken cost fallacy smh.
  As it is, this function pulls data from memory, then draws it to gfx.
  If you notice any wierd shit, it's probably a missed increment/decrement...  

  Library:
  jY            -> The actual y value to draw, accounting for overflow set/unset.
  x-start-index -> Listen carefully: The sum of the x value on the line (so 0-63,
                   the index of the start of the line, and the length of 8 - 
                   bytelist. I considered it more efficient to just account for a
                   smaller list, rather than enforcing an 8 long list!

  Have a nice day.
  "
  (let ((I (chip-i chip)))
    (when (>= (+ I N) 4096) (error "Index of I+N does not exist in memory!"))
    (loop FOR j FROM 0 to N DO
      (let ((jY (if (get-option-y-overflow chip) (MOD (+ j Y) 32) (+ j Y)))
            (bytelist (byte-to-bool-list (aref (chip-memory chip) (+ j I)))))
        (let ((x-start-index (+ X (* jY 64) (- 8 (length bytelist)))))
          (unless (> Y 31) (draw-sprite-line chip x-start-index bytelist)))))))


(defun draw-sprite-line (chip start-index bytelist)
  "A list of bytes (T T NIL T) is drawn onto the chip-gfx list at a given index.
   A notable gotcha is x-overflow. Depending on it being set (in chip-options), 
   this function will draw the overflow on the same line, but at the beginning."

  (if (x-wrap-aroundp start-index bytelist)
    (let ((split-bytelist (draw-sprite-line-split-lists start-index bytelist))
          (line-start (- start-index (MOD start-index 64))))
      (draw-line-loop chip start-index (car split-bytelist))
      (when (get-option-x-overflow chip)
        (draw-line-loop chip line-start  (car (reverse split-bytelist)))))
    (draw-line-loop chip start-index bytelist)))

(defun draw-line-loop (chip start-index bytelist)
  (loop FOR i FROM 0 TO (1- (length bytelist)) DO
    (draw-pixel chip (+ start-index i) (nth i bytelist))))


(defun draw-pixel (chip index on)
  "Given the location of the pixel, and it's on/off status (boolean):
   A pixel is drawn to the chip graphics list, and check for a pixel collision."
  (chip-setf-v chip #xF (if (check-pixel-collision chip index on) 1 0))
  (setf (aref (chip-gfx chip) index) on))

(defun draw-sprite-line-split-lists (start-index bytelist)
  "Split the list at the point where it would go over, and return both sides.
   This function SHOULD return a list containing two sub-list splits of bytelist."
  (let ((list-split-index (1- (- 64 (MOD start-index 64)))))
    (when (<= list-split-index 0) 'NIL)
    (list (subseq bytelist 0 (1+ list-split-index))
      (subseq bytelist (1+ list-split-index) (1- (length bytelist))))))

(defun check-pixel-collision (chip index on)
  "Just check if an index in gfx is going to be overwritten."
  (and on (not (aref (chip-gfx chip) index))))

(defun x-wrap-aroundp (start-index bytelist)
  "Predicate: Is an x wraparound going to happen with these parameters?"
  (< (MOD (+ start-index (length bytelist)) 64) (MOD start-index 64)))

(defun time-update-counters (chip)
  "Calculates the current counter value based on the set timestamps, and the clock 
   speed of the emulator. This changes both timers, because why the fnot."
  (let ((dt (chip-dt chip))
        (st (chip-st chip))
        (ldt (chip-last-dt chip))
        (lst (chip-last-st chip))
        (time (get-universal-time))
        (timer-cycle-hz 60))
    (let ((new-dt (/ (- time ldt) timer-cycle-hz))
          (new-st (/ (- time lst) timer-cycle-hz)))
      (unless (equalp ldt 0)
        (time-set-dt chip (if (> dt new-dt) (- dt new-dt) 0)))
      (unless (equalp lst 0)
        (time-set-st chip (if (> st new-st) (- st new-st) 0))))))

(defun time-set-dt (chip val)
  "Set the delay timer, and timestamp."
  (setf (chip-dt chip) val)
  (setf (chip-last-dt chip) (get-universal-time)))
(defun time-set-st (chip val)
  "Set the sound timer, and timestamp."
  (setf (chip-st chip) val)
  (setf (chip-last-st chip) (get-universal-time)))
(defun time-get-dt (chip) (time-update-counters chip) (chip-dt chip))
(defun time-get-st (chip) (time-update-counters chip) (chip-dt chip))
