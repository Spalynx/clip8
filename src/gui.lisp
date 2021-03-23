(in-package :clip8)

(defun start-gui ()
  ;;(tk:toplevel-tk)
  ;;(tk:tcl "pack [ttk::label .label -text {Hello world}]")
  ;;(tk:tcl "pack" (tk:tcl[ "ttk::button" ".exit" :text "Exit" :command (tk:event-handler #'tk:destroy)))

  ;;(tk:mainloop)
)

(defun key-event (chip keycode wasreleased)
  "Propogates forward the key-event in the gui to the underlying chip.
   Eventually this _could_ be modified for remapping, if need be.
   CURRENTLY: It's just hardcoded values."
  (let ((key (case keycode
               (59 23)
               (otherwise keycode))))
    (key-event chip key wasreleased)))
