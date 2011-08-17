;;; Improv Buddy by Gabriel Gilder. Copyright 2003.

;;; ============== INITIALIZATION OF MIDISHARE SYSTEM ==============

(load "ccl:MidiShare.lisp") ; load MidiShare.lisp to initialize all the MidiShare functions, etc

; report status and version of MidiShare
(if (midishare)
  (format t "MidiShare version ~2d is running." (midigetversion))
  (format t "Oh no! MidiShare is not running.")
  ) 

; register MCL as a MidiShare client, store reference number used for MidiShare functions
(defparameter *refnum* (midiopen "MCL"))

; a couple of MidiShare-related functions: list-appls displays MS clients, list-dest displays destinations for MCL,
; list-src displays sources for MCL - on the whole, these are not useful
(defun list-appls ()
  (format t "List of MidiShare client applications ~%")
  (dotimes (i (MidiCountAppls))
    (let ((ref (MidiGetIndAppl (1+ i))))
      (format t 
              " ~2d : reference number ~2d, name : '~a' ~%" 
              (1+ i) ref (MidiGetName ref)))))

(defun list-dest (ref1)
   (format t 
           "List of the destinations of '~a' (ref num = ~d) ~%" 
           (MidiGetName ref1) ref1)
   (dotimes (i (MidiCountAppls))
     (let ((ref2 (MidiGetIndAppl (1+ i))))
       (if (MidiIsConnected ref1 ref2)
         (format t " --> '~a' (ref num = ~d) ~%"
                 (MidiGetName ref2)  
                 ref2)))))

(defun list-src (ref1)
   (format t 
           "List of the sources of '~a' (ref num = ~d) ~%" 
           (MidiGetName ref1) ref1)
   (dotimes (i (MidiCountAppls))
     (let ((ref2 (MidiGetIndAppl (1+ i))))
       (if (MidiIsConnected ref2 ref1)
         (format t " <-- '~a' (ref num = ~d) ~%"
                 (MidiGetName ref2)  
                 ref2)))))


;;; ============== INITIALIZE GLOBALS AND FUNCTIONS ==============

(defparameter *delay-before-response* 1000)   ; delay before program responds to an input phrase

(defparameter *last-note-off* 0)    ; initialize time of last note-off event
(defparameter *note-register* ())   ; initialize list of notes that are being held down
(defparameter *phrase* ())
(defparameter *phrase-db* ())
(defparameter *composing* ())

; MIDI handling functions

(defun waiting-events-p (refnum)   ; this function gets called continuously to poll for MIDI events
  (let ((evs (midicountevs refnum)))
    (unless (> evs 0)
      (if (and                       ; compose a response if:
           (> (MidiGetTime) 
              (+ *delay-before-response* *last-note-off*))   ; 1. delay time has elapsed,
           (> *last-note-off* 0)     ; 2. last-note-off time is greater than 0 (0 indicates no phrase input),
           (null *note-register*)   ; 3. no notes are being held down
           (null *composing*))      ; 4. no response is being composed
        (compose) ; compose a response: probably this will need some arguments?
        )
      )
    (> evs 0) ; waiting-events-p returns T if there are new events to process, nil otherwise
    )
)
  
(defun midi-process (refnum) ; process incoming MIDI data
  (loop
    (do ((event (MidiGetEv refnum) (MidiGetEv refnum))) ; process each Midi event in the queue
        ((%null-ptr-p event))                           ; unless there aren't any
      (collect-note event))
    (process-wait "waiting events" #'waiting-events-p refnum)
    )
)

; Composition and Analysis functions

(defun compose ()
  (setq *composing* T)
  (format t "~%Responding...")
  (analyse)
  (setq phrase-length (length *phrase*))

  ; compose...
  (setq first-note (first (random-from-list *phrase-db*)))
  (setq response (cons first-note
                       (expand-relative-ontimes 
                        (cons first-note 
                              (notestring first-note (1- phrase-length))))))

  ; play response...
  (play-notes (add-to-ontimes (MidiGetTime) response))

  (reset-phrase-vars) ; clear all analysis variables for the next phrase
  (setq *composing* ())
)

(defun analyse ()
  (format t "~%Analyzing...")
  (setq *phrase* (sortcar '< (translate-events (sort-list *phrase*))))
  (setq *phrase* (add-to-ontimes (- 0 (first (first *phrase*))) *phrase*))
  (setq *phrase-db* (cons *phrase* *phrase-db*))
  (format t "~%Phrase database is ~2d items long." (length *phrase-db*))
)

(defun notestring (first-event phrase-length)
  (if (zerop phrase-length)()
      (let ((newnote (search-for-new-note first-event)))
        (if (null newnote)(setq newnote (list (+ 100 (random 500)) (cdr (first (random-from-list *phrase-db*))))))
        (cons newnote (notestring newnote (1- phrase-length)))
        )
      )
)

(defun search-for-new-note (preceding-event)
  (random-from-list
   (remove nil
           (make-ontimes-relative 
            (flatten-list 
             (remove nil 
                     (search-db-for-pitch (second preceding-event) *phrase-db*))))
           )
   )
)

(defun search-db-for-pitch (pitch db)
  (if (null db)()
      (cons (remove nil (search-phrase-for-pitch pitch (first db)))
            (search-db-for-pitch pitch (cdr db))
            )
      )
)

(defun search-phrase-for-pitch (pitch phrase)
  (if (null phrase)()
      (if (= (second (first phrase)) pitch)
        (cons (list (first phrase) (second phrase)) (search-phrase-for-pitch pitch (cdr phrase)))
        (cons nil (search-phrase-for-pitch pitch (cdr phrase)))
        )
      )
)

(defun collect-note (event)
  (if (member (type event) (list typeKeyOff))
    (progn
      (setq *last-note-off* (date event)) ; set last-note-off
      (setq *note-register* (remove (pitch event) *note-register*)) ; remove note from register
      (setq *phrase* (cons (list (pitch event) (date event) 0) *phrase*))
      )
   ())
  (if (member (type event) (list typeKeyOn)) ; note on event
    (progn 
      (setq *note-register* (cons (pitch event) *note-register*)) ; add to note register 
      (setq *phrase* (cons (list (pitch event) (date event) (vel event)) *phrase*))
    ))
)

(defun translate-events (eventlist)
  (if (or (null eventlist) (null (second eventlist))) ()
      (cons (list (second (first eventlist)) 
                  (first (first eventlist))
                  (- (second (second eventlist)) (second (first eventlist)))
                  (third (first eventlist))
                  ) 
            (translate-events (cddr eventlist)))
      )
)

(defun add-to-ontimes (amount eventlist)
  (if (null eventlist)()
      (cons (append (list (+ (first (first eventlist)) amount)) (cdr (first eventlist)))
            (add-to-ontimes amount (cdr eventlist))))
)

(defun make-ontimes-relative (eventlist)
  (if (null eventlist)()
      (let ((event (first eventlist)))
        (if (null (second event)) (setq temp nil)
            (setq temp (cons (- (first (second event)) (first (first event))) (cdr (second event))))
            )
        (cons temp (make-ontimes-relative (cdr eventlist))) 
        )
      )
)

(defun expand-relative-ontimes (eventlist)
  (if (null (second eventlist)) ()
      (let ((newnote (cons (+ (first (second eventlist)) (first (first eventlist)))
                           (cdr (second eventlist)))))
        (cons newnote
              (expand-relative-ontimes (cons newnote (cddr eventlist)))
              )
        )
      )
)

;;; list sorting functions

(defun sort-list (L)
  (if (null L)
    NIL
    (let ((p (car L))); let p=(car L) in the following expressions
      (append 
       (sort-list (less_than (first p) L 0))
       (second-sort-list (equal_to (first p) L 0))
       (sort-list (greater_than (first p) L 0))
       )
      )
    )
)

(defun less_than (item l key)
  (cond
   ((Null L)	NIL)
   ((< (nth key (car L)) item) (cons (car L) (less_than item (cdr l) key)))
   (T	 (less_than item (cdr l) key))
   )
)

(defun equal_to (item l key)
  (cond
   ((Null L)	NIL)
   ((= (nth key (car L)) item) (cons (car L) (equal_to item (cdr l) key)))
   (T	 (equal_to item (cdr l) key))
   )
)

(defun greater_than (item l key)
  (cond
   ((Null L)	NIL)
   ((> (nth key (car L)) item) (cons (car L) (greater_than item (cdr l) key)))
   (T	 (greater_than item (cdr l) key))
   )
)


(defun second-sort-list (L)
  (if (null L)
    NIL
    (let ((p (car L))); let p=(car L) in the following expressions
      (append 
       (sort-list (less_than (second p) L 1))
       (equal_to (second p) L 1)
       (sort-list (greater_than (second p) L 1))
       )
      )
    )
)

(defun flatten-list (the-list)
  (if (null the-list) ()
      (if (listp (first the-list))
        (append (first the-list) (flatten-list (cdr the-list)))
        (cons (first the-list) (flatten-list (cdr the-list)))
        )
      )
)

(defun play-notes (notelist)
  (if (null notelist)()
      (let ((note (first notelist)))
        (makenote (first note) (second note) (third note) (fourth note))
        (play-notes (cdr notelist))
        )
      )
)

(defun makenote (time pitch duration velocity)
  (let ((event (MidiNewEv typeNote)))   ; allocate a new note
    (unless (%null-ptr-p event)	        ; if the allocation was succesful:
      (chan event 0)			; set the midi channel to 0 (means channel 1)
      (port event 1)			; set the destination port to Modem
      (field event 0 pitch)		; set the pitch field
      (field event 1 velocity)		; set the velocity field
      (field event 2 duration)	        ; set the duration field

      (MidiSendAt *refnum* 		; send a copy of the original note
                  (MidiCopyEv event)	 
                  time)
      (MidiFreeEv event) )		; dispose the original note
    )
)

(defun random-from-list (the-list)
  (nth (random (length the-list)) the-list)
)



(defun reset-vars ()
  (reset-phrase-vars)
)

(defun reset-phrase-vars () ; initializes all phrase-related variables to their defaults.
  (setq *phrase* ())
  (setq *last-note-off* 0)
  (setq *note-register* ())
)

(defun Start-Improv-Buddy () ; run this function to initialize and get everything going
  (reset-vars)   ; set up variables
  (MidiFlushEvs *refnum*)  ; get rid of any events that have built up in the queue
  ; apparently this code sets up a "process" or "thread" or something that handles incoming MIDI data:
  (defparameter *midishare-process* 
    (process-run-function '(:name "midi-process" :priority 2) #'midi-process *refnum*))
)

(defun Stop-Improv-Buddy () ; run this function to kill MIDI processing
  (process-kill *midishare-process*)
)

(defun Quit-Improv-Buddy () ; to clean up
  (process-kill *midishare-process*)
  (MidiClose *refnum*)
)


;;; ============== USER NOTIFICATION ==============
(pprint "Make sure to connect MidiShare to MCL at this point.")
; because the functions to do it automatically don't work =P
; with MidiShare 1.86, at least... doesn't anybody check this stuff?
(pprint "Then, you can use the functions start-improv-buddy and stop-improv-buddy to control the program.")
(pprint "Be sure to call quit-improv-buddy when you're done.")



#| ;;; ============== EXAMPLE: ==============

(start-improv-buddy)
; play some notes
; have fun
(stop-improv-buddy)
(quit-improv-buddy)
; this should be streamlined in the future, as well as put into a convenient menu

|#