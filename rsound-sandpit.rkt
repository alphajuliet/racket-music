#lang racket
; rsound-sandpit.rkt
; Experiments in RSound
; AndrewJ 2018-06-02 
; See https://github.com/jbclements/RSound

(require threading
         rsound
         rsound/piano-tones
         rsound/envelope
         "music.rkt")

(provide (all-defined-out))

; Convert time in seconds to samples
(define (s n)
  (* n FRAME-RATE))

; ---------------------
; Define a MIDI note and event
(struct Note (name octave) #:transparent) ; e.g. (Note 'C 3)
(struct NoteEvent (pitch time duration) #:transparent) ; e.g. (NoteEvent 48 5000 1.0)

; ---------------------
; e.g. (note->midi (Note 'C 3)) -> 48
; Note -> Integer
(define (note->midi note)
  (+ (note->num (Note-name note))
     (+ 12
        (* (Note-octave note) 12))))

; e.g. (pitch-list '(F 3 A 3 C 4 E 4)) → '(53 57 60 64)
; pitch-list :: [NoteName|Integer] → [Integer]
(define (pitch-list lst)
  (for/list ([n (filter-not integer? lst)]
             [v (filter integer? lst)])
    (note->midi (Note n v))))

; Alternative constructor for NoteEvent
; make-event :: Note → Real → Real → NoteEvent
(define (make-event note time dur)
  (NoteEvent (note->midi note) time dur))

; ---------------------
; Sound generators

; Define an ADSR envelope
(define (my-adsr att peak dec sus rel total)
  (envelope-signal `((0 0.0)
                     (,att ,peak)
                     (,(+ att dec) ,sus)
                     (,(- total rel) ,sus)
                     (,total 0.0))))

; Define a simple 2-osc detuned saw patch with a bit of vibrato
; double-saw :: Real -> Signal
(define (saw2 f)
  (network ()
           [lfo <= sine-wave 5]
           [saw1 <= sawtooth-wave (+ (* f 1.005)
                                     (* 1.5 lfo))]
           [saw2 <= sawtooth-wave (+ (* f 0.995)
                                     (* 1.5 lfo))]
           [osc = (+ saw1 saw2)]
           [env <= (my-adsr (s 0.2) 1.0 (s 0.5) 0.8 (s 0.5) (s 2.0))]
           [out = (* 0.1 osc env)]))

; Simple sine wave patch
; sine :: Real -> Signal
(define (sine f)
  (network ()
           [osc <= sine-wave f]
           [out = (* 0.5 osc)]))

; ---------------------
; Play notes

; Play a sound, with a slight ADSR envelope
; note->rs :: Signal -> NoteEvent -> RSound
(define (note->rs sound n)
  (let ([samples (* (NoteEvent-duration n) 44100)]
        #;[env (my-adsr 0.2 1.0 0.2 0.8 0.5)])
    (~>> (NoteEvent-pitch n)
         midi-note-num->pitch
         sound
         (signal->rsound samples)
         #;(rs-mult env))))

; play-note :: Signal -> Note -> ()
; e.g. (play-midi-note double-saw (NoteEvent (Note 'F 3) 0 1.0))
(define (play-note sound note-event)
  (play (note->rs sound note-event)))

; Play a chord of midi notes
; play-chord :: Signal -> [NoteEvent] -> ()
(define (play-chord sound lst)
  (let ([ps (make-pstream)])
    (for ([note lst])
      (pstream-queue ps
                     (note->rs sound note)
                     (NoteEvent-time note)))))

; Play simple chord 
(define (play-chord+ sound lst dur)
  (play-chord saw2
              (map+ (λ (m) (NoteEvent m 10000 dur))
                    (pitch-list lst))))

; Make more responsive
(collect-garbage)

; ---------------------
; Examples

(define (ex1)
  (play-chord+ saw2 '(F 3 A 3 C 4 E 4) 2.0))

; The End