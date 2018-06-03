#lang racket
; rsound-sandpit.rkt
; Experiments in RSound
; AndrewJ 2018-06-02 
; See https://github.com/jbclements/RSound

(require threading
         rsound
         rsound/piano-tones
         rsound/envelope
         "tonnetz.rkt")

(provide (all-defined-out))

; ---------------------
; Define a MIDI note.
(struct Note (pitch time duration) #:transparent)

; ---------------------
; e.g. (note->midi 'C 3) -> 48
; NoteName -> Integer -> Integer
(define (note->midi note octave)
  (let ([index (note->num note)]
        [base (+ 12 (* octave 12))])
    (+ index base)))

; ---------------------
; Sound generators

; Define a simple 2-osc detuned saw patch with a bit of vibrato
; double-saw :: Real -> Signal
(define (double-saw f)
  (network ()
           [lfo <= sine-wave 5]
           [saw1 <= sawtooth-wave (+ (* f 1.005)
                                     (* 1.5 lfo))]
           [saw2 <= sawtooth-wave (+ (* f 0.995)
                                     (* 1.5 lfo))]
           [osc = (+ saw1 saw2)]
           [out = (* 0.1 osc)]))

; Simple sine wave patch
; sine :: Real -> Signal
(define (sine f (vol 0.1))
  (network ()
           [osc <= sine-wave f]
           [out = (* vol osc)]))

; ---------------------
; Play notes

; note->rs :: Signal -> Note -> RSound
(define (note->rs sound note)
  (let ([samples (* (Note-duration note) 44100)])
    (~>> (Note-pitch note)
         midi-note-num->pitch
         sound
         (signal->rsound samples))))

; play-midi-note :: Signal -> Integer -> ()
; e.g. (play-midi-note double-saw 48)
(define (play-midi-note sound note)
  (play (note->rs sound note)))

; Play a chord of midi notes
; play-chord :: Signal -> [Note] -> ()
(define (play-chord sound lst)
  (let ([ps (make-pstream)])
    (for ([note lst])
      (pstream-queue ps
                     (note->rs sound note)
                     10000))))

; Make responsive
(collect-garbage)

(play-chord double-saw (list (Note 48 0 2.0)
                             (Note 52 0 2.0)
                             (Note 55 0 2.0)
                             (Note 59 0 2.0)))


; The End