#lang racket
;; audio.rkt
;; Experiments in RSound
;; AndrewJ 2018-06-02
;; See https://github.com/jbclements/RSound

(provide (all-defined-out))

(require threading
         rsound
         rsound/envelope
         rakeda
         "core.rkt"
         "chord.rkt"
         "sound.rkt")

;; Convert time in seconds to samples
(define (sec n)
  (* n FRAME-RATE))

;; ---------------------
;; Define a MIDI note
;; e.g. (Pitch 'C 3)
(struct Pitch (name octave)
  #:transparent
  #:guard (λ (note oct _)
            (unless (and (r/in? note (flatten all-notes))
                         (natural? oct))
              (error "Invalid fields: (Pitch <note> <octave>)"))
            (values note oct)))

;; A note at a given start time and duration, both in seconds
;; e.g. (NoteEvent 48 5.0 1.0)
(struct NoteEvent (pitch time duration)
  #:transparent)

;; ---------------------
(define (note->midi note)
  ;; e.g. (note->midi (Pitch 'C 3)) -> 48
  ;; note->midi :: Note -> Integer
  (+ (note->num (Pitch-name note))
     (+ 12 (* 12 (Pitch-octave note)))))

(define (make-event note time dur)
  ;; Alternative constructor for NoteEvent
  ;; make-event :: Note → Real → Real → NoteEvent
  (if (Pitch? note)
      (NoteEvent (note->midi note) time dur)
      ;; else
      (NoteEvent note time dur)))

(define (chord->midi ch (octave 3))
  ;; Turn a chord into a list of midi notes in the given octave.
  ;; chord->note-list :: Chord -> [Integer]
  (let ([root-note (note->midi (Pitch (chord-root ch) octave))]
        [intervals (canonical (chord->num ch))])
    (map (r/+ root-note) intervals)))

;; ---------------------
;; Play notes

(define (note->rs sound note)
  ;; Play a sound
  ;; note->rs :: Signal -> NoteEvent -> RSound
  (let ([samples (sec (NoteEvent-duration note))])
    (~>> (NoteEvent-pitch note)
         midi-note-num->pitch
         sound
         (signal->rsound samples))))


(define (play-notes sound lst)
  ;; Play a collection of midi notes
  ;; play-chord :: Signal -> [NoteEvent] -> ()
  (let ([ps (make-pstream)])
    (for ([note lst])
      (pstream-queue ps
                     (note->rs sound note)
                     (NoteEvent-time note)))))

(define (play-chord sound ch [dur 1.0] [octave 3])
  ;; Play a chord, e.g. (play-chord++ saw2 (chord 'D 'minor))
  ;; play-chord++ :: Signal -> Chord -> ()
  (play-notes sound (map+ (λ (m) (NoteEvent m 10000 dur))
                          (chord->midi ch octave))))

;; Make more responsive
(collect-garbage)

;; ---------------------
;; Examples

(define (ex1)
  ;; Play Fmaj7
  (play-chord saw2 (chord 'E 'minor)))

;; The End
