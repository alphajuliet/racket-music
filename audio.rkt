#lang racket
;; audio.rkt
;; Experiments in RSound
;; AndrewJ 2018-06-02
;; See https://github.com/jbclements/RSound

(provide play-chord
         play-scale)

(require threading
         rsound
         rsound/envelope
         rakeda
         "core.rkt"
         "chord.rkt"
         "scale.rkt"
         "sound.rkt")

;; Convert time in seconds to samples
(define (samples n)
  (round (* n FRAME-RATE)))

;; Beats per minute
(define BPM 120)

(define (beats b)
  ;; Return the time in seconds for b beats
  ;; Assumes 4 beats per bar
  (* b (/ 60 BPM)))

;; ---------------------
;; Define a note at a particular pitch
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
  ;; note->midi :: Note -> Natural
  (+ (note->num (Pitch-name note))
     (+ 12 (* 12 (Pitch-octave note)))))

(define (make-event note time dur)
  ;; Alternative constructor for NoteEvent
  ;; make-event :: Note → Real → Real → NoteEvent
  (if (Pitch? note)
      (NoteEvent (note->midi note) time dur)
      ;; else
      (NoteEvent note time dur)))

(define (chord->midi ch [octave 3])
  ;; Turn a chord into a list of midi notes in the given octave.
  ;; chord->midi :: Chord (-> Natural) -> [Natural]
  (let ([root-note (note->midi (Pitch (chord-root ch) octave))]
        [intervals (canonical (chord->num ch))])
    (map (r/+ root-note) intervals)))


(define (scale->midi sc [octave 3])
  ;; Turn a scale into midi notes in the given octave
  ;; scale->midi :: Scale (-> Natural) -> [Natural]
  (let ([root-note (note->midi (Pitch (scale-root sc) octave))]
        [intervals (scale->num sc)])
    (map (r/+ root-note) intervals)))

;; ---------------------
;; Play notes

(define (note->rs sound note)
  ;; Play a sound
  ;; note->rs :: Signal -> NoteEvent -> RSound
  (let ([samples (samples (NoteEvent-duration note))])
    (~>> (NoteEvent-pitch note)
         midi-note-num->pitch
         sound
         (signal->rsound samples))))

(define (play-notes sound lst)
  ;; Play a collection of note events
  ;; play-chord :: Signal -> [NoteEvent] -> ()
  (let ([ps (make-pstream)])
    (for ([note lst])
      (pstream-queue ps
                     (note->rs sound note)
                     (samples (NoteEvent-time note))))))

(define (play-chord sound ch [start 0.0] [dur 1.0] [octave 3])
  ;; Play a chord, e.g. play-chord++ saw2 (chord 'D 'minor)
  ;; play-chord++ :: Signal -> Chord -> ()
  (play-notes sound (map+ (λ (m) (NoteEvent m start dur))
                          (chord->midi ch octave))))

(define (play-scale sound sc [dur 0.3] [octave 3])
  ;; Play a given scale, e.g. (play-scale saw2 (scale 'F 'minor))
  ;; play-scale :: Signal -> Scale -> ()
  (let ([midi (scale->midi sc octave)])
    (play-notes sound (for/list ([m midi]
                                 [i (in-range (length midi))])
                        (NoteEvent m (* i dur) dur)))))

;; Make more responsive
(collect-garbage)

;; ---------------------
;; Examples

(define (ex1)
  ;; Play Fmaj7
  (play-chord saw2 (chord 'E 'minor)))

;; The End
