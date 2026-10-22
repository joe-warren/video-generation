module Audio where

-- BLOCK: Audio Intro

-- The Soundtrack: "I Can Sing a Rainbow", using Tidal Cycles
import Sound.Tidal.Boot

melody =
    [ n "c@2 d@2 e c d@2 e@2 g@2 g@4"
    , n "a   f   d@2 g e c@2 d@8"
    , n "e   f   g   c b@2 a@2 ~@2 g c g@2 f@2"
    , n "~@2 e   g4  e@2 d@2 c@5 ~@3"
    ]

bpmIntro = 100

tempoIntro = cps (bpmIntro/60/8)

intro = cat melody # vibes # tempoIntro

-- BLOCK:  introWithPercussion

introPercussion = stack
    [ melodyInstrument $ cat melody
    , drums
    ] # tempoIntro

-- BLOCK: Parameter Setup

-- sets the amount of operator 'op' in the superfm output mix
-- (1 <= op <= 6)
fmamp :: Int -> Pattern Double -> ControlPattern
fmamp op = pF ("amp" ++ show op)

-- sets the ratio for operator 'op'.
-- the frequency is note * ratio + detune Hz
-- (1 <= op <= 6)
fmratio :: Int -> Pattern Double -> ControlPattern
fmratio op = pF ("ratio" ++ show op)

-- set the detune for operator 'op'
fmdetune :: Int -> Pattern Double -> ControlPattern
fmdetune op = pF ("detune" ++ show op)

-- set the modulation of operator opa by operator opb
-- if opa == opb, then the modulation amount is multiplied by the
-- 'feedback' parameter
fmmod :: Int -> Int -> Pattern Double -> ControlPattern
fmmod opa opb = pF ("mod" ++ show opa ++ show opb)

-- feedback
fmfeedback :: Pattern Double -> ControlPattern
fmfeedback = pF "feedback"

-- Envelope definition: each operator has an envelope with 4 steps
fmeglevel :: Int -> Int -> Pattern Double -> ControlPattern
fmeglevel op step = pF ("eglevel" ++ show op ++ show step)

-- Envelope definition: sets the rate at which the envelope moves
-- between steps.  Low numbers are slow, high numbers are fast.
fmegrate :: Int -> Int -> Pattern Double -> ControlPattern
fmegrate op step = pF ("egrate" ++ show op ++ show step)


-- BLOCK: Pipes

-- Parameters for an FM synth
pipes =
      sound "superfm" # amp 1.6
      # fmamp 1 1 # fmamp 2 0 # fmamp 3 1 # fmamp 4 0 # fmamp 5 0 # fmamp 6 0
      # fmratio 1 1 # fmratio 2 0.5 # fmratio 3 0.26
      # fmdetune 2 1
      # fmmod 1 1 5 # fmmod 1 2 0.5 # fmmod 1 3 0.2 # fmmod 3 2 2
      # fmeglevel 1 1 1 # fmeglevel 1 2 0.25 # fmeglevel 1 3 0 # fmeglevel 1 4 0
      # fmegrate 1 1 20 # fmegrate 1 2 0.05 # fmegrate 1 3 0.1 # fmegrate 1 4 1
      # fmeglevel 2 1 1 # fmeglevel 2 2 0 # fmeglevel 2 3 0 # fmeglevel 2 4 0
      # fmegrate 2 1 2 # fmegrate 2 2 0.3 # fmegrate 2 3 0.7 # fmegrate 2 4 1
      # fmeglevel 3 1 1 # fmeglevel 3 2 0.2 # fmeglevel 3 3 0 # fmeglevel 3 4 1
      # fmegrate 3 1 20 # fmegrate 3 2 0.5 # fmegrate 3 3 0.4 # fmegrate 3 4 1
      # room (range 0.1 0.5 (fast 10 $ tri))


-- BLOCK: Vibes
vibes =
      sound "superfm" # amp 0.8
      # fmamp 1 1 # fmamp 2 0 # fmamp 3 0 # fmamp 4 0 # fmamp 5 0 # fmamp 6 0
      # fmratio 1 1 # fmratio 2 3.5 # fmmod 1 2 0.6
      # fmeglevel 1 1 1 # fmeglevel 1 2 0.3 # fmeglevel 1 3 0 # fmeglevel 1 4 0
      # fmegrate 1 1 20 # fmegrate 1 2 0.5 # fmegrate 1 3 0.2 # fmegrate 1 4 1
      # fmeglevel 2 1 1 # fmeglevel 2 2 0 # fmeglevel 2 3 0 # fmeglevel 2 4 0
      # fmegrate 2 1 20 # fmegrate 2 2 0.7 # fmegrate 2 3 1 # fmegrate 2 4 1

-- BLOCK: Verse Right

pianoRight =
    [ n "c6@2 d6@2 e6 c6 d6@2 e6@2 g6@2 g6@4"
    , n "a6   f6   d6@2 g6 e6 c6@2 d6@8"
    , n "e6   f6   g6   c6 b6@2 a6@2 ~@2 g6 c6 g6@2 f6@2"
    , n "~@2  e6   c6   e6@2 d6@2 c6@5 ~@3"
    ]

-- BLOCK: Verse Bass

pianoBass =
    [ n "c c c  c"
    , n "c c b4 g4"
    , n "c c c  c"
    , n "c@3 ~ a4@2 b4@2 c@4 c ~ c4@2"
    ]

-- BLOCK: Verse Chords

pianoChords =
    [ n "~ g f g  ~ g f g  ~ g e g  ~ g  e g"
    , n "~ a f a  ~ a f a  ~ g e g  ~ g  e g"
    , n "~ g e g  ~ a f a  ~ g e g  ~ gs f gs"
    , n "~ g e c  ~ f ~ f  ~ g e g  ~@4"
    ]

-- BLOCK: Play Verse

bpmVerse = 130

tempoVerse = cps (bpmVerse/60/8)

piano = stack 
    [ stack (cat <$> [pianoRight, pianoBass])
        # s "superpiano" # amp 0.6
    , cat pianoChords 
        # pipes # amp 0.6
    ]
drums = s "[cp hh:9]*4" # amp "[0.3 0.1]*4" # pan "<0.3 0.7>*4"

melodyInstrument x = stack
    [ (x |+ n "12") # amp 0.4 # vibes
    , x # amp 0.8 # vibes
    ] # legato 1.20

verse = stack
    [ melodyInstrument $ cat melody
    , piano
    , drums
    ] # tempoVerse
