module Audio where

-- BLOCK: Melody

-- Soundtrack: "I Can Sing a Rainbow", using Tidal Cycles
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

-- BLOCK: Vibes

-- an FM synth: operator 2 modulates operator 1, at 3.5 times its frequency
vibes =
      sound "superfm" # amp 0.8
      # fmamp 1 1 # fmamp 2 0 # fmamp 3 0 # fmamp 4 0 # fmamp 5 0 # fmamp 6 0
      # fmratio 1 1 # fmratio 2 3.5 # fmmod 1 2 0.6
      # fmeglevel 1 1 1 # fmeglevel 1 2 0.3 # fmeglevel 1 3 0 # fmeglevel 1 4 0
      # fmegrate 1 1 20 # fmegrate 1 2 0.5 # fmegrate 1 3 0.2 # fmegrate 1 4 1
      # fmeglevel 2 1 1 # fmeglevel 2 2 0 # fmeglevel 2 3 0 # fmeglevel 2 4 0
      # fmegrate 2 1 20 # fmegrate 2 2 0.7 # fmegrate 2 3 1 # fmegrate 2 4 1

-- BLOCK: Verse Parts

rightHand = (|+ n 12) <$> melody

bassLine =
    [ n "c c c  c"
    , n "c c b4 g4"
    , n "c c c  c"
    , n "c@3 ~ a4@2 b4@2 c@4 c ~ c4@2"
    ]

brokenChords =
    [ n "~ g f g  ~ g f g  ~ g e g  ~ g  e g"
    , n "~ a f a  ~ a f a  ~ g e g  ~ g  e g"
    , n "~ g e g  ~ a f a  ~ g e g  ~ gs f gs"
    , n "~ g e c  ~ f ~ f  ~ g e g  ~@4"
    ]

-- BLOCK: Keys

-- loosely based on DX7's "E.PIANO 1", with feedback applied
keys =
      sound "superfm"
      # fmamp 1 0.6 # fmamp 2 0 # fmamp 4 0 # fmamp 5 0.7 # fmamp 6 0
      # fmdetune 1 0.5 # fmdetune 5 (-0.7)
      # fmmod 1 2 0.8 # fmmod 5 6 0.8 # fmmod 6 6 1
      # fmEnvelope 1 [1, 0.75, 0, 0] [20, 0.3, 0.15, 1]
      # fmEnvelope 2 [1, 0.75, 0, 0] [20, 1, 0.3, 1]
      # fmEnvelope 3 [1, 0.95, 0, 0] [20, 0.25, 0.12, 1]
      # fmEnvelope 4 [1, 0.95, 0, 0] [20, 0.4, 0.15, 1]
      # fmEnvelope 5 [1, 0.95, 0, 0] [20, 0.25, 0.12, 1]
      # fmEnvelope 6 [1, 0.95, 0, 0] [20, 0.4, 0.15, 1]
      # fmmod 2 2 1 # fmfeedback 1.2 # fmratio 2 14.3
      # fmmod 3 4 3 # fmdetune 6 3


-- BLOCK: Play Verse

bpmVerse = 130
tempoVerse = cps (bpmVerse/60/8)

verse = stack
    [ cat melody # fmLead # amp 0.6 # legato 1.1
        # orbit 1 # room 0.4 # size 0.8
    , cat rightHand # keys # amp 0.5
    , cat brokenChords # keys # amp 0.35
    , cat bassLine |- n 12 # fmBass # amp 0.8
        # orbit 2 # room 0.8 # size 0.8
    , fmDrums
    ] # tempoVerse

-- BLOCK: Intro With Percussion (Not Shown)

introPercussion = stack
    [ melodyInstrument $ cat melody
    , drums
    ] # tempoIntro

melodyInstrument x = stack
    [ (x |+ n "12") # amp 0.4 # vibes
    , x # amp 0.8 # vibes
    ] # legato 1.20

drums = s "[cb hh:9]*4" # amp "[0.3 0.1]*4" # pan "<0.3 0.7>*4"

-- BLOCK: FM Parameters (Not Shown)

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

-- the four levels and four rates of one operator's envelope
-- (rates are relative to the note length: a segment takes 0.1/rate of the note)
fmEnvelope :: Int -> [Pattern Double] -> [Pattern Double] -> ControlPattern
fmEnvelope op levels rates = foldr1 (#) $
    zipWith (fmeglevel op) [1..] levels <> zipWith (fmegrate op) [1..] rates

-- pitch vibrato, shared by all operators
fmlfofreq, fmlfodepth :: Pattern Double -> ControlPattern
fmlfofreq = pF "lfofreq"
fmlfodepth = pF "lfodepth"

-- BLOCK: Bass

-- after the DX7's "BASS 1" (algorithm 16)
-- with feedback applied
fmBass =
      sound "superfm"
      # fmamp 1 1 # fmamp 2 0 # fmamp 3 0 # fmamp 4 0 # fmamp 5 0.6 # fmamp 6 0
      # fmratio 1 1 # fmratio 2 1 # fmratio 3 1 
      # fmratio 4 10.07 # fmratio 5 0.5 # fmratio 6 0.5 
      # fmmod 5 6 2 # fmmod 6 6 1 # fmmod 1 2 1.5 
      # fmmod 2 2 1 # fmmod 1 3 2 # fmmod 3 4 0.8
      # fmfeedback 1.8
      # fmEnvelope 1 [1, 0.95, 0.3, 0] [20, 1, 0.3, 0.5]
      # fmEnvelope 2 [1, 0, 0, 0] [20, 4, 1, 1]
      # fmEnvelope 3 [0.8, 0.65, 0, 0] [10, 2, 0.3, 1]
      # fmEnvelope 4 [0.9, 0.3, 0, 0] [10, 1, 0.1, 1]
      # fmEnvelope 5 [1, 0.9, 0.6, 0] [20, 1, 0.3, 1]
      # fmEnvelope 6 [1, 0.7, 0.4, 0] [20, 1, 0.3, 1]

-- BLOCK: Lead (Not Shown)

-- loosely based on DX7's "SYN-LEAD 1" (algorithm 18)
-- with feedback applied
fmLead =
      sound "superfm"
      # fmamp 1 1 # fmamp 2 0.3 # fmamp 3 0 # fmamp 4 0 # fmamp 5 0 # fmamp 6 0
      # fmratio 1 1 # fmratio 2 1 # fmratio 3 1 # fmratio 4 2 # fmratio 5 3.01
      # fmmod 1 2 1 # fmmod 1 3 2 # fmmod 3 3 1 # fmmod 1 4 1 # fmmod 4 5 1.2
      # fmEnvelope 1 [1, 0.95, 0.95, 0] [20, 1, 0.12, 0.3]
      # fmEnvelope 2 [1, 0.96, 0.9, 0] [20, 2, 0.12, 0.3]
      # fmEnvelope 3 [0.93, 0.9, 0, 0] [20, 2, 0.2, 1]
      # fmEnvelope 4 [1, 0.9, 0, 0] [20, 1, 0.3, 1]
      # fmEnvelope 5 [1, 0.65, 0.6, 0] [20, 2, 0.2, 1]
      # fmlfofreq 5.5 # fmlfodepth 0.004
      # fmfeedback 1.6 # fmdetune 2 (-2) # fmdetune 1 0.3 

-- BLOCK: FM Drums (Not Shown)

-- hi-hat: a high carrier, modulated hard by inharmonic, fed-back operators, so it's metallic noise
fmHat =
      sound "superfm"
      # fmamp 1 1 # fmamp 2 0 # fmamp 3 0 # fmamp 4 0 # fmamp 5 0 # fmamp 6 0
      # fmratio 1 1 # fmratio 2 1.47 # fmratio 3 2.23
      # fmmod 1 2 6 # fmmod 2 3 4 # fmmod 3 3 1 # fmfeedback 2
      # fmEnvelope 1 [1, 0, 0, 0] [20, 3, 1, 1]
      # fmEnvelope 2 [1, 0, 0, 0] [20, 3, 1, 1]
      # fmEnvelope 3 [1, 0, 0, 0] [20, 3, 1, 1]

-- snare: a low tone with a burst of noise from a wildly fed-back modulator
fmSnare =
      sound "superfm"
      # fmamp 1 1 # fmamp 2 0 # fmamp 3 0 # fmamp 4 0 # fmamp 5 0 # fmamp 6 0
      # fmratio 1 1 # fmratio 2 1
      # fmmod 1 2 8 # fmmod 2 2 1 # fmfeedback 3
      # fmEnvelope 1 [1, 0.3, 0, 0] [20, 2, 0.8, 1]
      # fmEnvelope 2 [1, 0.2, 0, 0] [20, 1.5, 1, 1]

-- superfm has no pitch envelope, so the kick stays an 808
fmDrums = stack
    [ s "808bd:3*8" # amp 0.9
    , n "[~ -5]*4" # fmSnare # amp 0.6
    , n "[~ 36]*8" # fmHat # amp 0.3 # legato 0.5 # pan "<0.4 0.6>*8"
    ]
