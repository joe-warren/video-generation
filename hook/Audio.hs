module Audio where

-- BLOCK: Audio Intro

-- The Soundtrack: using Tidal Cycles
import Sound.Tidal.Boot

introMelody =
    [ n "~  d  _  d4 a4 g4 fs4 a4 d@8"
    , n "~  e  _  e4 cs e  gs  b  a@8"
    , n "~  g  fs g  a  b  a   g  fs@8"
    , n "~  b4 d  cs e  d  fs  e  d@8"
    ]

introRhythm =
    [ n "d'major a'dom7"
    , n "a'dom7  a'major"
    , n "g'major d'major"
    , n "a'dom7  d'major"
    ]

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

-- BLOCK: Play Intro

addSwing = swingBy (1/6) 8

intro = addSwing $ stack
    [ cat introMelody # pipes
    , rolledBy (1/16) (cat introRhythm -| n "24*8" # vibes)
    ] # cps (180/60/8)

-- BLOCK: Verse 1

verse1Melody =
    [ n "d  _  d4 _  d4 _  a4 g4 fs4 a4 d  cs d  e  fs d"
    , n "e  _  e4 _  e4 _  e  d  cs  e  a  gs a  _  fs _"
    , n "g  fs g  a  b  a  g  fs g   fs e  cs d  cs b4 a4"
    , n "b4 d  cs e  d  fs e  g  fs  _  d  _  d  _  [a4 b4 cs]@2"
    ]

verse1Rhythm =
    [ n "d'major@3 a'dom7"
    , n "a'dom7@2 e'dom7 a'major"
    , n "g'major d'major"
    , n "e'minor a'dom7 d'major@2"
    ]

verse1Drums =
    replicate 3 (s "<cp hh:9>*4" # amp "<0.3 0.1*3>*2" # pan "<0.3 0.7>*4")
    <> [s "[cp hh:9]*8" # amp "<0.3 0.1*3>*2" # pan 0.3]

-- BLOCK: Play Verse 1
melodyInstrument x = stack
    [ (x |+ n "12") # amp 0.4 # pipes
    , x # amp 0.8 # pipes
    ] # legato 1.20

rhythmInstrument x = rolledBy (1/16) (x -| n "12*8")
        # vibes
        # amp "<1.2 0.8*3>*2"

verse1 =
    addSwing $ stack
    [ melodyInstrument $ cat verse1Melody
    , rhythmInstrument $ cat verse1Rhythm
    , cat verse1Drums
    ] # cps (180/60/8)

-- BLOCK: Verse 2

verse2Melody =
    [ n "fs4 a4 d  a4 fs4 a4 d a4 b4 _ g4 _  g4 _ g4 fs4"
    , n "g4  b4 e  cs gs4 b4 e d  cs _ a4 _  a4 _ e  fs"
    , n "g   fs g  a  b   a  g fs g  fs e cs d cs b4 a4"
    , n "b4  d  cs e  d   fs e g  fs  _ d  _  d  _ a4 g4"
    ]

verse2Rhythm =
    [ n "d'major g'major"
    , n "a'dom7 e'dom7 a'major@2"
    , n "g'major d'major"
    , n "e'minor a'dom7 d'major@2"
    ]

verse2Drums = s "<cp [cp hh:9]>*4" # amp "0.3 0.1*7" # pan "<0.3 0.7>*4"

-- BLOCK: Play Verse 2

verse2 =
    addSwing $ stack
    [ melodyInstrument $ cat verse2Melody
    , rhythmInstrument $ cat verse2Rhythm
    , verse2Drums
    ] # cps (180/60/8)


-- BLOCK: Alt Verse 2 Not Shown

verse2' =
    addSwing $ stack
    [ melodyInstrument . cat $ verse2Melody <> verse2Melody <> verse1Melody
    , rhythmInstrument . cat $ verse2Rhythm <> verse2Rhythm <> verse1Rhythm
    , cat (replicate 8 verse2Drums <> verse1Drums)
    ] # cps (180/60/8)

melodyInstrumentAlt x = stack
    [ (x |+ n "12") # amp 0.4 # vibes
    , x # amp 0.8 # vibes
    ] # legato 1.20

rhythmInstrumentAlt x = rolledBy (1/16) (x -| n "12*8")
        # pipes
        # amp "<1.2 0.8*3>*2"

verse2Alt =
    addSwing $ stack
    [ melodyInstrumentAlt . cat $ verse2Melody <> verse2Melody <> verse1Melody
    , rhythmInstrumentAlt . cat $ verse2Rhythm <> verse2Rhythm <> verse1Rhythm
    , cat (replicate 8 verse2Drums <> verse1Drums)
    ] # cps (180/60/8)

playout = n "b4 d  cs e  d  fs e  g  fs  _  d  _  d  _  _ _ " 

playPlayout =
    addSwing $ stack
    [ melodyInstrumentAlt $ cat (playout : replicate 3 silence)
    ] # cps (180/60/8)
