module Audio where

-- BLOCK: Intro

-- But first, music
-- With Tidal-Cycles

-- BLOCK: Audio Intro

import Sound.Tidal.Boot

introMelody = cat 
    [ n "~  fs fs fs g   fs b  fs e@8"
    , n "~  e  e  e  cs6 as fs e  d@8" 
    , n "~  a  a  a  b   a  g  fs g@8" 
    , n "~  fs fs fs b   fs gs as b@8"
    ]
introRhythm = cat
    [ n "b'minor   fs'major7"
    , n "fs'minor7 b'minor"
    , n "b'major7  e'minor"
    , n "fs'major7 b'minor"
    ]

-- BLOCK: Play Intro

addSwing = swingBy (1/7) 8

intro = addSwing $ stack
    [ introMelody |- n 12 # sound "supersaw" # amp 0.8 
    , rolled (introRhythm -| n "36" 
        # sound "supersquare" 
        # amp 0.3 
        # decay 0.0 
        # voice 0.5 
        # resonance 0.0 
        # lfo 0)
    ] # cps (80/60/4)

-- BLOCK: Verse 1

verse1Melody = cat
    [ n "g  _  fs _  g  _  fs _  g  fs g  fs fs _  e  _" 
    , n "e  e  e  e  fs _  e  e  fs e  d  cs d  _  ~  ~"
    -- 
    , n "g  _  fs _  g  _  fs _  g  fs g  fs fs _  e  _"
    , n "b  b  a  g  g  _  fs _  fs fs gs as b  _  ~  ~"
    ]
verse1Rhythm = cat 
    [ n "b'minor@3 fs'major7"
    , n "fs'major7@3 b'minor"
    , n "b'minor@2 b'major7 e'minor"
    , n "fs'major7@3 b'minor"
    ]

-- BLOCK: Play Verse 1
melodyInstrument x = stack 
    [ x # amp 0.4 # sound "supersaw"
    , (x |- n "12") # amp 0.8 # sound "supersaw"
    ] # decay 0.25 # attack 0.05 # hold 0.2 # release 0.7 # legato 1.20
rhythmInstrument x = rolled x -| n "48"
        # sound "superpwm" 
        # decay 0.5 # voice 0.5 # resonance 0.0 # lfo 0
        # dry 0.5 # room 0.5 # size 0.5
        # amp 1.2 
        -- # attack 0.1 # hold 2.0 # release 1.0
verse1 =
    addSwing $ stack
    [ melodyInstrument verse1Melody 
    , rhythmInstrument verse1Rhythm
    , s "<[sn sd:3] bd>*16" # amp 0.2 # pan "<0.3 0.6>*8"
    ] # cps (80/60/4)
-- BLOCK: Verse 2

verse2Melody = cat 
    [ n "b  _  b  _  a  _  g  _  b  b  a  g  b  b  a  g"
    , n "fs fs fs fs e  _  fs _  fs e  d  cs b  _  ~  ~"
    --- 
    , n "b  _  b  _  a  _  g  _  b  b  a  g  b  b  a  g"
    , n "fs fs fs fs fs fs fs fs fs _  gs as b  _  ~  ~"
    ]
verse2Rhythm = cat 
    [ n "e'minor b'major7"
    , n "fs'major7@2 fs'major7 b'major7"
    , n "e'minor b'major7"
    , n "fs'major7@2 fs'major7 b'minor"
    ]

-- BLOCK: Tempo
varyTempo inn mid out =
    let a = slow 2 (inn + saw * (mid-inn))
        b = slow 4 (mid)
        c = slow 2 (mid + saw * (out-mid))
    in cps $ cat [a, a, b, b, b, b, c , c]

-- BLOCK: Play Verse 2
verse2 = 
    addSwing $ stack
    [ melodyInstrument verse2Melody
    , rhythmInstrument verse2Rhythm 
    , s "<[sn:2 bd] [sd:3]>*16" # amp 0.2 # pan "<0.3 0.6>*8"
    ] # cps (80/60/4)
    
verse2' = verse2 # varyTempo (80/60/4) (140/60/4) (80/60/4) 

-- BLOCK: Outro

-- And to Jeff Moss