module ExampleAudio where

-- BLOCK: Intro

-- But first, music
-- With Tidal-Cycles

-- BLOCK: Audio Intro

import Sound.Tidal.Boot

introRhythm = cat 
    [ n "~  fs fs fs g   fs b  fs e@8"
    , n "~  e  e  e  cs6 as fs e  d@8" 
    , n "~  a  a  a  b   a  g  fs g@8" 
    , n "~  fs fs fs b   fs gs as b@8"
    ]
introChords = cat
    [ n "b'minor   fs'major7"
    , n "fs'minor7 b'minor"
    , n "b'major7  e'minor"
    , n "fs'major7 b'minor"
    ]

-- BLOCK: Play Intro

intro = stack
    [ introRhythm |- n 24 # sound "supersaw" # amp 0.8 
    , rolled (introChords -| n "36" 
        # sound "supersquare" 
        # amp 0.3 
        # decay 0.0 
        # voice 0.5 
        # resonance 0.0 
        # lfo 0)
    ] # cps (80/60/4)

-- BLOCK: Verse 1

verse1Rhythm = cat
    [ n "g  _  fs _  g  _  fs _  g  fs g  fs fs _  e  _" 
    , n "e  e  e  e  fs _  e  e  fs e  d  cs d  _  ~  ~"
    -- 
    , n "g  _  fs _  g  _  fs _  g  fs g  fs fs _  e  _"
    , n "b  b  a  g  g  _  fs _  fs fs gs as b  _  ~  ~"
    ]
verse1Chords = cat 
    [ n "b'minor@3 fs'major7"
    , n "fs'major7@3 b'minor"
    , n "b'minor@2 b'major7 e'minor"
    , n "fs'major7@3 b'minor"
    ]

-- BLOCK: Play Verse 1
slightPan = pan (0.5 + sine * 0.2)  
drumPan = pan "<0.4 0.6 0.3>*8"

verse1 =
    stack
    [ verse1Rhythm |- n 12 # sound "supersaw" # amp 0.8  
    , rolled (verse1Chords -| n "36"
         # sound "superpwm" 
         # amp 0.3 
         # decay 0.0 
         # voice 0.5 # resonance 0.0 # lfo 0
         # slightPan
         )
    , s "<bd bd [sn sd:3]>*16" # delay 0.1 # drumPan
    ] # cps (80/60/4)


-- BLOCK: Verse 2

verse2Rhythm = cat 
    [ n "b  _  b  _  a  _  g  _  b  b  a  g  b  b  a  g"
    , n "fs fs fs fs e  _  fs _  fs e  d  cs b  _  ~  ~"
    --- 
    , n "b  _  b  _  a  _  g  _  b  b  a  g  b  b  a  g"
    , n "fs fs fs fs fs fs fs fs fs _  gs as b  _  ~  ~"
    ]
verse2Chords = cat 
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
    stack
    [ verse2Rhythm |- n 12 # sound "supersaw" # amp 0.8  
    , rolled (verse2Chords -| n "36" 
        # sound "superpwm" 
        # amp 0.3 
        # decay 0.0 
        # voice 0.5 
        # resonance 0.0 
        # lfo 0 # slightPan )
    , s "<sn [sn:2 bd] [sn sd:3]>*16" # delay 0.1 # drumPan
    ] # cps (80/60/4)
    
verse2' = verse2 # varyTempo (80/60/4) (160/60/4) (80/60/4) 

-- BLOCK: Outro

-- And to Jeff Moss