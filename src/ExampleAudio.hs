module ExampleAudio where

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
intro = stack
    [ introRhythm |- n 24 # sound "supersaw" # amp 0.8 
    , rolled (introChords -| n "36" # sound "supersquare" # amp 0.3 # decay 0.0 # voice 0.5 # resonance 0.0 # lfo 0)
    ]

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

verse1 =
    stack
    [ verse1Rhythm |- n 12 # sound "supersaw" # amp 0.8  
    , rolled (verse1Chords -| n "36" # sound "superpwm" # amp 0.3 # decay 0.0 # voice 0.5 # resonance 0.0 # lfo 0)
    ]

-- verse 2 

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
verse2 = 
    stack
    [ verse2Rhythm |- n 12 # sound "supersaw" # amp 0.8  
    , rolled (verse2Chords -| n "36" # sound "superpwm" # amp 0.3 # decay 0.0 # voice 0.5 # resonance 0.0 # lfo 0)
    ] # cps (slow 8 $ 0.5 + saw)

verse2' = stack
    [ verse2Rhythm |- n 12 # sound "supersaw" # amp 0.8  
    , rolled (verse2Chords -| n "36" # sound "superpwm" # amp 0.3 # decay 0.0 # voice 0.5 # resonance 0.0 # lfo 0)
    ] # cps (slow 8 $ 0.25 + 0.5 * saw2)