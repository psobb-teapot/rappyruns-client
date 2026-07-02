;;; Quest trigger definitions for the ephinea-ta client.
;;; Transcribed from psostats-client questDefinitions.go (MIT,
;;; https://github.com/phelix-/psostats-client).
;;;
;;; Entry keys:
;;;   :slug    server quest slug (must match ephinea-ta seed.lisp slugify)
;;;   :episode site episode (1, 2, 4)
;;;   :names   in-game quest names (memory) that map to this entry
;;;   :number  in-game quest number (matched first when present)
;;;   :start   (:register N) set | (:warp-in) | (:floor-switch FLOOR ID)
;;;   :end     (:register N) set | (:floor-switch FLOOR ID)
;;;
;;; This file is read with *READ-EVAL* off; keep it pure data.

(;; ------------------------------------------------ Episode 1 - Extermination
 (:slug "ep1-mop-up-operation-1" :episode 1 :number 101
  :names ("Mop-up Operation #1") :start (:register 0) :end (:register 254))
 (:slug "ep1-mop-up-operation-2" :episode 1 :number 102
  :names ("Mop-up Operation #2") :start (:register 0) :end (:register 254))
 (:slug "ep1-mop-up-operation-3" :episode 1 :number 103
  :names ("Mop-up Operation #3") :start (:register 0) :end (:register 254))
 (:slug "ep1-mop-up-operation-4" :episode 1 :number 104
  :names ("Mop-up Operation #4") :start (:register 0) :end (:register 254))
 (:slug "ep1-sweep-up-operation-1" :episode 1 :number 1761
  :names ("Sweep-up Operation #1") :start (:register 210) :end (:register 254))
 (:slug "ep1-sweep-up-operation-2" :episode 1 :number 1762
  :names ("Sweep-up Operation #2") :start (:register 210) :end (:register 254))
 (:slug "ep1-sweep-up-operation-3" :episode 1 :number 1763
  :names ("Sweep-up Operation #3") :start (:register 210) :end (:register 254))
 (:slug "ep1-sweep-up-operation-4" :episode 1 :number 1764
  :names ("Sweep-up Operation #4") :start (:register 210) :end (:register 254))
 (:slug "ep1-endless-nightmare-1" :episode 1 :number 108
  :names ("Endless Nightmare #1") :start (:warp-in) :end (:register 30))
 (:slug "ep1-endless-nightmare-2" :episode 1 :number 109
  :names ("Endless Nightmare #2") :start (:warp-in) :end (:register 30))
 (:slug "ep1-endless-nightmare-3" :episode 1 :number 110
  :names ("Endless Nightmare #3") :start (:warp-in) :end (:register 30))
 (:slug "ep1-endless-nightmare-4" :episode 1 :number 111
  :names ("Endless Nightmare #4") :start (:warp-in) :end (:register 30))
 (:slug "ep1-anomalous-ordeal-1" :episode 1 :number 1810
  :names ("Anomalous Ordeal #1") :start (:register 82) :end (:register 254))
 (:slug "ep1-anomalous-ordeal-2" :episode 1 :number 1811
  :names ("Anomalous Ordeal #2") :start (:register 82) :end (:register 254))
 (:slug "ep1-scarlet-realm-1" :episode 1 :number 1827
  :names ("Scarlet Realm #1") :start (:register 5) :end (:register 254))
 (:slug "ep1-scarlet-realm-2" :episode 1 :number 1828
  :names ("Scarlet Realm #2") :start (:register 7) :end (:register 254))
 (:slug "ep1-scarlet-realm-3" :episode 1 :number 1829
  :names ("Scarlet Realm #3") :start (:register 25) :end (:register 254))
 (:slug "ep1-scarlet-realm-4" :episode 1 :number 1830
  :names ("Scarlet Realm #4") :start (:register 4) :end (:register 254))
 (:slug "ep1-silent-afterimage-1" :episode 1 :number 1865
  :names ("Silent Afterimage #1") :start (:register 210) :end (:register 50))
 (:slug "ep1-silent-afterimage-2" :episode 1 :number 1866
  :names ("Silent Afterimage #2") :start (:register 210) :end (:register 50))
 (:slug "ep1-chronocide-trial-1" :episode 1 :number 1869
  :names ("Chronocide Trial #1") :start (:register 210) :end (:register 50))
 (:slug "ep1-chronocide-trial-2" :episode 1 :number 1870
  :names ("Chronocide Trial #2") :start (:register 210) :end (:register 50))
 (:slug "ep1-chronocide-trial-3" :episode 1 :number 1871
  :names ("Chronocide Trial #3") :start (:register 210) :end (:register 50))
 (:slug "ep1-chronocide-trial-4" :episode 1 :number 1872
  :names ("Chronocide Trial #4") :start (:register 210) :end (:register 50))
 ;; ------------------------------------------------ Episode 1 - Maximum Attack
 (:slug "ep1-maximum-attack-4th-stage-1a" :episode 1 :number 144
  :names ("Maximum Attack 4 -1A-" "Maximum Attack 4th Stage -1A-")
  :start (:floor-switch 4 99) :end (:floor-switch 10 31))
 (:slug "ep1-maximum-attack-4th-stage-1b" :episode 1 :number 145
  :names ("Maximum Attack 4 -1B-" "Maximum Attack 4th Stage -1B-")
  :start (:floor-switch 4 99) :end (:floor-switch 10 32))
 (:slug "ep1-maximum-attack-4th-stage-1c" :episode 1 :number 146
  :names ("Maximum Attack 4 -1C-" "Maximum Attack 4th Stage -1C-")
  :start (:floor-switch 4 99) :end (:register 254))
 (:slug "ep1-maximum-attack-e-episode-1" :episode 1 :number 942
  :names ("Maximum Attack E: Episode 1")
  :start (:floor-switch 2 0) :end (:register 254))
 (:slug "ep1-random-attack-xrd-stage" :episode 1 :number 1303
  :names ("Random Attack Xrd Stage") :start (:warp-in) :end (:register 254))
 (:slug "ep1-random-attack-xrd-rev-1" :episode 1 :number 1801
  :names ("Random Attack Xrd REV 1") :start (:warp-in) :end (:register 254))
 ;; ------------------------------------------------ Episode 1 - Retrieval
 (:slug "ep1-lost-heat-sword" :episode 1 :number 105
  :names ("Lost HEAT SWORD") :start (:warp-in) :end (:register 15))
 (:slug "ep1-lost-ice-spinner" :episode 1 :number 106
  :names ("Lost ICE SPINNER") :start (:warp-in) :end (:register 15))
 (:slug "ep1-lost-soul-blade" :episode 1 :number 107
  :names ("Lost SOUL BLADE") :start (:warp-in) :end (:register 18))
 (:slug "ep1-lost-hell-pallasch" :episode 1 :number 120
  :names ("Lost HELL PALLASCH") :start (:warp-in) :end (:register 110))
 (:slug "ep1-forsaken-friends" :episode 1 :number 907
  :names ("Forsaken Friends") :start (:warp-in) :end (:register 99))
 (:slug "ep1-subterranean-patrol-1" :episode 1 :number 1960
  :names ("Subterranean Patrol #1") :start (:register 1) :end (:register 254))
 ;; ------------------------------------------------ Episode 1 - VR
 (:slug "ep1-towards-the-future" :episode 1 :number 118
  :names ("Towards the Future") :start (:register 12) :end (:register 254))
 (:slug "ep1-tyrell-s-ego" :episode 1 :number 161
  :names ("Tyrell's Ego") :start (:register 4) :end (:register 101))
 (:slug "ep1-endless-episode-1" :episode 1 :number 1850
  :names ("Endless: Episode 1") :start (:register 50) :end (:register 248))
 ;; ------------------------------------------------ Episode 1 - Event
 (:slug "ep1-christmas-fiasco" :episode 1 :number 900
  :names ("Christmas Fiasco" "Christmas Fiasco Episode 1")
  :start (:floor-switch 4 100) :end (:floor-switch 10 3))
 (:slug "ep1-december-disaster-1" :episode 1 :number 1786
  :names ("December Disaster #1") :start (:floor-switch 2 1) :end (:register 241))
 (:slug "ep1-august-atrocity-1" :episode 1 :number 960
  :names ("August Atrocity #1") :start (:floor-switch 7 1) :end (:register 50))
 (:slug "ep1-hollow-battlefield-forest" :episode 1 :number 1666
  :names ("Hollow Battlefield: Forest") :start (:warp-in) :end (:register 0))
 (:slug "ep1-hollow-battlefield-cave" :episode 1 :number 1667
  :names ("Hollow Battlefield: Cave") :start (:warp-in) :end (:register 0))
 (:slug "ep1-hollow-battlefield-mine" :episode 1 :number 1668
  :names ("Hollow Battlefield: Mine") :start (:warp-in) :end (:register 0))
 (:slug "ep1-hollow-battlefield-ruins" :episode 1 :number 1669
  :names ("Hollow Battlefield: Ruins") :start (:warp-in) :end (:register 0))
 ;; ------------------------------------------------ Episode 2 - Extermination
 (:slug "ep2-phantasmal-world-1" :episode 2 :number 233
  :names ("Phantasmal World #1") :start (:warp-in) :end (:register 254))
 (:slug "ep2-phantasmal-world-2" :episode 2 :number 234
  :names ("Phantasmal World #2") :start (:warp-in) :end (:register 111))
 (:slug "ep2-phantasmal-world-3" :episode 2 :number 235
  :names ("Phantasmal World #3") :start (:warp-in) :end (:floor-switch 11 180))
 (:slug "ep2-phantasmal-world-4" :episode 2 :number 236
  :names ("Phantasmal World #4") :start (:warp-in) :end (:floor-switch 16 120))
 (:slug "ep2-sweep-up-operation-5" :episode 2 :number 1765
  :names ("Sweep-up Operation #5") :start (:register 210) :end (:register 254))
 (:slug "ep2-sweep-up-operation-6" :episode 2 :number 1766
  :names ("Sweep-up Operation #6") :start (:register 210) :end (:register 254))
 (:slug "ep2-sweep-up-operation-7" :episode 2 :number 1767
  :names ("Sweep-up Operation #7") :start (:register 210) :end (:register 254))
 (:slug "ep2-sweep-up-operation-8" :episode 2 :number 1768
  :names ("Sweep-up Operation #8") :start (:register 210) :end (:register 254))
 (:slug "ep2-sweep-up-operation-9" :episode 2 :number 1769
  :names ("Sweep-up Operation #9") :start (:register 210) :end (:register 254))
 (:slug "ep2-penumbral-surge-1" :episode 2 :number 1821
  :names ("Penumbral Surge #1") :start (:register 50) :end (:register 254))
 (:slug "ep2-penumbral-surge-2" :episode 2 :number 1822
  :names ("Penumbral Surge #2") :start (:register 15) :end (:register 254))
 (:slug "ep2-penumbral-surge-3" :episode 2 :number 1823
  :names ("Penumbral Surge #3") :start (:register 90) :end (:register 254))
 (:slug "ep2-penumbral-surge-4" :episode 2 :number 1824
  :names ("Penumbral Surge #4") :start (:register 51) :end (:register 254))
 (:slug "ep2-penumbral-surge-5" :episode 2 :number 1825
  :names ("Penumbral Surge #5") :start (:register 15) :end (:register 254))
 (:slug "ep2-penumbral-surge-6" :episode 2 :number 1826
  :names ("Penumbral Surge #6") :start (:register 15) :end (:register 254))
 ;; psostats files Anomalous Ordeal #3-#5 under episode 1; the quest number
 ;; match makes the episode field irrelevant for detection.
 (:slug "ep2-anomalous-ordeal-3" :episode 2 :number 1812
  :names ("Anomalous Ordeal #3") :start (:register 82) :end (:register 254))
 (:slug "ep2-anomalous-ordeal-4" :episode 2 :number 1813
  :names ("Anomalous Ordeal #4") :start (:register 82) :end (:register 254))
 (:slug "ep2-anomalous-ordeal-5" :episode 2 :number 1814
  :names ("Anomalous Ordeal #5") :start (:register 82) :end (:register 254))
 (:slug "ep2-gal-da-val-s-darkness" :episode 2 :number 1309
  :names ("Gal Da Val's Darkness") :start (:floor-switch 3 20) :end (:register 89))
 (:slug "ep2-cal-s-clock-challenge" :episode 2 :number 1700
  :names ("CAL's Clock Challenge") :start (:floor-switch 1 40) :end (:register 254))
 ;; ------------------------------------------------ Episode 2 - Maximum Attack
 (:slug "ep2-maximum-attack-2-ver2" :episode 2 :number 238
  :names ("MAXIMUM ATTACK 2 Ver2") :start (:floor-switch 2 12) :end (:register 123))
 (:slug "ep2-maximum-attack-4th-stage-2a" :episode 2 :number 241
  :names ("Maximum Attack 4 -2A-" "Maximum Attack 4th Stage -2A-")
  :start (:floor-switch 5 99) :end (:floor-switch 11 29))
 (:slug "ep2-maximum-attack-4th-stage-2b" :episode 2 :number 242
  :names ("Maximum Attack 4 -2B-" "Maximum Attack 4th Stage -2B-")
  :start (:floor-switch 5 99) :end (:floor-switch 11 29))
 (:slug "ep2-maximum-attack-4th-stage-2c" :episode 2 :number 243
  :names ("Maximum Attack 4 -2C-" "Maximum Attack 4th Stage -2C-")
  :start (:floor-switch 5 99) :end (:register 254))
 (:slug "ep2-maximum-attack-e-vr" :episode 2 :number 943
  :names ("Maximum Attack E: VR") :start (:floor-switch 1 0) :end (:register 254))
 (:slug "ep2-maximum-attack-e-gal-da-val" :episode 2 :number 944
  :names ("Maximum Attack E: GDV" "Maximum Attack E: Gal Da Val")
  :start (:floor-switch 5 0) :end (:register 254))
 (:slug "ep2-random-attack-xrd-rev-2" :episode 2 :number 1802
  :names ("Random Attack Xrd REV 2") :start (:warp-in) :end (:register 254))
 ;; ------------------------------------------------ Episode 2 - Retrieval
 (:slug "ep2-lost-shock-rifle" :episode 2 :number 1780
  :names ("Lost SHOCK RIFLE") :start (:warp-in) :end (:register 15))
 (:slug "ep2-lost-bind-assault" :episode 2 :number 1781
  :names ("Lost BIND ASSAULT") :start (:warp-in) :end (:register 15))
 (:slug "ep2-lost-fill-cannon" :episode 2 :number 1782
  :names ("Lost FILL CANNON") :start (:warp-in) :end (:register 15))
 (:slug "ep2-lost-demon-s-railgun" :episode 2 :number 1783
  :names ("Lost DEMON'S RAILGUN") :start (:warp-in) :end (:register 15))
 (:slug "ep2-lost-charge-vulcan" :episode 2 :number 1784
  :names ("Lost CHARGE VULCAN") :start (:warp-in) :end (:register 15))
 ;; ------------------------------------------------ Episode 2 - VR / Tower
 (:slug "ep2-respective-tomorrow" :episode 2 :number 231
  :names ("Respective Tomorrow") :start (:register 84) :end (:register 98))
 (:slug "ep2-endless-episode-2" :episode 2 :number 1851
  :names ("Endless: Episode 2") :start (:register 50) :end (:register 248))
 (:slug "ep2-the-military-strikes-back" :episode 2 :number 1319
  :names ("The Military Strikes Back") :start (:warp-in) :end (:register 121))
 (:slug "ep2-twilight-sanctuary" :episode 2 :number 1820
  :names ("Twilight Sanctuary") :start (:register 50) :end (:register 254))
 ;; ------------------------------------------------ Episode 2 - Event
 (:slug "ep2-christmas-fiasco" :episode 2 :number 901
  :names ("Christmas Fiasco" "Christmas Fiasco Episode 2")
  :start (:floor-switch 4 100) :end (:floor-switch 11 3))
 (:slug "ep2-december-disaster-2" :episode 2 :number 1787
  :names ("December Disaster #2") :start (:floor-switch 7 200) :end (:register 241))
 (:slug "ep2-august-atrocity-2" :episode 2 :number 961
  :names ("August Atrocity #2") :start (:floor-switch 6 1) :end (:register 50))
 (:slug "ep2-hollow-reality-temple" :episode 2 :number 1670
  :names ("Hollow Reality: Temple") :start (:warp-in) :end (:register 0))
 (:slug "ep2-hollow-reality-spaceship" :episode 2 :number 1671
  :names ("Hollow Reality: Spaceship") :start (:warp-in) :end (:register 0))
 (:slug "ep2-hollow-phantasm-jungle" :episode 2 :number 1672
  :names ("Hollow Phantasm: Jungle") :start (:warp-in) :end (:register 0))
 (:slug "ep2-hollow-phantasm-seabed" :episode 2 :number 1673
  :names ("Hollow Phantasm: Seabed") :start (:warp-in) :end (:register 0))
 (:slug "ep2-hollow-phantasm-tower" :episode 2 :number 1674
  :names ("Hollow Phantasm: Tower") :start (:warp-in) :end (:register 0))
 ;; ------------------------------------------------ Episode 4 - Extermination
 (:slug "ep4-point-of-disaster" :episode 4
  :names ("Point of Disaster") :start (:warp-in) :end (:register 233))
 (:slug "ep4-war-of-limits-1" :episode 4 :number 811
  :names ("War of Limits 1") :start (:warp-in) :end (:register 254))
 (:slug "ep4-war-of-limits-2" :episode 4 :number 812
  :names ("War of Limits 2") :start (:warp-in) :end (:register 254))
 (:slug "ep4-war-of-limits-3" :episode 4 :number 813
  :names ("War of Limits 3") :start (:warp-in) :end (:register 254))
 (:slug "ep4-war-of-limits-4" :episode 4 :number 814
  :names ("War of Limits 4") :start (:warp-in) :end (:register 254))
 (:slug "ep4-war-of-limits-5" :episode 4 :number 815
  :names ("War of Limits 5") :start (:warp-in) :end (:register 254))
 (:slug "ep4-new-mop-up-operation-1" :episode 4 :number 816
  :names ("New Mop-Up Operation #1") :start (:register 205) :end (:register 157))
 (:slug "ep4-new-mop-up-operation-2" :episode 4 :number 817
  :names ("New Mop-Up Operation #2") :start (:register 86) :end (:register 43))
 (:slug "ep4-new-mop-up-operation-3" :episode 4 :number 818
  :names ("New Mop-Up Operation #3") :start (:register 205) :end (:register 254))
 (:slug "ep4-new-mop-up-operation-4" :episode 4 :number 819
  :names ("New Mop-Up Operation #4") :start (:register 110) :end (:register 43))
 (:slug "ep4-new-mop-up-operation-5" :episode 4 :number 820
  :names ("New Mop-Up Operation #5") :start (:register 86) :end (:register 43))
 (:slug "ep4-sweep-up-operation-10" :episode 4 :number 1770
  :names ("Sweep-up Operation #10") :start (:register 210) :end (:register 254))
 (:slug "ep4-sweep-up-operation-11" :episode 4 :number 1771
  :names ("Sweep-up Operation #11") :start (:register 210) :end (:register 254))
 (:slug "ep4-sweep-up-operation-12" :episode 4 :number 1772
  :names ("Sweep-up Operation #12") :start (:register 210) :end (:register 254))
 (:slug "ep4-sweep-up-operation-13" :episode 4 :number 1773
  :names ("Sweep-up Operation #13") :start (:register 210) :end (:register 254))
 (:slug "ep4-sweep-up-operation-14" :episode 4 :number 1774
  :names ("Sweep-up Operation #14") :start (:register 210) :end (:register 254))
 ;; ------------------------------------------------ Episode 4 - Retrieval
 (:slug "ep4-lost-berserk-baton" :episode 4 :number 1790
  :names ("Lost BERSERK BATON") :start (:warp-in) :end (:register 15))
 (:slug "ep4-lost-spirit-striker" :episode 4 :number 1791
  :names ("Lost SPIRIT STRIKER") :start (:warp-in) :end (:register 15))
 ;; ------------------------------------------------ Episode 4 - Maximum Attack
 (:slug "ep4-maximum-attack-4th-stage-4a" :episode 4 :number 303
  :names ("Maximum Attack 4 -4A-" "Maximum Attack 4th Stage -4A-")
  :start (:floor-switch 5 66) :end (:floor-switch 8 50))
 (:slug "ep4-maximum-attack-4th-stage-4b" :episode 4 :number 304
  :names ("Maximum Attack 4 -4B-" "Maximum Attack 4th Stage -4B-"
          "Maximum Attack 4th Stage -B-")
  :start (:floor-switch 5 66) :end (:floor-switch 8 20))
 (:slug "ep4-maximum-attack-4th-stage-4c" :episode 4 :number 305
  :names ("Maximum Attack 4 -4C-" "Maximum Attack 4th Stage -4C-")
  :start (:floor-switch 5 66) :end (:floor-switch 8 192))
 (:slug "ep4-maximum-attack-e-episode-4" :episode 4 :number 945
  :names ("Maximum Attack E: Episode 4")
  :start (:floor-switch 2 0) :end (:register 254))
 (:slug "ep4-random-attack-xrd-rev-4" :episode 4 :number 1803
  :names ("Random Attack Xrd REV 4") :start (:warp-in) :end (:register 254))
 ;; ------------------------------------------------ Episode 4 - VR
 (:slug "ep4-beyond-the-horizon" :episode 4 :number 313
  :names ("Beyond the Horizon") :start (:floor-switch 1 20) :end (:floor-switch 8 80))
 ;; ------------------------------------------------ Episode 4 - Event
 (:slug "ep4-christmas-fiasco" :episode 4 :number 902
  :names ("Christmas Fiasco" "Christmas Fiasco Episode 4")
  :start (:floor-switch 1 100) :end (:floor-switch 8 3))
 (:slug "ep4-hollow-wasteland-wilderness" :episode 4 :number 1675
  :names ("Hollow Wasteland: Wilderness") :start (:warp-in) :end (:register 0))
 (:slug "ep4-hollow-wasteland-desert" :episode 4 :number 1676
  :names ("Hollow Wasteland: Desert") :start (:warp-in) :end (:register 0)))
