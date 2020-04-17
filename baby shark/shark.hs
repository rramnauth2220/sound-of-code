import Euterpea
import Euterpea.Music
import HSoM

repd d = \ns -> line $ map (\x -> x d) ns
wns = repd wn
hns = repd hn
qns = repd qn
ens = repd en
sns = repd sn

bqns = qns -- barred quarter notes
bens = ens -- barred eighth notes
sens = sns -- barred sixteenth notes

chordd d = \ns -> chord $ map (\x -> x d) ns
cwns = chordd wn
chns = chordd hn
cqns = chordd qn
cens = chordd en
csns = chordd sn
ctns = chordd (3/4)

rep :: Int -> Music Pitch -> Music Pitch
rep 0 b = rest 0 
rep a b = b :+: rep (a - 1) b 

grace :: Int -> Rational -> Music Pitch -> Music Pitch -> Music Pitch
grace n r (Prim (Note d1 p1)) (Prim (Note d2 p2)) =
  note (d1 - r * d2) p1 :+: note (r * d2) (trans n p2) :+: note d2 p2

accidental :: [Dur -> Music Pitch] -> Music Pitch -> Music Pitch
accidental a (Prim (Note d p)) = repd (0.05 * d) a :+: note (0.95 * d) p

dot :: Int -> Music Pitch -> Music Pitch
dot n (Prim (Note d p)) = note (d * (2 - 1/(2 ^ n))) p

translate :: Int -> Music Pitch -> [Music Pitch]
translate n p = map (transpose n) (lineToList p)

reverseList :: [Music Pitch] -> [Music Pitch]
reverseList xs = foldl (\x y -> y:x) [] xs

staccato :: [Music Pitch] -> [Music Pitch]
staccato [] = []
staccato (m : ms) = f3_impl(m) : (staccato ms)
  where
    f3_impl(m') = case m' of
        Prim (Note d p) -> Prim(Note (d / 2) p) :+: Prim(Rest (d / 2))
        Prim (Rest d)   -> Prim(Rest d)
        m1 :+: m2       -> f3_impl(m1) :+: f3_impl(m2)
        m1 :=: m2       -> f3_impl(m1) :=: f3_impl(m2)
        Modify mod m1   -> Modify mod (f3_impl m1)

instr_treb = instrument AcousticGrandPiano
instr_bass = instrument AcousticGrandPiano

zipStaves :: [Music a] -> [Music a] -> Music a
zipStaves trebs basses =
  line $ zipWith (:=:)
                 (map instr_treb trebs)
                 (map instr_bass basses)
  
itm_treb_mel_start_slow = qns [d 4, e 4]
itm_treb_mel_start_slur = bens [d 4, e 4]
itm_treb_mel_mid = bens [g 4, g 4, g 4]
               :+: sens [g 4, g 4, rest, g 4] --, rest]
               :+: ens [g 4] -- extra doo?
itm_treb_mel_end = bens [g 4, g 4] :+: hns [fs 4]

itm_bass_mel_start_slow = hns [rest]
itm_bass_mel_start_slur = qns [rest]
itm_bass_mel_0 = chns [g 2, g 3]
itm_bass_mel_1 = chns [d 3, g 3, b 3]
itm_bass_mel_2 = chns [e 3, g 3, c 4]
itm_bass_mel_3 = chns [g 3, b 3, e 4]
itm_bass_mel_4 = qnr :+: chns [fs 3, a 3, d 4]

itm_mel_0 = zipStaves [itm_treb_mel_start_slow, itm_treb_mel_mid]
                      [itm_bass_mel_start_slow, itm_bass_mel_1]
itm_mel_1 = zipStaves [itm_treb_mel_start_slur, itm_treb_mel_mid]
                      [itm_bass_mel_start_slur, itm_bass_mel_2]
itm_mel_2 = zipStaves [itm_treb_mel_start_slur, itm_treb_mel_mid]
                      [itm_bass_mel_start_slur, itm_bass_mel_3]
itm_mel_3 = zipStaves [itm_treb_mel_end]
                      [itm_bass_mel_4]

shark_generic = rep 2 (itm_mel_0 :+: itm_mel_1 :+: itm_mel_2 :+: itm_mel_3)
itm_treb_10_17 = (itm_treb_mel_start_slow)
            :+: (itm_treb_mel_mid :=: (transpose (-5) itm_treb_mel_mid))
            :+: rep 2 ((itm_treb_mel_start_slur)
            :+: (itm_treb_mel_mid :=: (transpose (-3) itm_treb_mel_mid)))
            :+: (itm_treb_mel_end :=: (transpose (-3) itm_treb_mel_end))
itm_bass_10_17_daddy = chns [g 2, g 3] :+:
                 rep 4 (bens [g 2, g 3]) :+:
                 rep 4 (bens [c 3, c 4]) :+:
                 rep 4 (bens [e 3, e 4]) :+:
                 rep 2 (bens [d 3, c 3, b 2, a 2])
itm_bass_10_17_mommy_inv = line (reverseList (lineToList itm_bass_10_17_daddy))
itm_bass_18_25_daddy = chns [g 2, g 3] :+:
                 rep 4 (bens [g 2, g 3]) :+:
                 rep 4 (bens [c 3, c 4]) :+:
                 rep 4 (bens [e 3, e 4]) :+:
                 bens [d 3, c 3, b 2, a 2] :+:
                 hns [d 3]
itm_bass_18_25_mommy_inv = line (reverseList (lineToList itm_bass_18_25_daddy))
shark_daddy = zipStaves [itm_treb_10_17, itm_treb_10_17]
                        [itm_bass_10_17_daddy, itm_bass_18_25_daddy]
shark_mommy_inverted = transpose 12 (
                        zipStaves [itm_treb_10_17, itm_treb_10_17]
                                  [itm_bass_10_17_mommy_inv, itm_bass_18_25_mommy_inv])
itm_treb_10_17_mommy = transpose 12 itm_treb_10_17                       
itm_bass_10_17_mommy = chns [g 2, g 3] :+:
                       hns [g 2] :+:
                       bens [g 2, b 2, d 3, g 3] :+:
                       hns [c 3] :+:
                       bens [c 3, e 3, g 3, c 4] :+:
                       hns [e 3] :+:
                       bens [e 3, g 3, b 3, e 4] :+:
                       bens [d 3, a 3, d 4] :+: 
                       bens [d 3, c 3, b 2, a 2]

shark_mommy = zipStaves [itm_treb_10_17_mommy, itm_treb_10_17_mommy]
                        [itm_bass_10_17_mommy, itm_bass_10_17_mommy]

shark_grdma = transpose (-6) shark_mommy
shark_grdpa = transpose (-6) shark_daddy

itm_bass_shark_run = rep 2 ((chns [g 2, g 3] :+:
                     rep 4 (bens [g 2, g 3]) :+:
                     rep 4 (bens [c 3, c 4]) :+:
                     rep 4 (bens [e 3, e 4]) :+:
                     bens [d 3, c 3, b 2, a 2]))
itm_treb_shark_run = line (staccato (lineToList itm_bass_10_17_mommy))

shark_hunt_run = Modify (Tempo (145/120)) $ transpose (-6) shark_safe_end
shark_safe_end = transpose (24) shark_generic :=: itm_treb_shark_run :=:
                 zipStaves [itm_treb_10_17] [itm_bass_shark_run]

jaws :: Dur -> Int -> Music Pitch
jaws _ 0  = rest 0
jaws s t  = bqns [e 2] :+: sens [f 2] :+: rest s :+: jaws (s * 0.5) (t - 1)
jaws_speed = snr :+: rep 5 (bens [e 2, f 2, e 3, f 3] :=: bqns [e 2, f 2])
jaws_opening = (Modify (Instrument FrenchHorn) $ jaws 1 5) :+:
  (Modify (Instrument TremoloStrings) $ line (staccato (lineToList jaws_speed)))
   :+: (Modify (Instrument Marimba) $ bens [e 4]) :+: enr :+: snr
  
end_prog = bens [d 4, d 5, g 3, g 2, g 4] :+: chns [g 2, g 3, g 4, d 3, b 3]

itm = Modify (KeySig A Minor) $
      jaws_opening :+:
      shark_generic :+:
      shark_mommy :+:
      shark_daddy :+:
      shark_grdma :+:
      shark_grdpa :+:
      shark_hunt_run :+:
      shark_safe_end :+: 
      end_prog
writeMidix :: ToMusic1 a => FilePath -> Music a -> IO ()
writeMidix fn m = exportMidiFile fn $ toMidi $ perform m
