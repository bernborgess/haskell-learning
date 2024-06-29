import Data.ByteString.Builder qualified as BL
import Data.ByteString.Lazy qualified as B
import Data.Foldable
import System.Process
import Text.Printf

type Seconds = Float
type Samples = Float
type Hz = Float
type Pulse = Float
type Semitones = Float
type Beats = Float

outputFilePath :: FilePath
outputFilePath = "sandstorm.wav"

volume :: Float
volume = 0.5

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0

f :: Semitones -> Hz
f n = pitchStandard * a ** n
 where
  a = 2 ** (1.0 / 12.0)

bpm :: Beats
bpm = 120.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

banger :: Semitones -> Int -> [Pulse]
banger st rp = concat (replicate rp sm) ++ note st 0.5
 where
  sm = note st 0.25

note :: Semitones -> Beats -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = map (* volume) $ zipWith3 (\x y z -> x * y * z) attack output release
 where
  step = (hz * 2 * pi) / sampleRate

  attack :: [Pulse]
  attack = map (min 1.0) [0.0, 0.001 ..]

  release :: [Pulse]
  release = reverse $ take (length output) attack

  output :: [Pulse]
  output = map (sin . (* step)) [0.0 .. sampleRate * duration]

wave :: [Pulse]
wave =
  concatMap
    (uncurry banger)
    [ (0, 4)
    , (0, 6)
    , (5, 6)
    , (3, 6)
    , (-2, 0)
    , (0, 4)
    , (5, 0)
    , (0, 0)
    , (5, 0)
    , (0, 0)
    , (-2, 0)
    , (0, 4)
    ]

save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ BL.toLazyByteString $ foldMap BL.floatLE wave

play :: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()