module AppState where
    
import Data.Word (Word32)
    
data ImageInfo p = ImageInfo {minValue :: p, maxValue :: p}

data ImageMode = Original | Binarized Word32 | WindowLevel Word32 Word32 deriving Eq

data Settings = Settings {l::Float, w:: Float, b::Float}