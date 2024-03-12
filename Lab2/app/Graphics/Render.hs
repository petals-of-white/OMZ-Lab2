module Graphics.Render where
import           AppState
import           Control.Monad.Except
import           Data.Text.Lazy       as LazyText
import           Graphics.GPipe
import           Typograffiti


type ContextExcept ctx m a = forall os. ExceptT TypograffitiError (ContextT ctx os m) a


imageInfo :: (MonadIO m, MonadFail m, Show a, MonadError TypograffitiError m) => 
    ExceptT TypograffitiError m (ImageInfo a -> ExceptT TypograffitiError m ())

imageInfo = do
    funcRes <- liftIO $ makeDrawText' fontPath 0 (PixelSize 13 13) (defaultSample { sampleText = textSample })

    case funcRes of
        Right drawText ->
            let drawThing (ImageInfo minV maxV)  = do
                    drawText' <- drawText $ txt $ LazyText.pack $ "Мін яскравість: " ++ show minV ++ "\nМакс яскравість: " ++ show maxV
                    liftIO $ arDraw drawText' [move 10 10, TextTransformMultiply (V4 1 1 1 1)] (V2 256 256)
            in return drawThing 

        Left err -> throwError err

fontPath :: FilePath
fontPath = "C:\\Users\\maxle\\Обробка_медичних_зображень\\Lab2\\assets\\Lora-Regular.ttf"

textSample :: Text
textSample = LazyText.pack "Мінімальна яскравість: 0.,123456789\nМаксимальна яскравість: 0.,123456789"
