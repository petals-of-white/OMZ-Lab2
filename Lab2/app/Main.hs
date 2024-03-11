
module Main where
import           Control.Monad.Except
import           Control.Monad.Exception     as MonadException
import           Data.Binary                 as Binary (decode, encode)
import           Data.ByteString.Lazy        as LBS (append, fromStrict)
import           Data.DICOM                  as Dicom
import           Data.DICOM.Utilities        hiding (Size)
import           Data.Either
import           Data.Maybe                  (catMaybes, fromJust)
import qualified Data.Text.Lazy              as LazyText
import           Data.Word                   (Word32, Word8)
import           Graphics.GL
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW as GLFW
import           Paths_OMZ_Lab2              (getDataFileName)
import           Prelude                     hiding (reverse)
-- import           Typograffiti
-- import           Graphics.Rendering.MiniTypeset


-- import           Graphics.Rendering.OpenGL      as GL (MatrixMode (Modelview, Projection),
--                                                        Position (..),
--                                                        Size (Size),
--                                                        loadIdentity, matrixMode,
--                                                        ortho, viewport, ($=!))


-- import           Data.IORef
import           Data.List                   (delete)
import           Typograffiti





{-
setWindowCoordSystem :: IO ()
setWindowCoordSystem = do
  -- (w,h) <- readIORef theWindowSize
  -- viewport $=! (Position 0 0 , Size (fromIntegral w) (fromIntegral h))
  matrixMode $=! Projection
  loadIdentity
  GL.ortho 0 (fromIntegral 256) (fromIntegral 256) 0 (-1) (1::Double)
  matrixMode $=! Modelview 0
  loadIdentity

-}

data ShaderEnvironment a = ShaderEnvironment
  {
    primitives :: PrimitiveArray Triangles a,
    colorImage :: Image (Format RWord)
  }


data Renderings ctx os m c = Renderings {
  renderOriginal    :: Render os (),
  renderWindowLevel :: Float -> Float -> Render os (),
  renderBinarized   :: Float -> Render os (),
  renderTarget :: Render os (),
  renderInfo        :: ImageInfo c -> ExceptT TypograffitiError (ContextT ctx os m) (),
  getImageInfo      :: ContextT ctx os m (ImageInfo c)
  }

data ImageInfo a = ImageInfo {minValue :: a, maxValue :: a}

data ImageMode = Original | Binarized Float | WindowLevel Float Float deriving Eq

type ContextExcept ctx m a = forall os. ExceptT TypograffitiError (ContextT ctx os m) a

main :: IO ()
main = do
    filename <- getDataFileName "DICOM_Image_8b.dcm"
    dicom <- either error id <$> readObjectFromFile filename
    let ((rows, columns), imgBytes, intercept, slope, bitsAlloc) = fromJust $ getDicomData dicom
        size = rows * columns
        imgWord8 :: [Word8] = (decode . LBS.append (encode size) . fromStrict) imgBytes
    putStrLn $
      "Rows: " ++ show rows ++ ". Columns: " ++ show columns ++ ". Intercept: "
      ++ show intercept ++ ". Slope: " ++ show slope ++ ". Bits allocated: " ++ show bitsAlloc


    case (intercept, slope, bitsAlloc) of
      (i, s, _) | i /= 0, s /= 0 -> putStrLn "gl float"
      (_,_, 8)                   -> putStrLn "gl byte"
      (_,_, 16)                  -> putStrLn "gl short"
      _                          -> error "Unrecognized type?"

    let ttfName = "C:\\Users\\maxle\\Обробка_медичних_зображень\\Lab2\\assets\\Lora-Regular.ttf"


    {-
    multifont <- newMultiFont (UserFontConfig id (\ _ _ -> ttfName) id 1)
    let doc = WithColor (Col 0.3 0.3 0.3) $ String "Hello kekasdjfkl;adsjf;klasjdlfads"
    layout :: Layout Int BasicStyle <- createLayout multifont (Height 20) doc
    let rText = renderLayout layout (Pos 10 10)
    -}

    let myText = LazyText.pack "Мінімальна яскравість: 0123456789\nМаксимальна яскравість: 0123456789"


    void $
      runContextT GLFW.defaultHandleConfig (runExceptT $ do
            lift $ do
                vertexBuffer1 :: Buffer os (B2 Float, B2 Float) <- newBuffer 4
                vertexBuffer2 :: Buffer os (B2 Float, B2 Float) <- newBuffer 4


                writeBuffer vertexBuffer1 0 [(V2 (-1) (-1), V2 0 1),  (V2 1 (-1), V2 1 1),
                                          (V2 (-1) 1, V2 0 0),     (V2 1 1, V2 1 0)]

                writeBuffer vertexBuffer2 0 [(V2 (-1) (-1), V2 0 0),  (V2 1 (-1), V2 1 0),
                                          (V2 (-1) 1, V2 0 1),     (V2 1 1, V2 1 1)]

                -- Textures
                let texSize = V2 rows columns
                originalTex <- newTexture2D R8UI texSize 1
                outputTex <- newTexture2D R8UI texSize 1

                writeTexture2D originalTex 0 0 texSize imgWord8
                writeTexture2D outputTex 0 0 texSize imgWord8
                win <- newWindow (WindowFormatColor RGB8) $ (GLFW.defaultWindowConfig "Lab2") {configWidth=rows, configHeight=columns}

                funcRes <- liftIO $ makeDrawText' ttfName 0 (PixelSize 13 13) (defaultSample { sampleText = myText })

                (Right rText) <- runExceptT $ do

                      case funcRes of
                        Right drawText ->
                          let wow (ImageInfo minV maxV)  =  do
                          -- return $ \(ImageInfo minV maxV) -> do
                                -- let (minV, maxV) = (1,1)
                                drawText' <-
                                  drawText $ txt $ LazyText.pack $ "Мін яскравість: " ++ show minV ++ "\nМакс яскравість: " ++ show maxV
                                liftIO $ arDraw drawText' [move 10 10, TextTransformMultiply (V4 1 1 1 1)] (V2 256 256)
                          in return wow

                        Left err -> liftIO (print err) >> throwError err


                -- Shaders
                let retriveImgInfo = do
                      readTexture2D outputTex 0 0 texSize (\(ImageInfo minV maxV) (pixel:: Word8) ->
                        return ImageInfo {minValue=min pixel minV, maxValue = max pixel maxV}) (ImageInfo {maxValue=minBound, minValue=maxBound})

                shaderWord8TexToWin <- compileShader $ do
                  primitiveStream  <- toPrimitiveStream fst

                  let primitiveStream2 = fmap (\(V2 x y, uv) -> (V4 x y 0 1, uv)) primitiveStream
                  fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 rows columns), DepthRange 0 1)) primitiveStream2

                  let edge = (pure Repeat, undefined)
                  samp <- newSampler2D (\(_,targetTex) -> (targetTex, SamplerNearest, edge))
                  let sampleTexture = pure . sample2D samp SampleAuto Nothing Nothing
                      -- перетворимо word8 на float
                      fragmentStream2 = fmap (((/ 255) . fmap toFloat) . sampleTexture) fragmentStream

                  drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream2


                {-
                render $ do
                    rectangleVertArray <- newVertexArray vertexBufferMask
                    cImage <- getTexture2DImage logicTex 0
                    shaderMask $ ShaderEnvironment
                      (toPrimitiveArray TriangleList rectangleVertArray)
                      cImage
                -}
                -- liftIO setWindowCoordSystem

                renderLoop win Renderings {
                  renderOriginal = do
                    clearWindowColor win 0
                    vertexArray <- newVertexArray vertexBuffer1
                    shaderWord8TexToWin (toPrimitiveArray TriangleStrip vertexArray, originalTex)
                    ,
                    renderBinarized = undefined,
                    renderWindowLevel = undefined,
                    renderInfo = rText,
                    getImageInfo = retriveImgInfo,
                    renderTarget = do
                      vertexArray <- newVertexArray vertexBuffer1
                      shaderWord8TexToWin (toPrimitiveArray TriangleStrip vertexArray, outputTex)
                } Original ImageInfo {minValue=0, maxValue=255} True
              )


renderLoop :: (MonadIO m,  MonadException m, Num c) => Window os RGBFloat ds -> Renderings GLFW.Handle os m c ->
  ImageMode -> ImageInfo c -> Bool -> ContextT GLFW.Handle os m ()
renderLoop win renderings appMode imgInfo shouldUpdate = do

  bin <- getKey win Key'B
  windowLevel <- getKey win Key'W
  orig <- getKey win Key'O

  newInfo <-
    if shouldUpdate then do
      case appMode of
          Original ->
            render (renderOriginal renderings)
          Binarized porih ->
            render $ renderBinarized renderings porih
          WindowLevel l w ->
            render $ renderWindowLevel renderings l w
      getImageInfo renderings
    else return imgInfo


  render $ renderTarget renderings
  
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  (Right ()) <-  runExceptT $ renderInfo renderings imgInfo

  let activeKeys = catMaybes $ zipWith (\keyStatus mode ->
            case keyStatus of
              Just KeyState'Pressed -> Just mode
              _                     -> Nothing
            ) [orig, windowLevel, bin] [Original, WindowLevel 0.2 0.2, Binarized 0.3]

      newMode = case activeKeys of
        []    -> appMode
        new:_ -> new


  swapWindowBuffers win
  closeRequested <- GLFW.windowShouldClose win

  unless (closeRequested == Just True) $
    renderLoop win renderings newMode newInfo (appMode /= newMode)

