{-# LANGUAGE MonoLocalBinds #-}
module Main where
import           AppState
import           Control.Monad.Except
import           Control.Monad.Exception     as MonadException
import           Data.Binary                 as Binary (decode, encode)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS (append, fromStrict)
import           Data.DICOM                  as Dicom
import           Data.DICOM.Utilities
import           Data.Either
import qualified Data.List                   as List
import           Data.Maybe                  (catMaybes)
import qualified Data.Text.Lazy              as LazyText
import           Data.Word                   (Word16, Word32, Word8)
import           Graphics.GL
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL   as GL
import           Graphics.Shaders as Shaders
import           Prelude                     hiding (reverse)
import           System.Environment          (getArgs)
import           Typograffiti
import Graphics.Render

data GraphicsIO ctx os m c = GraphicsIO {
  renderOriginal    :: Render os (),
  renderWindowLevel :: Render os (),
  renderBinarized   :: Render os (),
  renderTarget       :: Render os (),
  renderInfo        :: ImageInfo c -> ExceptT TypograffitiError (ContextT ctx os m) (),
  getImageInfo      :: ContextT ctx os m (ImageInfo c),
  convertTex :: Render os (),
  floatTex :: Texture2D os (Format RFloat)
  }

data Uniforms os = Uniforms {
  binaryUni      :: (Buffer os (Uniform (B Word32)), Int),
  windowLevelUni :: (Buffer os (Uniform (B2 Word32)), Int)}



type ContextExcept ctx m a = forall os. ExceptT TypograffitiError (ContextT ctx os m) a

vertices1, vertices2 :: [(V2 Float, V2 Float)]

vertices1 =  [(V2 (-1) (-1), V2 0 1),  (V2 1 (-1), V2 1 1),
              (V2 (-1) 1, V2 0 0),     (V2 1 1, V2 1 0)]

vertices2 = [(V2 (-1) (-1), V2 0 0),  (V2 1 (-1), V2 1 0),
             (V2 (-1) 1, V2 0 1),     (V2 1 1, V2 1 1)]

main :: IO ()
main = do

    dicomPath:other <- getArgs
    let [_l,_w,_b] = fmap read other
    let settings = Settings {l=_l, w=_w, b=_b}
    dicom <- either error id <$> readObjectFromFile dicomPath
    putStrLn "successfull read"
    let elemMap = toMap dicom
    print elemMap
    let metadata = do
          r <- rows elemMap
          c <- columns elemMap
          intercept <- rescaleIntercept elemMap
          slope <- rescaleSlope elemMap
          bitsAlloc <- bitsAllocated elemMap
          imgBytes <- pixelData elemMap
          return (r,c, imgBytes, intercept, slope, bitsAlloc)

    case metadata of
      Right (row, col, imgBytes, intercept, slope, bitsAlloc) -> do
        putStrLn $ "Rows: " ++ show row ++ ". Columns: " ++ show col ++ ". Intercept: " ++
         show intercept ++ ". Slope: " ++ show slope ++ ". Bits allocated: " ++ show bitsAlloc

        case (intercept, slope, bitsAlloc) of
          (i, s, _) | i /= 0, s /= 0 -> do

            putStrLn "gl float"
          (_,_, 8)                   -> do
            putStrLn "gl byte"
            bigThing R8UI (V2 (fromIntegral row) (fromIntegral col)) imgBytes settings
          (_,_, 16)                  -> do
            putStrLn "gl short"
            bigThing R16UI (V2 (fromIntegral row) (fromIntegral col)) imgBytes settings
          _                          ->
            error "Unrecognized pixel TYPE!!!"

      Left _ -> error "Error reading metadata!"


bigThing :: Format RWord -> Size2 -> BS.ByteString -> Settings -> IO ()
bigThing format (V2 row col) imgBytes settings = do
    let size = row * col

        ttfName = "C:\\Users\\maxle\\Обробка_медичних_зображень\\Lab2\\assets\\Lora-Regular.ttf"

        myText = LazyText.pack "Мінімальна яскравість: 0123456789\nМаксимальна яскравість: 0123456789"

        imgWord = (decode . LBS.append (encode size) . LBS.fromStrict) imgBytes :: [Word8]


    print imgWord

    void $
      runContextT GLFW.defaultHandleConfig (runExceptT $ do
            lift $ do
                vertexBuffer1 :: Buffer os (B2 Float, B2 Float) <- newBuffer 4
                vertexBuffer2 :: Buffer os (B2 Float, B2 Float) <- newBuffer 4
                uniformFloatBuffer :: Buffer os (Uniform (B Word32)) <- newBuffer 1
                uniform2FloatBuffer :: Buffer os (Uniform (B2 Word32)) <- newBuffer 1

                let binarizeUni = (uniformFloatBuffer, 0)
                    windowLevelUni = (uniform2FloatBuffer, 0)

                writeBuffer vertexBuffer1 0 vertices1
                writeBuffer vertexBuffer2 0 vertices2

                -- Textures
                let texSize = V2 row col
                originalTex <- newTexture2D format texSize 1
                outputTex <- newTexture2D format texSize 1
                floatTex <- newTexture2D R8 texSize 1

                writeTexture2D originalTex 0 0 texSize imgWord
                writeTexture2D outputTex 0 0 texSize imgWord
                writeTexture2D floatTex 0 0 texSize (repeat 0.3 :: [Float])

                wow :: [Word8] <- List.reverse <$> readTexture2D originalTex 0 0 texSize (\list pixel -> return $ pixel:list) []

                -- liftIO $
                  -- if imgWord == wow then putStrLn "Texture is the same!"  else putStrLn "Wa wa wa..."

                win <- newWindow (WindowFormatColor RGB16) $ (GLFW.defaultWindowConfig "Lab2") {configWidth=row, configHeight=col}

                (Right rText) <- runExceptT imageInfo 


                -- Shaders
                let retriveImgInfo = readTexture2D outputTex 0 0 texSize
                      (\(ImageInfo minV maxV) (pixel :: Word16) -> return
                          ImageInfo {minValue=min pixel minV, maxValue = max pixel maxV}) (ImageInfo {maxValue=minBound, minValue=maxBound})

                shaderToFloat <- compileShader $ do
                    floatStream <- (Shaders.texToFloat :: S F Float -> Shader os (ShaderEnvironment os Triangles RWord RFloat (B2 Float, B2 Float)) (FragmentStream (S F Float))) (realToFrac (1/65535))
                    draw (const NoBlending) floatStream
                      (drawColor (\ShaderEnvironment{colorImage=img, clrMask=mask} -> (img, mask, False)))


                let blend = NoBlending


                shaderOriginal <- compileShader Shaders.copyTexture
                liftIO $ putStrLn "Original shader compiled"
                shaderBinarize <- compileShader Shaders.binarize
                liftIO $ putStrLn "Binary shader compiled"
                shaderWindowLevel <- compileShader Shaders.windowLevel
                liftIO $ putStrLn "Window Level shader compiled"
                shaderRenderToWindow <- compileShader $ Shaders.texToWindow (realToFrac (1/255))
                liftIO $ putStrLn "Texture-Window shader compiled"



                let rendOrig = do
                      img <- getTexture2DImage outputTex 0
                      vertexArray <- newVertexArray vertexBuffer2
                      let shaderEnv = ShaderEnvironment {
                        primitives = toPrimitiveArray TriangleStrip vertexArray,
                        colorImage=img,
                        clrMask = True,
                        tex2D = originalTex,
                        window = win,
                        blending=blend
                        }
                      shaderOriginal shaderEnv


                    rendBin = do
                      vertexArray <- newVertexArray vertexBuffer2
                      target <- getTexture2DImage outputTex 0
                      let shaderEnv = ShaderEnvironment {
                            primitives = toPrimitiveArray TriangleStrip vertexArray,
                            colorImage=target,
                            clrMask = True,
                            tex2D = originalTex,
                            window = win,
                            blending=blend
                            }
                      shaderBinarize (shaderEnv, binarizeUni)



                    rendWinLevel = do
                      vertexArray <- newVertexArray vertexBuffer2
                      target <- getTexture2DImage outputTex 0
                      let shaderEnv = ShaderEnvironment {
                            primitives = toPrimitiveArray TriangleStrip vertexArray,
                            colorImage=target,
                            clrMask = True,
                            tex2D = originalTex,
                            window = win,
                            blending=blend
                            }

                      shaderWindowLevel (shaderEnv, windowLevelUni)


                    rendTarget = do
                        vertexArray <- newVertexArray vertexBuffer2
                        let shaderEnv = ShaderEnvironment {
                            primitives = toPrimitiveArray TriangleStrip vertexArray,
                            colorImage=undefined,
                            clrMask = True,
                            tex2D = outputTex,
                            window = win,
                            blending=blend
                            }

                        shaderRenderToWindow shaderEnv




                renderLoop win
                  GraphicsIO {
                    floatTex = floatTex,
                    renderOriginal = rendOrig,
                    renderBinarized = rendBin,
                    renderWindowLevel = rendWinLevel,
                    renderInfo = rText,
                    getImageInfo = retriveImgInfo,
                    renderTarget = rendTarget,
                    convertTex = do
                          target <- getTexture2DImage floatTex 0
                          vertexArray <- newVertexArray vertexBuffer1
                          let shaderEnv = ShaderEnvironment {
                            primitives = toPrimitiveArray TriangleStrip vertexArray,
                            colorImage=target,
                            clrMask = True,
                            tex2D = originalTex,
                            window = win,
                            blending=blend
                            }
                          shaderToFloat shaderEnv
                  }
                  Uniforms {binaryUni = (uniformFloatBuffer, 0), windowLevelUni = (uniform2FloatBuffer, 0)}
                  settings
                  Original ImageInfo {minValue=0, maxValue=255} True
              )



renderLoop :: (MonadIO m,  MonadException m, MonadAsyncException m, Num c) => Window os RGBFloat ds ->
  GraphicsIO GLFW.Handle os m c -> Uniforms os -> Settings ->
  ImageMode -> ImageInfo c -> Bool -> ContextT GLFW.Handle os m ()
renderLoop win renderings uniforms settings@Settings {w,l,b} appMode imgInfo shouldUpdate = do

  bin <- getKey win Key'B
  winlevel <- getKey win Key'W
  orig <- getKey win Key'O
  render $ clearWindowColor win 0

  newInfo <-
    if shouldUpdate then do
      let writeUniform loc value = uncurry writeBuffer loc [value]
      case appMode of
          Original ->
            liftIO (putStrLn "Rendering orig") >>
              render (renderOriginal renderings)
          Binarized porih -> do
            liftIO (putStrLn "Rendering binarized")
            writeUniform (binaryUni uniforms) porih
            render $ renderBinarized renderings
          WindowLevel l w -> do
            liftIO (putStrLn "Rendering WindowLevel")
            writeUniform (windowLevelUni uniforms) (V2 l w)
            render $ renderWindowLevel renderings

      getImageInfo renderings
    else return imgInfo


  render $ renderTarget renderings

  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  (Right ()) <-  runExceptT $ renderInfo renderings imgInfo

  let toWord = round . realToFrac . (*) (fromIntegral (maxBound :: Word16))
  let lWord = toWord l
      wWord = toWord w
      bWord = toWord b

  liftIO $ print (lWord, wWord, bWord)
  let activeKeys = catMaybes $ zipWith (\keyStatus mode ->
            case keyStatus of
              Just KeyState'Pressed -> Just mode
              _                     -> Nothing
            ) [orig, winlevel, bin] [Original, WindowLevel lWord wWord, Binarized bWord]

      newMode = case activeKeys of
        []    -> appMode
        new:_ -> new


  swapWindowBuffers win
  closeRequested <- GLFW.windowShouldClose win

  unless (closeRequested == Just True) $
    renderLoop win renderings uniforms settings newMode newInfo (appMode /= newMode)

