
module Main where
import           Control.Monad.Except
import           Control.Monad.Exception     as MonadException
import           Data.Binary                 as Binary (decode, encode)
import           Data.ByteString.Lazy        as LBS (append, fromStrict)
import           Data.DICOM                  as Dicom
import           Data.DICOM.Utilities        hiding (Size)
import           Data.Either
import           Data.Maybe                  (fromJust)
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
-- newtype ExceptT e m a = ExceptT (m (Either e a))
type ContextExcept ctx m a = forall os. ExceptT TypograffitiError (ContextT ctx os m) a

main :: IO ()
main =
  let logicOp = LogicOp And
      transformColor green = V3 0 green 0 in
  do
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

    let myText = LazyText.pack $ unlines [
            "Decoder Ring Theatre brings you the continuing adventures",
            "of Canada's greatest superhero, that scourge of the underworld,",
            "hunter of those who pray upon the innocent,",
            "that marvelous masked mystery man",
            "known only as The Red Panda!",
            "",
            "The Red Panda, masked crucader for justice, hides his secret identity",
            "as one of the city's wealthiest men in his neverending battle",
            "against crime & corruption. Only his trusty driver, Kit Baxter",
            "who joins him in the guise of The Flying Squirrel,",
            "knows who wears the mask of The Red Panda!"]


    void $
      runContextT GLFW.defaultHandleConfig (runExceptT $ do
            lift $ do
                vertexBuffer1 :: Buffer os (B2 Float, B2 Float) <- newBuffer 4
                vertexBuffer2 :: Buffer os (B2 Float, B2 Float) <- newBuffer 4
                vertexBufferMask :: Buffer os (B2 Float, B Word32) <- newBuffer 12


                writeBuffer vertexBuffer1 0 [(V2 (-1) (-1), V2 0 1),  (V2 1 (-1), V2 1 1),
                                          (V2 (-1) 1, V2 0 0),     (V2 1 1, V2 1 0)]

                writeBuffer vertexBuffer2 0 [(V2 (-1) (-1), V2 0 0),  (V2 1 (-1), V2 1 0),
                                          (V2 (-1) 1, V2 0 1),     (V2 1 1, V2 1 1)]

                writeBuffer vertexBufferMask 0
                  [(V2 (-1) (-1), 255), (V2 (-1) 1, 255), (V2 0 (-1), 255),
                  (V2 0 (-1),255),     (V2 0 1, 255), (V2 (-1) 1, 255),
                  (V2 0 (-1),0),       (V2 0 1,0),       (V2 1 (-1),0),
                  (V2 1 (-1),0),       (V2 1 1,0),       (V2 0 1,0)]

                -- Textures
                let texSize = V2 rows columns
                originalTex <- newTexture2D R8UI texSize 1
                logicTex <- newTexture2D R8UI texSize 1

                writeTexture2D originalTex 0 0 texSize imgWord8
                writeTexture2D logicTex 0 0 texSize imgWord8



                win <- newWindow (WindowFormatColor RGB8) ((GLFW.defaultWindowConfig "Lab2") {configWidth=rows, configHeight=columns})


                (Right rText) <- runExceptT $ do
                      funcRes <- liftIO $ makeDrawText' ttfName 0 (PixelSize 15 15) (defaultSample { sampleText = myText })
                      case funcRes of
                        Right func -> do
                          drawText' <- func (txt myText)
                          liftIO $ print "very good!"
                          return $ arDraw drawText' [TextTransformMultiply (V4 0.5 0.5 0.7 1)] (V2 300 300)
                        Left err -> liftIO (print err) >> throwError err

                liftIO $ putStrLn "Hello"


                -- we will comment out these for now


                -- Shaders
                {-
                shaderMask <- compileShader $ do
                  primitiveStream <- toPrimitiveStream primitives

                  let primitiveStream2 = fmap (\(V2 x y, c) -> (V4 x y 0 1, c)) primitiveStream
                  fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) texSize, DepthRange 0 1)) primitiveStream2

                  draw (const logicOp) fragmentStream $ \c ->
                    drawColor (\ s -> (colorImage s, True, True)) c
                -}

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
                  renderText = rText
                }
              )

renderLoop :: (MonadIO m,  MonadException m) => Window os RGBFloat ds -> Renderings os -> ContextT GLFW.Handle os m ()
renderLoop win renderings@(Renderings rOrig rText)  = do
  logic <- getKey win Key'L
  colorModelling <- getKey win Key'C

  {-
  case (logic, colorModelling) of
    (Just KeyState'Pressed, _) -> render rLogic
    (_, Just KeyState'Pressed) -> render rColor
    _                          -> render rOrig
  -}
  -- render $ clearWindowColor win 1
  -- render rOrig

  render rOrig
  winSize <- getWindowSize win
  bufSize <- getFrameBufferSize win
  liftIO $ putStrLn $ "Window size is " ++ show winSize
  liftIO $ putStrLn $ "Framebuf size is " ++ show bufSize

  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  liftIO rText
  swapWindowBuffers win
  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    renderLoop win renderings

data ShaderEnvironment a = ShaderEnvironment
  {
    primitives :: PrimitiveArray Triangles a,
    colorImage :: Image (Format RWord)
  }


data Renderings os  = Renderings {
  renderOriginal :: Render os (),
  renderText     :: IO ()
  }

data AppMode = Original | Binarized
