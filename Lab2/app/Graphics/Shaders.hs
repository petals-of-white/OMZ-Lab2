module Graphics.Shaders where

import           Data.Word      (Word16, Word32)
import           Graphics.GPipe
import           Prelude        hiding ((<*))

data ShaderEnvironment os prim source target a = ShaderEnvironment
  {
    primitives :: PrimitiveArray prim a,
    colorImage :: Image (Format target),
    clrMask    :: Color target Bool,
    tex2D      :: Texture2D os (Format source),
    window     :: Window os RGBFloat (),
    blending   :: Blending
  }

type PorihUni os = (Buffer os (Uniform (B Word32)), Int)

type WindowLevelUni os = (Buffer os (Uniform (B2 Word32)), Int)

copyTexture :: (ColorRenderable c) => Shader os (ShaderEnvironment os prim c c (B2 Float, B2 Float)) ()
copyTexture  = do
    (fragmentStream, sampler) <- mapShader (\ShaderEnvironment {primitives, tex2D} -> (primitives, tex2D)) common
    let sampleTexture = sample2D sampler SampleAuto Nothing Nothing
    let fragmentStream2 = fmap sampleTexture fragmentStream
    draw (const NoBlending) fragmentStream2
      (drawColor (\ShaderEnvironment{colorImage=img, clrMask=mask} -> (img, mask, False)))


texToFloat :: forall src os prim s c h.
  (ColorRenderable src, c ~ ColorElement src, s ~ S F c, Convert s, ColorElement src ~ h, Color src (S F h) ~ S F h,
  ConvertFloat (S F h) ~ S F Float, ConvertFloat s ~ S F Float) =>
  S F Float -> Shader os (ShaderEnvironment os prim src RFloat (B2 Float, B2 Float)) (FragmentStream (S F Float))
texToFloat scale = do
    (fragmentStream, sampler) <- mapShader (\ShaderEnvironment {primitives, tex2D} -> (primitives, tex2D)) common
    let sampleTexture = sample2D sampler SampleAuto Nothing Nothing
    let fragmentStream2 = fmap ((* scale) . toFloat . sampleTexture) fragmentStream
    return fragmentStream2


binarize ::  Shader os (ShaderEnvironment os prim RWord RWord (B2 Float, B2 Float), PorihUni os) ()
binarize = do
  (fragmentStream, sampler) <- mapShader (\(ShaderEnvironment {primitives, tex2D}, _) -> (primitives, tex2D)) common
  uniform <- getUniform snd
  let sampleTexture = sample2D sampler SampleAuto Nothing Nothing
  let fragmentStream2 = fmap sampleTexture fragmentStream

  draw (blending . fst) fragmentStream2
    (\c ->
      let porih = uniform
          binarized = ifThenElse' (c >* porih) c 0 in
      drawColor (\(ShaderEnvironment{colorImage=img, clrMask=mask}, _) -> (img, mask, False) ) binarized)



windowLevel :: Shader os (ShaderEnvironment os prim RWord RWord (B2 Float, B2 Float), WindowLevelUni os) ()
windowLevel = do

  (fragmentStream, sampler) <- mapShader (\(ShaderEnvironment {primitives, tex2D}, _) -> (primitives, tex2D)) common
  (V2 l w) <- getUniform snd

  let sampleTexture = sample2D sampler SampleAuto Nothing Nothing
  let fragmentStream2 = fmap sampleTexture fragmentStream

  draw (blending . fst) fragmentStream2
    (\c ->
      let transformed =
            let minV = 0 :: S F Word
                maxV = 65535 :: S F Word
            in

            ifThenElse (c <=* l - (w `div'` 2))
              (const minV)
              (const $ ifThenElse (c <=* l + (w `div'` 2)) (const (minV + (c - l + (w `div'` 2)))) (const maxV) c)
              c
      in
      drawColor (\(ShaderEnvironment{colorImage=img, clrMask=mask}, _) -> (img, mask, False) ) 0)


texToWindow :: (ColorRenderable src, c ~ ColorElement src, s ~ S F c, Convert s, ColorElement src ~ h, Color src (S F h) ~ S F h,
  ConvertFloat (S F h) ~ S F Float, ConvertFloat s ~ S F Float) =>
  S F Float -> Shader os (ShaderEnvironment os prim src RFloat (B2 Float, B2 Float)) ()
texToWindow scale = do
  floatStream <- texToFloat scale
  let floatStreamV =  fmap pure floatStream
  drawWindowColor
    (\ShaderEnvironment{clrMask=mask, window=win, blending=blend} -> (win, ContextColorOption blend (pure mask) ))
    floatStreamV


common :: ColorRenderable c => Shader os (PrimitiveArray prim (B2 Float, B2 Float), Texture2D os (Format c)) (FragmentStream (V2 FFloat), Sampler2D (Format c))
common = do
  primitiveStream  <- toPrimitiveStream fst
  let primitiveStream2 = fmap (\(V2 x y, uv) -> (V4 x y 0 1, uv)) primitiveStream

  fragmentStream <- rasterize (\(_, tex) ->
                        let V2 rows columns = head $ texture2DSizes tex in
                        (FrontAndBack, ViewPort (V2 0 0) (V2 rows columns), DepthRange 0 1)) primitiveStream2


  let edge = (pure ClampToEdge, undefined)
  samp <- newSampler2D $ \(_, tex) -> (tex, SamplerNearest, edge)
  return (fragmentStream, samp)
