module Main where

import Prelude

import Data.Int (toNumber, round)
import Data.Maybe (maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import React.Basic (JSX, createComponent, make, Self, readState)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

data Optional a
    = Empty
    | Full a

getOrElse :: forall a. a -> Optional a -> a
getOrElse =
    \a opt ->
        case opt of
             Empty -> a
             Full x -> x

orElse :: forall a. Optional a -> Optional a -> Optional a
orElse =
    \a b ->
        case b of
             Empty -> a
             Full _ -> b

optionalToString :: forall a. (a -> String) -> Optional a -> String
optionalToString =
    \f o ->
        case o of
             Empty -> "Empty"
             Full a -> "Full: " <> f a

mapOptional :: forall a b. (a -> b) -> Optional a -> Optional b
mapOptional =
    \f o ->
        case o of
             Empty -> Empty
             Full x -> Full (f x)

twiceOptional ::
    forall a b c.
        (a -> b -> c)
        -> Optional a
        -> Optional b
        -> Optional c

twiceOptional =
    \f oa ob ->
        case oa of
             Empty -> Empty
             -- also: Full a -> mapOptional (f a) ob
             Full a ->
                 case ob of
                      Empty -> Empty
                      Full b -> Full (f a b)

bindOptional ::
    forall a b.
        (a -> Optional b)
        -> Optional a
        -> Optional b

bindOptional =
    \f oa ->
        case oa of
             Empty -> Empty
             Full x -> f x

data List a
    = Nil
    | Cons a (List a)


oneTwoThree :: List Int
oneTwoThree =
    Cons 1 (Cons 2 (Cons 3 Nil))

headOr :: forall a. a -> List a -> a
headOr =
    \a l ->
        foldRight (\acc curr -> acc) a l
-- also: headOr = foldRight (\acc curr -> curr)
-- also: headOr = foldRight const

foldRight :: forall a b. (a -> b -> b) -> b -> List a -> b
foldRight =
    \f b l ->
        case l of
             Nil -> b
             Cons a r -> f a (foldRight f b r)
{-- also valid: --}
{-- case l of --}
{--      Nil -> a --}
{--      Cons x _ -> x --}

listToString :: forall a. Show a => List a -> String
listToString =
  foldRight (\a b -> "(Cons " <> show a <> " " <> b <> ")") "Nil"

mapList :: forall a b. (a -> b) -> List a -> List b
mapList =
    \f l -> foldRight (\curr list -> Cons (f curr) list) Nil l

twiceList :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
twiceList =
    \f la lb ->
        foldRight (\a y -> foldRight (\b z -> Cons (f a b) z) y lb) Nil la

bindList :: forall a b. (a -> List b) -> List a -> List b
bindList =
    \f la ->
        foldRight (\curr list -> foldRight Cons list (f curr)) Nil la

foreign import setSize :: Int -> Effect Unit

didMount :: Self Unit State -> Aff Unit
didMount = \o -> do
    s <- liftEffect (readState o)
    delay (Milliseconds (300.0 - toNumber (4 * s.current)))
    liftEffect (o.setState decrement)
    didMount o

type State = { current :: Int, high :: Int }

increment :: State -> State
increment =
    \o -> o { current = o.current + 1, high = (max o.high (o.current + 1)) }

decrement :: State -> State
decrement =
    \o -> o { current = (max (o.current - 1) 0) }

component :: JSX
component =
    make (createComponent "App") {
    initialState: { current: 0, high: 0 },
    render: \o ->
        R.button {
        type: "button",
        onClick: capture_ (o.setState increment),
        children: [ R.text ("Score: " <> show o.state.current <> ", High: " <> show o.state.high) ]
        },
    didMount: \o -> launchAff_ (didMount o),
    didUpdate: \o _ -> setSize (toPercent o.state)
    } unit

toPercent :: State -> Int
toPercent =
    \o -> round ((toNumber o.current / toNumber o.high) * 100.0)

showx :: Int -> String
showx = \x -> show x

showxy :: Int -> Int -> String
showxy = \x y -> (show x) <> (show y)

main :: Effect Unit
main = do
    log (getOrElse "12" Empty)
    log (optionalToString identity (mapOptional showx (Full 1)))
    log (optionalToString identity (mapOptional showx Empty))
    log (optionalToString identity (twiceOptional showxy (Full 1111) (Full 1234)))
    log (show (headOr 12 oneTwoThree))
    log (show (headOr 12 Nil))
    log (show (foldRight (+) 0 oneTwoThree))
    log (show (headOr 0 (mapList (\x -> x + 1) oneTwoThree)))
    log (listToString oneTwoThree)
    w <- window
    d <- document w
    e <- getElementById "app" (toNonElementParentNode d)
    maybe (log "#app not found") (R.render component) e
