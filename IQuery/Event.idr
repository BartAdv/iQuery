module Event

import IQuery.Key
import Data.List

%access public

public
data EventType : Type where
  Click : EventType
  DoubleClick : EventType
  MouseDown : EventType
  MouseMove : EventType
  MouseOver : EventType
  MouseOut : EventType
  MouseUp : EventType
  KeyDown : EventType
  KeyUp : EventType
  KeyPress : EventType
  Abort : EventType
  Error : EventType
  Load : EventType
  Resize : EventType
  Scroll : EventType
  Unload : EventType
  Blur : EventType
  Change : EventType
  Focus : EventType
  Reset : EventType
  Select : EventType
  Submit : EventType

instance Show EventType where
  show Click = "click"
  show DoubleClick = "dblclick"
  show MouseDown = "mousedown"
  show MouseMove = "mousemove"
  show MouseOver = "mouseover"
  show MouseOut = "mouseout"
  show MouseUp = "mouseup"
  show KeyDown = "keydown"
  show KeyUp = "keyup"
  show KeyPress = "keypress"
  show Abort = "abort"
  show Error = "error"
  show Load = "load"
  show Resize = "resize"
  show Scroll = "scroll"
  show Unload = "unload"
  show Blur = "blur"
  show Change = "change"
  show Focus = "focus"
  show Reset = "reset"
  show Select = "select"
  show Submit = "submit"

abstract
data Event : EventType -> Type where
  MkEvent : Ptr -> Event e

private
evProp : {fty : FTy} -> String -> Event et -> IO (interpFTy fty)
evProp {fty} propName (MkEvent e) = mkForeign (
                                      FFun "%0[%1]" [ FPtr, FString ] fty
                                    ) e propName

private
boolProp : String -> Event et -> IO Bool
boolProp propName e = map toBool $ evProp {fty = FInt} propName e
  where toBool : Int -> Bool
        toBool 1 = True
        toBool _ = False

key : {et : EventType}
    -> Event et 
    -> { default tactics { search 3 } p : Elem et [KeyDown, KeyUp, KeyPress]}
    -> IO (Maybe Key)
key e = map fromKeyCode $ evProp {fty = FInt} "keyCode" e

clientX : Event et -> IO Int
clientX = evProp {fty = FInt} "clientX"

clientY : Event et -> IO Int
clientY = evProp {fty = FInt} "clientY"

altKey : Event et -> IO Bool
altKey = boolProp "altKey"

ctrlKey : Event et -> IO Bool
ctrlKey = boolProp "ctrlKey"

metaKey : Event et -> IO Bool
metaKey = boolProp "metaKey"

shiftKey : Event et -> IO Bool
shiftKey = boolProp "shiftKey"
