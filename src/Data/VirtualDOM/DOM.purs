module Data.VirtualDOM.DOM (api) where

import Prelude
import DOM.Event.EventTarget as EventTarget
import DOM.HTML as HTML
import DOM.HTML.Window as Window
import DOM.Node.Document as Doc
import DOM.Node.Element as Element
import DOM.Node.Node as Node
import DOM.Node.NodeList as NodeList
import Data.VirtualDOM as VDOM
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener)
import DOM.Event.Types (Event, EventTarget, EventType(EventType))
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.Node.Types (Element, Document, textToNode, elementToNode, Node)
import Data.Maybe (Maybe(Just))
import Unsafe.Coerce (unsafeCoerce)

document :: ∀ e. Eff (dom :: DOM | e) Document
document = HTML.window >>= Window.document >>= htmlDocumentToDocument >>> pure

createElement :: ∀ e. String → Eff (dom :: DOM | e) Node
createElement name = document >>= Doc.createElement name >>= elementToNode >>> pure

createElementNS :: ∀ e. String → String → Eff (dom :: DOM | e) Node
createElementNS ns name = document >>= Doc.createElementNS (Just ns) name >>= elementToNode >>> pure

createTextNode :: ∀ e. String → Eff (dom :: DOM | e) Node
createTextNode t = document >>= Doc.createTextNode t >>= textToNode >>> pure

replaceChild :: ∀ e. Node → Node → Node → Eff (dom :: DOM | e) Unit
replaceChild new old parent = void $ Node.replaceChild new old parent

removeChild :: ∀ e. Node → Node → Eff (dom :: DOM | e) Unit
removeChild child parent = void $ Node.removeChild child parent

appendChild :: ∀ e. Node → Node → Eff (dom :: DOM | e) Unit
appendChild child parent = void $ Node.appendChild child parent

childCount :: ∀ e. Node → Eff (dom :: DOM | e) Int
childCount = Node.childNodes >=> NodeList.length

childAt :: ∀ e. Int → Node → Eff (dom :: DOM | e) (Maybe Node)
childAt index node = Node.childNodes node >>= NodeList.item index

nextSibling :: ∀ e. Node → Eff (dom :: DOM | e) (Maybe Node)
nextSibling = Node.nextSibling >=> pure

setTextContent :: ∀ e. String → Node → Eff (dom :: DOM | e) Unit
setTextContent = Node.setTextContent

unsafeCastElement :: Node → Element
unsafeCastElement = unsafeCoerce

unsafeCastEventTarget :: Node → EventTarget
unsafeCastEventTarget = unsafeCoerce

setAttribute :: ∀ e. String → String → Node → Eff (dom :: DOM | e) Unit
setAttribute key value = unsafeCastElement >>> Element.setAttribute key value

removeAttribute :: ∀ e. String → Node → Eff (dom :: DOM | e) Unit
removeAttribute key = unsafeCastElement >>> Element.removeAttribute key

addEventListener :: ∀ e. String → (Event → Eff (dom :: DOM | e) Unit) → Node → Eff (dom :: DOM | e) Unit
addEventListener name handler node =
  EventTarget.addEventListener (EventType name) (eventListener handler) false (unsafeCastEventTarget node)



-- | An implementation of the `DOM` interface for the DOM itself,
-- | using `purescript-dom`.
api :: ∀ e. VDOM.DOM (dom :: DOM | e) Node Event
api =
  { createElement
  , createElementNS
  , createTextNode
  , replaceChild
  , removeChild
  , appendChild
  , childCount
  , childAt
  , setTextContent
  , setAttribute
  , removeAttribute
  , addEventListener
  }
