module Html.Helper where

-- lens package
import Control.Lens

-- blaze-html package
import Text.Blaze.Html

-- | Lens for generating `Markup` type
markup :: ToMarkup a => Getter a Markup
markup = to toMarkup

value :: ToValue a => Getter a AttributeValue
value = to toValue
