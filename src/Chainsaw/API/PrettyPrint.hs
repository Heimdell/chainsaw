
module Chainsaw.API.PrettyPrint
    ( module Text.PrettyPrint
    , Pretty (..)
    , block
    , list
    , wrap
    , (=:)
    )
    where

import Text.PrettyPrint hiding ((<>))

class Pretty p where
    pretty :: p -> Doc

instance Pretty p => Pretty [p] where
    pretty = list . map pretty

wrap :: String -> Doc -> Doc
wrap header item =
    hang (hang (text header <+> text "(") 4 item) 0 (text ")")
  where
    aux [] = [text "{}"]
    aux (x : xs) = [text "{" <+> x] ++ map (text "," <+>) xs ++ [text "}"]

block :: String -> [Doc] -> Doc
block header items =
    hang (text header) 4 $
        vcat (aux items)
  where
    aux [] = [text "{}"]
    aux (x : xs) = [text "{" <+> x] ++ map (text "," <+>) xs ++ [text "}"]

list :: [Doc] -> Doc
list [] = text "[]"
list (x : xs) = vcat $ [text "[" <+> x] ++ map (text "," <+>) xs ++ [text "]"]

(=:) :: String -> Doc -> Doc
txt =: val = hang (text txt <+> text "=") 4 val
