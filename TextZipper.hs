{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TextZipper where

import "text-zipper" Data.Text.Zipper

import "base" Data.Foldable (sequence_) 
import "base" Data.Function ((&))  
 
{-

moveCursor 
Move the cursor to the specified row and column. Invalid cursor positions will be ignored. Valid cursor positions range as described for cursorPosition.

moveRight  
Move the cursor right by one position. If the cursor is at the end of a line, the cursor is moved to the first position of the following line (if any).

moveLeft 
Move the cursor left by one position. If the cursor is at the beginning of a line, the cursor is moved to the last position of the preceding line (if any).

moveUp 
Move the cursor up by one row. If there no are rows above the current one, move to the first position of the current row. If the row above is shorter, move to the end of that row.

moveDown
Move the cursor down by one row. If there are no rows below the current one, move to the last position of the current row. If the row below is shorter, move to the end of that row.

gotoEOL
Move the cursor to the end of the current line.

gotoBOL
Move the cursor to the beginning of the current line.


currentChar
Get the Char on which the cursor currently resides. If the cursor is at the end of the text or the text is empty return Nothing.

nextChar
Get the Char after the cursor position. If the cursor is at the end of a line return the first character of the next line, or if that one is empty as well, return Nothing.

previousChar 
Get the Char before the cursor position. If the cursor is at the beginning of the text, return Nothing


insertChar
Insert a character at the current cursor position.
If the character is a newline, break the current line.
If the character is non-printable, ignore it.
Otherwise insert the character and move the cursor one position to the right.

insertMany 
Insert many characters at the current cursor position. Move the cursor to the end of the inserted text.

deletePrevChar
Delete the character preceding the cursor position, and move the cursor backwards by one character.

deleteChar
Delete the character at the cursor position. Leaves the cursor position unchanged. If the cursor is at the end of a line of text, this combines the line with the line below.

breakLine
Insert a line break at the current cursor position.

killToEOL
Remove all text from the cursor position to the end of the current line. If the cursor is at the beginning of a line and the line is empty, the entire line will be removed.

killToBOL
Remove all text from the cursor position to the beginning of the current line.

transposeChars
Transpose the character before the cursor with the one at the cursor position and move the cursor one position to the right. If the cursor is at the end of the current line, transpose the current line's last two characters.

-}

main = do
  sequence_ examples

-- (-:) :: a -> b -> (a,b)
-- (-:) = (,)
-- infixr 0 -:

(-:) :: Show a => String -> a -> IO ()
(-:) = pp 
  where
  pp name value = do
    putStrLn ""
    putStrLn name
    print value 
infixr 0 -:

example
  = stringZipper (lines "first line\nsecond line\nthird line\nfourth line") (Just 6)
  & moveCursor (2,4) 

examples =
  [ "(the example editor)" -: example
  , "clearZipper"          -: example & clearZipper
  , "currentLine "         -: example & currentLine 
  , "getText "             -: example & getText 
  , "cursorPosition "      -: example & cursorPosition 
  , "lineLengths "         -: example & lineLengths 
  , "getLineLimit "        -: example & getLineLimit 
  , "moveCursor"           -: example & moveCursor (0,0)
  , "moveLeft "            -: example & moveLeft 
  , "moveRight "           -: example & moveRight 
  , "moveUp "              -: example & moveUp 
  , "moveDown "            -: example & moveDown 
  , "gotoEOL"              -: example & gotoEOL
  , "gotoBOL"              -: example & gotoBOL
  , "currentChar"          -: example & currentChar
  , "nextChar"             -: example & nextChar
  , "previousChar"         -: example & previousChar 
  , "insertChar"           -: example & insertChar 'X' 
  , "insertMany"           -: example & insertMany "XXX" 
  , "deletePrevChar"       -: example & deletePrevChar
  , "deleteChar"           -: example & deleteChar
  , "breakLine"            -: example & breakLine
  , "killToEOL"            -: example & killToEOL
  , "killToBOL"            -: example & killToBOL
  , "transposeChars"       -: example & transposeChars
  ]
