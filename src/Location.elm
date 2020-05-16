module Location exposing (..)


import List.Extra


type alias Located a =
  { from : (Int, Int)
  , value : a
  , to : (Int, Int)
  }


withLocation : Located a -> b -> Located b
withLocation location value =
  { from =
    location.from
  , to =
    location.to
  , value =
      value
  }


fakeLocated : a -> Located a
fakeLocated value =
  { from =
    (-1, -1)
  , to =
    (-1, -1)
  , value =
    value
  }


showLocation : String -> Located a -> String
showLocation src located =
  let
    (fromRow, fromCol) =
      located.from
    (toRow, toCol) =
      located.to
  in
  showLocationRange fromRow fromCol toRow toCol src


showLocationRange : Int -> Int -> Int -> Int -> String -> String
showLocationRange startRow startCol endRow endCol src =
  String.join "\n" <|
  List.map
  (\row ->
    let
      rawLine =
        getLine row src
      line =
        String.fromInt row ++ "| " ++ (String.trimLeft <| rawLine)
      offset =
        String.length line - String.length rawLine - 1
      underlineStartCol =
        if row == startRow then
          offset + startCol
        else
          1
      underlineEndCol =
        if row == endRow then
          offset + endCol
        else
          String.length line
      underline =
        makeUnderline line underlineStartCol underlineEndCol
    in
    line ++ "\n" ++ underline
  )
  (List.range startRow endRow)


makeUnderline : String -> Int -> Int -> String
makeUnderline row minCol maxCol =
  String.toList row
    |> List.indexedMap (\i _ -> toUnderlineChar minCol maxCol i)
    |> String.fromList


toUnderlineChar : Int -> Int -> Int -> Char
toUnderlineChar minCol maxCol col =
  if minCol <= col && col < maxCol then
    '^'
  else
    ' '


getLine : Int -> String -> String
getLine row src =
  Maybe.withDefault ("CAN'T GET LINE AT ROW " ++ String.fromInt row) -- impossible
    <| List.Extra.getAt (row - 1) <| String.split "\n" src