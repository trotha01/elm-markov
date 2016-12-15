module Markov exposing (..)

{-| This library is for markov chaining
-}

import Dict exposing (Dict)
import Random exposing (Generator)
import String
import Regex


type alias Markov =
    { lastWord : String
    , dict : MarkovDict
    , seed : Random.Seed
    }


{-| markovify takes a seed, a string and returns a markov generator
-}
markovify : Random.Seed -> String -> Markov
markovify seed str =
    let
        words =
            (String.split " " str)

        mchain =
            chain words

        mdict =
            dictFromChain mchain
    in
        { seed = seed, lastWord = "", dict = mdict }


{-| use a different input for the generator
-}
newInput : Markov -> String -> Markov
newInput markov input =
    markovify markov.seed input


{-| next takes a markov generator and returns
   a string and a new markov generator
-}
next : Markov -> ( String, Markov )
next markov =
    let
        ( word, nextSeed ) =
            Random.step (nextWord markov.dict markov.lastWord) markov.seed

        ( word2, nextSeed2 ) =
            case (Debug.log "next" word) of
                Just w ->
                    ( w, nextSeed )

                Nothing ->
                    -- start a new sentence
                    ( "", nextSeed )

        ( word3, lastWord ) =
            if word2 == "" then
                ( ".", "" )
            else
                ( word2, word2 )
    in
        ( word3, { markov | seed = nextSeed2, lastWord = lastWord } )


{-| chain creates a list of pairs from the list. Empty string represents the start of a sentence.
    example:
    chain ["one","two","one","three"] == [("","one"),("one","three"),("two","one"),("one","two")]
-}
chain : List String -> List ( String, String )
chain list =
    chain_ [] ("" :: list)


chain_ : List ( String, String ) -> List String -> List ( String, String )
chain_ pairs list =
    case list of
        a :: b :: t ->
            if (endOfSentence a) then
                chain_ (( (String.dropRight 1 a), "" ) :: ( "", b ) :: pairs) (b :: t)
            else
                chain_ (( a, b ) :: pairs) (b :: t)

        a :: _ ->
            if (endOfSentence a) then
                (( (String.dropRight 1 a), "" ) :: pairs)
            else
                pairs

        _ ->
            pairs


endOfSentence : String -> Bool
endOfSentence word =
    Regex.contains (Regex.regex "\\.|\\?|\\!") word


type alias MarkovDict =
    Dict String (List String)


{-|
   dictFromChain d [("one","three"),("two","one"),("one","two")] ==
     {
       "one" -> ["two","three"]
       "two" -> ["one"]
     }
-}
dictFromChain : List ( String, String ) -> MarkovDict
dictFromChain chain =
    dictFromChain_ Dict.empty chain


dictFromChain_ : MarkovDict -> List ( String, String ) -> MarkovDict
dictFromChain_ d chain =
    case chain of
        ( k, v ) :: t ->
            case Dict.get k d of
                Nothing ->
                    dictFromChain_ (Dict.insert k [ v ] d) t

                Just list ->
                    dictFromChain_ (Dict.insert k (v :: list) d) t

        _ ->
            d


type alias Word =
    String


type alias Sentence =
    String


{-|
   next, given a dictionary and a word, gives the next word
   next
     { "one" -> ["two","three"]
     , "two" -> ["one"]
     }
     "one"
     ==
    "two"
-}
nextWord : MarkovDict -> String -> Random.Generator (Maybe String)
nextWord d word =
    case Dict.get (Debug.log "word" word) d of
        Nothing ->
            randConst Nothing

        Just list ->
            Random.map
                (\i -> itemAt (Debug.log "i" i) list)
                (Random.int 0 ((List.length list) - 1))


itemAt : Int -> List a -> Maybe a
itemAt n list =
    List.take (n + 1) list |> List.reverse |> List.head


randConst : a -> Random.Generator a
randConst a =
    -- TODO: is there a better way to make a random const?
    Random.map (\_ -> a) Random.bool


{-|
   sentence, given a dictionary, a max length, and the starting word, generated a new sentence
-}
sentence : MarkovDict -> Int -> Sentence -> String -> Random.Generator Sentence
sentence d max currentSentence currentWord =
    if max < 0 then
        randConst currentSentence
    else
        nextWord d currentWord
            |> Random.andThen
                (\w ->
                    case w of
                        Nothing ->
                            randConst currentSentence

                        Just word ->
                            sentence d (max - 1) (currentSentence ++ " " ++ word) word
                )
