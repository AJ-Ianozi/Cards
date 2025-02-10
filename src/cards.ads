-------------------------------------------------------------------------------
--     Cards - An implementation the standard 52-card deck
-------------------------------------------------------------------------------
--  Copyright (c) 2025 AJ Ianozi
--  Licensed under the MIT License.  See attached LICENSE for details.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Containers.Vectors;

--  ****h* Cards/Cards
--  SOURCE
package Cards is
--  DESCRIPTION
--    This package provides Suits, Rank, and Cards.
--    While this library has helper functions to print Joker and blank cards,
--    it does NOT support Joker cards.
--  ****

   --  ****d* Cards/Cards.Version
   --  SOURCE
   Version : constant String := "1.0.0-rc";
   --  DESCRIPTION
   --    Version of the library itself.
   --  ****

   --  ****t* Cards/Cards.Black_White
   --  SOURCE
   type Black_White is (
      Black, --  Default color e.g. '♥'
      White  --  Alternate color e.g. '♡'
   );
   --  DESCRIPTION
   --    Option for retrieving suit or symbol.
   --  ****

   --  ****t* Cards/Cards.Suit_Symbol
   --  SOURCE
   subtype Suit_Symbol is Wide_Wide_Character
      with Static_Predicate => Suit_Symbol in
         Wide_Wide_Character'Val (16#2665#) | --  ♥
         Wide_Wide_Character'Val (16#2663#) | --  ♣
         Wide_Wide_Character'Val (16#2666#) | --  ♦
         Wide_Wide_Character'Val (16#2660#) | --  ♠
         Wide_Wide_Character'Val (16#2661#) | --  ♡
         Wide_Wide_Character'Val (16#2667#) | --  ♧
         Wide_Wide_Character'Val (16#2662#) | --  ♢
         Wide_Wide_Character'Val (16#2664#);  --  ♤
   --  DESCRIPTION
   --    Symbols restricted to possible suit symbols.
   --  ****

   --  ****t* Cards/Cards.Card_Symbol
   --  SOURCE
   subtype Card_Symbol is Wide_Wide_Character
      with Static_Predicate => Card_Symbol in
         Wide_Wide_Character'Val (16#0001_F0A1#) | --  🂡: Ace Of Spades
         Wide_Wide_Character'Val (16#0001_F0A2#) | --  🂢: Two Of Spades
         Wide_Wide_Character'Val (16#0001_F0A3#) | --  🂣: Three Of Spades
         Wide_Wide_Character'Val (16#0001_F0A4#) | --  🂤: Four Of Spades
         Wide_Wide_Character'Val (16#0001_F0A5#) | --  🂥: Five Of Spades
         Wide_Wide_Character'Val (16#0001_F0A6#) | --  🂦: Six Of Spades
         Wide_Wide_Character'Val (16#0001_F0A7#) | --  🂧: Seven Of Spades
         Wide_Wide_Character'Val (16#0001_F0A8#) | --  🂨: Eight Of Spades
         Wide_Wide_Character'Val (16#0001_F0A9#) | --  🂩: Nine Of Spades
         Wide_Wide_Character'Val (16#0001_F0AA#) | --  🂪: Ten Of Spades
         Wide_Wide_Character'Val (16#0001_F0AB#) | --  🂫: Jack Of Spades
         Wide_Wide_Character'Val (16#0001_F0AD#) | --  🂭: Queen Of Spades
         Wide_Wide_Character'Val (16#0001_F0AE#) | --  🂮: King Of Spades
         Wide_Wide_Character'Val (16#0001_F0B1#) | --  🂱: Ace Of Hearts
         Wide_Wide_Character'Val (16#0001_F0B2#) | --  🂲: Two Of Hearts
         Wide_Wide_Character'Val (16#0001_F0B3#) | --  🂳: Three Of Hearts
         Wide_Wide_Character'Val (16#0001_F0B4#) | --  🂴: Four Of Hearts
         Wide_Wide_Character'Val (16#0001_F0B5#) | --  🂵: Five Of Hearts
         Wide_Wide_Character'Val (16#0001_F0B6#) | --  🂶: Six Of Hearts
         Wide_Wide_Character'Val (16#0001_F0B7#) | --  🂷: Seven Of Hearts
         Wide_Wide_Character'Val (16#0001_F0B8#) | --  🂸: Eight Of Hearts
         Wide_Wide_Character'Val (16#0001_F0B9#) | --  🂹: Nine Of Hearts
         Wide_Wide_Character'Val (16#0001_F0BA#) | --  🂺: Ten Of Hearts
         Wide_Wide_Character'Val (16#0001_F0BB#) | --  🂻: Jack Of Hearts
         Wide_Wide_Character'Val (16#0001_F0BD#) | --  🂽: Queen Of Hearts
         Wide_Wide_Character'Val (16#0001_F0BE#) | --  🂾: King Of Hearts
         Wide_Wide_Character'Val (16#0001_F0C1#) | --  🃁: Ace Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0C2#) | --  🃂: Two Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0C3#) | --  🃃: Three Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0C4#) | --  🃄: Four Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0C5#) | --  🃅: Five Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0C6#) | --  🃆: Six Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0C7#) | --  🃇: Seven Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0C8#) | --  🃈: Eight Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0C9#) | --  🃉: Nine Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0CA#) | --  🃊: Ten Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0CB#) | --  🃋: Jack Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0CD#) | --  🃍: Queen Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0CE#) | --  🃎: King Of Diamonds
         Wide_Wide_Character'Val (16#0001_F0D1#) | --  🃑: Ace Of Clubs
         Wide_Wide_Character'Val (16#0001_F0D2#) | --  🃒: Two Of Clubs
         Wide_Wide_Character'Val (16#0001_F0D3#) | --  🃓: Three Of Clubs
         Wide_Wide_Character'Val (16#0001_F0D4#) | --  🃔: Four Of Clubs
         Wide_Wide_Character'Val (16#0001_F0D5#) | --  🃕: Five Of Clubs
         Wide_Wide_Character'Val (16#0001_F0D6#) | --  🃖: Six Of Clubs
         Wide_Wide_Character'Val (16#0001_F0D7#) | --  🃗: Seven Of Clubs
         Wide_Wide_Character'Val (16#0001_F0D8#) | --  🃘: Eight Of Clubs
         Wide_Wide_Character'Val (16#0001_F0D9#) | --  🃙: Nine Of Clubs
         Wide_Wide_Character'Val (16#0001_F0DA#) | --  🃚: Ten Of Clubs
         Wide_Wide_Character'Val (16#0001_F0DB#) | --  🃛: Jack Of Clubs
         Wide_Wide_Character'Val (16#0001_F0DD#) | --  🃝: Queen Of Clubs
         Wide_Wide_Character'Val (16#0001_F0DE#) | --  🃞: King Of Clubs
         Wide_Wide_Character'Val (16#0001_F0CF#) | --  🃏︎: Black Joker
         Wide_Wide_Character'Val (16#0001_F0DF#) | --  🃟: White Joker
         Wide_Wide_Character'Val (16#0001_F0A0#);  --  🂠: Blank card
   --  DESCRIPTION
   --    Symbols restricted mostly to possible playing cards.
   --  ****

   --  ****d* Cards/Cards.Blank_Card
   --  SOURCE
   Blank_Card : constant Card_Symbol := Wide_Wide_Character'Val (16#1F0A0#);
   --  DESCRIPTION
   --    A card symbol with no face or symbol: `🂠`
   --  ****

   --  ****f* Cards/Cards.Joker_Symbol_Char
   --  SOURCE
   function Joker_Symbol_Char
      (
       Symbol_Color : Black_White --  character color; default: Black
      ) return Card_Symbol;
   function Joker_Symbol_Char return Card_Symbol;
   --  FUNCTION
   --    Retrieve the unicode symbol for a joker.
   --  PARAMETERS
   --    Symbol_Color - Default color of character, default is black
   --  RETURN VALUE
   --    Cards.Card_Symbol - Unicode symbol of joker
   --  EXAMPLE
   --    --  This will print "🃏︎"
   --    Ada.Wide_Wide_Text_IO.Put (Joker_Symbol_Char);
   --    --  This will print "🃟";
   --    Ada.Wide_Wide_Text_IO.Put (Joker_Symbol_Char (White));
   --  ****

   --  ****f* Cards/Cards.Joker_Symbol
   --  SOURCE
   function Joker_Symbol
      (
       Symbol_Color : Black_White --  character color; default: Black
      ) return Wide_Wide_String;
   function Joker_Symbol return Wide_Wide_String;
   --  FUNCTION
   --    Retrieve the unicode symbol for a joker, as a string.
   --  PARAMETERS
   --    Symbol_Color - Default color of character, default is black
   --  RETURN VALUE
   --    Cards.Card_Symbol - Unicode symbol of joker
   --  EXAMPLE
   --    --  This will print "🃏︎"
   --    Ada.Wide_Wide_Text_IO.Put (Joker_Symbol);
   --    --  This will print "🃟";
   --    Ada.Wide_Wide_Text_IO.Put (Joker_Symbol (White));
   --  ****

   --  ****t* Cards/Cards.Colors
   --  SOURCE
   type Colors is (
      Black, --  Color of suit is black
      Red    --  Color of suit is red
   );
   --  DESCRIPTION
   --    The color that a certain suit is.
   --  ****

   --  ****t* Cards/Cards.Draw_From
   --  SOURCE
   type Draw_From is (Top, Bottom, Random);
   --  DESCRIPTION
   --    Option for which part of the deck to draw a card from.
   --  ****

   --  ****t* Cards/Cards.Suit
   --  SOURCE
   type Suit is (Hearts, Clubs, Diamonds, Spades);
   --  DESCRIPTION
   --    The four suits of a playing card.
   --  ****

   --  ****f* Cards.Suit/Color
   --  SOURCE
   function Color (
         Item : Suit -- The suit to retrieve the color from
      ) return Colors;
   --  FUNCTION
   --    Retrieve the color of a given suit
   --  PARAMETERS
   --    Item - The suit to retrieve the color from
   --  RETURN VALUE
   --    Cards.Colors - Color of the suit
   --  EXAMPLE
   --    Assert (Color (Hearts) = Red);
   --  ****

   --  ****f* Cards.Suit/Symbol_Char
   --  SOURCE
   function Symbol_Char
      (Item : Suit; --  The suit to retrieve the symbol from
       Symbol_Color : Black_White := Black --  character color; default: Black
      ) return Suit_Symbol;
   --  FUNCTION
   --    Retrieve the unicode symbol for a given suit.
   --  PARAMETERS
   --    Item - The suit to retrieve the symbol from
   --    Symbol_Color - Default color of character
   --  RETURN VALUE
   --    Cards.Suit_Symbol - Unicode symbol of suit
   --  EXAMPLE
   --    --  This will print "♥"
   --    Ada.Wide_Wide_Text_IO.Put (Symbol_Char (Hearts));
   --    --  This will print "♡";
   --    Ada.Wide_Wide_Text_IO.Put (Symbol_Char (Hearts, White));
   --  ****

   --  ****f* Cards.Suit/Symbol
   --  SOURCE
   function Symbol
      (Item : Suit; --  The suit to retrieve the color from
       Symbol_Color : Black_White := Black --  character color; default: Black
      ) return Wide_Wide_String; -- Wide_Wide_String of suit unicode symbol
   --  FUNCTION
   --    Retrieve the unicode symbol as a string for a given suit.
   --  PARAMETERS
   --    Item - The suit to retrieve the color from
   --    Symbol_Color - color of character, default is black
   --  RETURN VALUE
   --    Wide_Wide_String - containing unicode symbol of suit
   --  EXAMPLE
   --    --  This will print "♥"
   --    Ada.Wide_Wide_Text_IO.Put (Symbol (Hearts));
   --    --  This will print "♡";
   --    Ada.Wide_Wide_Text_IO.Put (Symbol (Hearts, White));
   --  ****

   --  ****f* Cards.Suit/Abbr
   --  SOURCE
   function Abbr (Item : Suit) return String;
   --  FUNCTION
   --    Retrieve the abbreviation for a given suit.
   --  PARAMETERS
   --    Item - The suit to retrieve the abbreviation for.
   --  RETURN VALUE
   --    String containing "H", "C", "D", or "S"
   --  EXAMPLE
   --    --  This will print "H"
   --    Ada.Text_IO.Put (Abbr (Hearts));
   --    --  This will print "S";
   --    Ada.Text_IO.Put (Abbr (Spades));
   --  ****

   --  ------------------------------------------------------------------------
   --                            Card Ranks
   --  ------------------------------------------------------------------------

   --  ****t* Cards/Cards.Rank
   --  SOURCE
   type Rank is (Two, Three, Four, Five, Six, Seven, Eight,
                 Nine, Ten, Jack, Queen, King, Ace);
   for Rank use (2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14);
   --  DESCRIPTION
   --    The possible rank of each card, excluding the Joker (see note below).
   --    Each rank's enumeration value is from 2 to 14 (With Ace = 14)
   --    I recommended to retrieve the value via Cards/Card.Value
   --  DERIVED BY
   --    * Cards.Face
   --    * Cards.Pip
   --  ****

   --  ****t* Cards/Cards.Face
   --  SOURCE
   subtype Face is Rank range Jack .. King;
   --  DESCRIPTION
   --    Subset of Cards.Rank that only include the face cards.
   --  DERIVED FROM
   --    * Cards.Rank
   --  ****

   --  ****t* Cards/Cards.Pip
   --  SOURCE
   subtype Pip is Rank
      with Static_Predicate => Pip in Two .. Ten | Ace;
   --  DESCRIPTION
   --    Subset of Cards.Rank that only include the face pip.
   --  DERIVED FROM
   --    * Cards.Rank
   --  ****

   --  ****f* Cards.Rank/Abbr
   --  SOURCE
   function Abbr (Item : Rank) return String;
   --  FUNCTION
   --    Retrieve the abbreviation for a given rank.
   --  PARAMETERS
   --    Item - The suit to retrieve the abbreviation for.
   --  RETURN VALUE
   --    String containing the shorthand name for the suit.
   --  EXAMPLE
   --    --  This will print "K"
   --    Ada.Wide_Wide_Text_IO.Put (Abbr (King));
   --    --  This will print "10";
   --    Ada.Wide_Wide_Text_IO.Put (Abbr (Ten));
   --  ****

   --  ****t* Cards/Cards.Card
   --  SOURCE
   type Card is tagged record
      Rank : Cards.Rank; --  Rank of the current card
      Suit : Cards.Suit; --  Suit of the current card
   end record;
   --  DESCRIPTION
   --    A single card holding a Suit and Rank item, such as an Ace of Spades.
   --  METHODS
   --    * Cards.Card/Value
   --    * Cards.Card/Is_Face
   --    * Cards.Card/Is_Ace
   --    * Cards.Card/Is_Pip
   --    * Cards.Card/Name
   --    * Cards.Card/Name_Symbol
   --    * Cards.Card/Symbol
   --  ****

   --  ****f* Cards.Card/Value
   --  SOURCE
   function Value (Self : Card; Ace_Value : Positive := 1) return Positive;
   --  FUNCTION
   --    Returns the numerical value of the card.
   --  PARAMETERS
   --    Self - The card
   --    Ace_Value - The value of an Ace.  Default is 1, but may be redefined.
   --  RETURN VALUE
   --    * Ace is either 1 or the value of "Ace_Value"
   --    * All oher pip cards are valued by their pip rank, e.g. "Two" is 2
   --    * Face Cards are 10.
   --  EXAMPLE
   --    A_S : constant Card := (Rank => Ace, Suit => Spades);
   --    K_S : constant Card := (Rank => King, Suit => Spades);
   --    T_H : constant Card := (Rank => Ten, Suit => Hearts);
   --    F_D : constant Card := (Rank => Five, Suit => Diamonds);
   --    Ace_1 : constant Positive := A_S.Value; --  will hold 1
   --    Ace_10 : constant Positive := A_S.Value (10); --  will hold 10
   --    Five_Value : constant Positive := F_D.Value; -- Will hold 5
   --    Five_Value : constant Positive := F_D.Value (10); -- Will still hold 5
   --    King_Value : constant Positive := K_S.Value; --  will hold 10
   --    Ten_Value : constant Positive := T_H.Value;  --  will hold 10
   --  ****

   --  ****f* Cards.Card/Is_Face
   --  SOURCE
   function Is_Face (Self : Card) return Boolean;
   --  FUNCTION
   --    Determine whether the card is a face card, such as "King of Hearts"
   --  PARAMETERS
   --    Self - The card
   --  RETURN VALUE
   --    True if card is a Jack, Queen, or King. Otherwise, False.
   --  EXAMPLE
   --    if My_Card.Is_Face then
   --      Put_Line ("It is a face card.");
   --    end if;
   --  ****

   --  ****f* Cards.Card/Is_Ace
   --  SOURCE
   function Is_Ace (Self : Card) return Boolean;
   --  FUNCTION
   --    Determine whether the card is an Ace or not, such as an Ace of Spades.
   --  PARAMETERS
   --    Self - The card
   --  RETURN VALUE
   --    True if card is an Ace. Otherwise, False.
   --  EXAMPLE
   --    if My_Card.Is_Ace then
   --      Put_Line ("It is an Ace.");
   --    end if;
   --  ****

   --  ****f* Cards.Card/Is_Pip
   --  SOURCE
   function Is_Pip (Self : Card) return Boolean;
   --  FUNCTION
   --    Determine whether the card is a pip card (Ace through 10).
   --  PARAMETERS
   --    Self - The card
   --  RETURN VALUE
   --    True if card is an Ace, or ranked Two through Ten. Otherwise, False.
   --  EXAMPLE
   --    if My_Card.Is_Pip then
   --      Put_Line ("It is a pip card.");
   --    end if;
   --  ****

   --  ****f* Cards.Card/Name
   --  SOURCE
   function Name
      (Self : Card;
       Long_Format   : Boolean := False -- Short "AS" or long "Ace of Spades"
      ) return String; --  Name of Card.
   --  FUNCTION
   --    Returns the name of the card.
   --  PARAMETERS
   --    Self - The card
   --    Long_Format - Whether to print the full name.  Default is "False"
   --  RETURN VALUE
   --    A string containing the card's name.
   --  EXAMPLE
   --    declare
   --       A_S : constant Card := (Rank => Ace, Suit => Spades);
   --       T_H : constant Card := (Rank => Ten, Suit => Hearts);
   --    begin
   --       --  This will print "AS
   --       Ada.Text_IO.Put_Line (A_S.Name);
   --       --  This will print "Ace of Spades"
   --       Ada.Text_IO.Put_Line (A_S.Name, True);
   --       --  This will print "10H"
   --       Ada.Text_IO.Put_Line (T_H.Name);
   --       --  This will print "Ten of Hearts"
   --       Ada.Text_IO.Put_Line (T_H.Name, True); --  "Ten of Hearts"
   --    end;
   --  SEE ALSO
   --    * Cards.Card/Name_Symbol
   --    * Cards.Card/Symbol_Char
   --    * Cards.Card/Symbol
   --  ****

   --  ****f* Cards.Card/Name_Symbol
   --  SOURCE
   function Name_Symbol (Self : Card;
       Symbol_Color : Black_White := Black -- character color; default: Black
   ) return Wide_Wide_String; -- e.g. 4♥
   --  FUNCTION
   --    Returns the short name of a card using the suit symbol
   --  PARAMETERS
   --    Self - The card
   --    Symbol_Color - Which color of character
   --  RETURN VALUE
   --    Wide_Wide_String containing rank and suit symbmol
   --  EXAMPLE
   --    declare
   --       A_S : constant Card := (Rank => Ace, Suit => Spades);
   --       T_H : constant Card := (Rank => Ten, Suit => Hearts);
   --    begin
   --       --  This will print "A♠"
   --       Ada.Text_IO.Put_Line (A_S.Name_Symbol);
   --       --  This will print "A♤"
   --       Ada.Text_IO.Put_Line (A_S.Name_Symbol (White));
   --       --  This will print "10♥"
   --       Ada.Wide_Wide_Text_IO.Put_Line (T_H.Name_Symbol);
   --       --  This will print "10♡"
   --       Ada.Wide_Wide_Text_IO.Put_Line (T_H.Name_Symbol (White));
   --    end;
   --  SEE ALSO
   --    * Cards.Card/Name
   --    * Cards.Card/Symbol
   --    * Cards.Card/Symbol_Char
   --    * Cards.Card/As_Box
   --  ****

   --  ****f* Cards.Card/As_Box
   --  SOURCE
   function As_Box (Self : Card;
       Symbol_Color : Black_White := Black -- character color; default: Black
   ) return Wide_Wide_String; -- e.g. |4♥|
   --  FUNCTION
   --    Returns the short name of a card with pipes on each side.
   --    It's essentially just wrapping Name_Symbol in "|"
   --  PARAMETERS
   --    Self - The card
   --    Symbol_Color - Which color of character
   --  RETURN VALUE
   --    Wide_Wide_String containing rank and suit symbmol between pipes
   --  EXAMPLE
   --    declare
   --       A_S : constant Card := (Rank => Ace, Suit => Spades);
   --       T_H : constant Card := (Rank => Ten, Suit => Hearts);
   --    begin
   --       --  This will print "|A♠|"
   --       Ada.Text_IO.Put_Line (A_S.As_Box);
   --       --  This will print "|A♤|"
   --       Ada.Text_IO.Put_Line (A_S.As_Box (White));
   --       --  This will print "|10♥|"
   --       Ada.Wide_Wide_Text_IO.Put_Line (T_H.As_Box);
   --       --  This will print "|10♡|"
   --       Ada.Wide_Wide_Text_IO.Put_Line (T_H.As_Box (White));
   --    end;
   --  SEE ALSO
   --    * Cards.Card/Name
   --    * Cards.Card/Symbol_Char
   --    * Cards.Card/Symbol
   --    * Cards.Card/Name_Symbol
   --  ****

   --  ****f* Cards.Card/Symbol_Char
   --  SOURCE
   function Symbol_Char (Self : Card) return Card_Symbol;
   --  FUNCTION
   --    Retrieve the unicode symbol for a given card.
   --  PARAMETERS
   --    Self - The card
   --  RETURN VALUE
   --    Cards.Card_Symbol - the unicode symbol of the card.
   --  EXAMPLE
   --    declare
   --       A_S : constant Card := (Rank => Ace, Suit => Spades);
   --       T_H : constant Card := (Rank => Ten, Suit => Hearts);
   --    begin
   --       --  This will print "🂡"
   --       Ada.Wide_Wide_Text_IO.Put (A_S.Symbol_Char);
   --       --  This will print "🂺"
   --       Ada.Wide_Wide_Text_IO.Put (T_H.Symbol_Char);
   --    end;
   --  SEE ALSO
   --    * Cards.Card/Name
   --    * Cards.Card/Name_Symbol
   --    * Cards.Card/Symbol
   --  ****

   --  ****f* Cards.Card/Symbol
   --  SOURCE
   function Symbol (Self : Card) return Wide_Wide_String; -- returns eg. "🂡"
   --  FUNCTION
   --    Returns the symbol of the card.
   --  PARAMETERS
   --    Self - The card
   --  RETURN VALUE
   --    Wide_Wide_String -- unicode string containing symbol of card
   --  EXAMPLE
   --    declare
   --       A_S : constant Card := (Rank => Ace, Suit => Spades);
   --       T_H : constant Card := (Rank => Ten, Suit => Hearts);
   --    begin
   --       --  This will print "🂡", aka U+1F0A1
   --       Ada.Wide_Wide_Text_IO.Put_Line (A_S.Symbol);
   --       --  This will print "🂺", aka U+1F0BA
   --       Ada.Wide_Wide_Text_IO.Put_Line (T_H.Symbol);
   --    end;
   --  SEE ALSO
   --    * Cards.Card/Name
   --    * Cards.Card/Name_Symbol
   --    * Cards.Card/Symbol_Char
   --  ****

   --  ****t* Cards/Cards.Card_List
   --  SOURCE
   type Card_List is array (Positive range <>) of Card;
   --  DESCRIPTION
   --    A simple array of Cards.Card
   --  ****

   --  ****t* Cards/Cards.Stack
   --  SOURCE
   package Card_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Card);
   type Stack is new Card_Vectors.Vector with null record;
   --  DESCRIPTION
   --    A vector of Cards.Card that can be shuffled or dealt from.
   --    It's derived from Ada.Containers.Vectors and thus you can
   --    utilize any vector-related functions for this datatype.
   --  NOTES
   --    The "top" of the stack is implimented to be Stack.Last_Element and
   --    the bottom of the stack is implimented as Stack.First_Element.
   --  METHODS
   --    * Cards.Stack/Count
   --    * Cards.Stack/To_List
   --    * Cards.Stack/Shuffle
   --    * Cards.Stack/Deal
   --    * Cards.Stack/Deal_All
   --  ****

   --  ****f* Cards.Stack/Count
   --  SOURCE
   function Count (Self : Stack;
                   Which_Suit : Suit;
                   Which_Rank : Rank)
      return Natural;
   --  FUNCTION
   --    Counts how many instances of a card is in the Stack
   --  PARAMETERS
   --    Self - The cards
   --    Which_Suit - The card's suit to count
   --    Which_Rank - The card's rank to count
   --  RETURN VALUE
   --    Natural -- The numeber of times that card exists in the stack
   --  EXAMPLE
   --    CD : constant Stack := Deck;
   --    --  C contains "1"
   --    C : constant Natural := CD.Count (Spades, Ace);
   --  ****

   --  ****f* Cards.Stack/To_List
   --  SOURCE

   function To_List (Self : Stack) return Card_List;
   --  FUNCTION
   --    Converts a card vector to card array
   --  PARAMETERS
   --    Self - The cards
   --  RETURN VALUE
   --    Card_List -- Array of cards containing the current vector of cards
   --  EXAMPLE
   --       CD : constant Stack := Deck;
   --       CL : constant Card_List := CD.To_List;
   --  SEE ALSO
   --    * Cards.Stack/Deck
   --  ****

   --  ****m* Cards.Stack/Shuffle
   --  SOURCE
   procedure Shuffle (Self : in out Stack; Rounds : Positive := 1);
   --  FUNCTION
   --    Shuffle a stack of cards with the provided Rounds.  Each round
   --    is a single pass, swapping the cards between random slots.
   --    Default is a single pass.
   --  PARAMETERS
   --    Self   -- The stack of cards to shuffle
   --    Rounds -- How many times should it be shuffled?  Default is 1 pass.
   --  EXAMPLE
   --    declare
   --       CD : Stack := Deck;
   --    begin
   --       CD.Shuffle; --  This will contain 
   --    end;
   --  SEE ALSO
   --    * Cards.Stack/Deck
   --  ****

   --  ****f* Cards.Stack/Deal
   --  SOURCE
   function Deal (Self : in out Stack;
                  Amount : Natural := 1;
                  From : Draw_From  := Top) return Stack;
   function Deal (Self : in out Stack;
                  Amount : Natural := 1;
                  From : Draw_From  := Top) return Card_List;
   --  FUNCTION
   --    Deal X amount of cards from a stack of cards.  If the stack contains
   --       less then the amount provided, then all of the cards will be dealt.
   --  PARAMETERS
   --    Self   -- The stack of cards to deal from
   --    Amount -- How many cards should be delt from the deck?
   --    From   -- Deal from the Top, Bottom, or a Random position?
   --  RETURN VALUE
   --    Stack or Card_List -- stack or list of cards containing dealt cards. 
   --  NOTE
   --    Since Stack is type Ada.Containers.Vector, you can append the results
   --    of Stack.Deal as a parameter to Stack.Append_Vector.
   --  EXAMPLE
   --    declare
   --       CD1 : Stack := Deck;
   --       --  Move 5 random cards from CD1 to CD2
   --       CD2 : Stack := CD1.Deal (5, Random);
   --    begin
   --       --  Append 10 more cards to CD2
   --       CD2.Append_Vector (CD1.Deal (10));
   --    end;
   --  SEE ALSO
   --    * Cards.Stack/Deal_All
   --  ****

   --  ****f* Cards.Stack/Deal_All
   --  SOURCE
   function Deal_All (Self : in out Stack;
                      From : Draw_From := Top) return Stack;
   function Deal_All (Self : in out Stack;
                      From : Draw_From := Top) return Card_List;
   --  FUNCTION
   --    Deal all cards from a stack of cards.
   --    Equivalent to "Deal (Self, Natural (Self.Length), From)"
   --  PARAMETERS
   --    Self   -- The stack of cards to deal from
   --    From   -- Deal from the Top, Bottom, or a Random position?
   --  RETURN VALUE
   --    Stack or Card_List -- The now-spent deck.
   --  EXAMPLE
--       CD1 : Stack := Deck;
--       --  Move all cards to CD1 to CD2 in random order.
--       CD2 : Stack := CD1.Deal_All (From => Random);
   --  SEE ALSO
   --    * Cards.Stack/Deal
   --  ****

   --  ****f* Cards.Stack/Create
   --  SOURCE
   function Create (From : Card_List) return Stack;
   --  FUNCTION
   --    Create a deck of cards based on a list of existing cards
   --  PARAMETERS
   --    From   -- Array of cards to create the stack from
   --  RETURN VALUE
   --    Stack containing cards in array "From"
   --  EXAMPLE
   --    --  create a stack of cards with an Ace, King, and Jack of spades
   --    CD : Stack := Create ([ (Rank => Ace, Suit => Spades),
   --                            (Rank => King, Suit => Spades),
   --                            (Rank => Jack, Suit => Spades)]);
   --  ****

   --  ****f* Cards.Stack/Deck
   --  SOURCE
   function Deck return Stack;
   --  FUNCTION
   --    Create a standard 52-card deck.
   --  RETURN VALUE
   --    Stack containing all 52-cards.
   --  EXAMPLE
   --    --  create a stack of the standard 52 deck
   --    CD : Stack := Deck;
   --  ****

   --  ****f* Cards.Stack/Multiply
   --  SOURCE
   function "*" (Left : Natural; Right : Stack) return Stack;
   --  FUNCTION
   --    Multiply a stack
   --  PARAMETERS
   --    Right   -- The stack of cards to multiply
   --    Left    -- The amount of times the stack should bu multiplied
   --  RETURN VALUE
   --    Stack containing the original stack duplicated "Left" times
   --  EXAMPLE
   --    --  create a stack of 3 decks
   --    CD1 : Stack := 3 * Deck;
   --  ****

private
   --  2d array containing symbols of the Suit. The first index is
   --  Cards.Colors and the second is Cards.Suit
   Suit_Symbols : constant array (Black_White, Suit) of Suit_Symbol :=
      [Black => [
         Hearts => Wide_Wide_Character'Val (16#2665#),   --  ♥
         Clubs => Wide_Wide_Character'Val (16#2663#),    --  ♣
         Diamonds => Wide_Wide_Character'Val (16#2666#), --  ♦
         Spades => Wide_Wide_Character'Val (16#2660#)],  --  ♠
       White => [
         Hearts => Wide_Wide_Character'Val (16#2661#),   --  ♡
         Clubs => Wide_Wide_Character'Val (16#2667#),    --  ♧
         Diamonds => Wide_Wide_Character'Val (16#2662#), --  ♢
         Spades => Wide_Wide_Character'Val (16#2664#)]]; --  ♤

   --  2d array containing symbols of the playing cards. The first index is
   --  Cards.Suit and the second is Cards.Colors.
   Card_Symbols : constant array (Suit, Rank) of Card_Symbol :=
      [Spades =>
         [Ace   => Wide_Wide_Character'Val (16#0001_F0A1#),  --  🂡
          Two   => Wide_Wide_Character'Val (16#0001_F0A2#),  --  🂢
          Three => Wide_Wide_Character'Val (16#0001_F0A3#),  --  🂣
          Four  => Wide_Wide_Character'Val (16#0001_F0A4#),  --  🂤
          Five  => Wide_Wide_Character'Val (16#0001_F0A5#),  --  🂥
          Six   => Wide_Wide_Character'Val (16#0001_F0A6#),  --  🂦
          Seven => Wide_Wide_Character'Val (16#0001_F0A7#),  --  🂧
          Eight => Wide_Wide_Character'Val (16#0001_F0A8#),  --  🂨
          Nine  => Wide_Wide_Character'Val (16#0001_F0A9#),  --  🂩
          Ten   => Wide_Wide_Character'Val (16#0001_F0AA#),  --  🂪
          Jack  => Wide_Wide_Character'Val (16#0001_F0AB#),  --  🂫
          Queen => Wide_Wide_Character'Val (16#0001_F0AD#),  --  🂭
          King  => Wide_Wide_Character'Val (16#0001_F0AE#)], --  🂮
       Hearts =>
         [Ace   => Wide_Wide_Character'Val (16#0001_F0B1#),  --  🂱`
          Two   => Wide_Wide_Character'Val (16#0001_F0B2#),  --  🂲
          Three => Wide_Wide_Character'Val (16#0001_F0B3#),  --  🂳
          Four  => Wide_Wide_Character'Val (16#0001_F0B4#),  --  🂴
          Five  => Wide_Wide_Character'Val (16#0001_F0B5#),  --  🂵
          Six   => Wide_Wide_Character'Val (16#0001_F0B6#),  --  🂶
          Seven => Wide_Wide_Character'Val (16#0001_F0B7#),  --  🂷
          Eight => Wide_Wide_Character'Val (16#0001_F0B8#),  --  🂸
          Nine  => Wide_Wide_Character'Val (16#0001_F0B9#),  --  🂹
          Ten   => Wide_Wide_Character'Val (16#0001_F0BA#),  --  🂺
          Jack  => Wide_Wide_Character'Val (16#0001_F0BB#),  --  🂻
          Queen => Wide_Wide_Character'Val (16#0001_F0BD#),  --  🂽
          King  => Wide_Wide_Character'Val (16#0001_F0BE#)], --  🂾
       Diamonds =>
         [Ace   => Wide_Wide_Character'Val (16#0001_F0C1#),  --  🃁
          Two   => Wide_Wide_Character'Val (16#0001_F0C2#),  --  🃂
          Three => Wide_Wide_Character'Val (16#0001_F0C3#),  --  🃃
          Four  => Wide_Wide_Character'Val (16#0001_F0C4#),  --  🃄
          Five  => Wide_Wide_Character'Val (16#0001_F0C5#),  --  🃅
          Six   => Wide_Wide_Character'Val (16#0001_F0C6#),  --  🃆
          Seven => Wide_Wide_Character'Val (16#0001_F0C7#),  --  🃇
          Eight => Wide_Wide_Character'Val (16#0001_F0C8#),  --  🃈
          Nine  => Wide_Wide_Character'Val (16#0001_F0C9#),  --  🃉
          Ten   => Wide_Wide_Character'Val (16#0001_F0CA#),  --  🃊
          Jack  => Wide_Wide_Character'Val (16#0001_F0CB#),  --  🃋
          Queen => Wide_Wide_Character'Val (16#0001_F0CD#),  --  🃍
          King  => Wide_Wide_Character'Val (16#0001_F0CE#)], --  🃎
      Clubs =>
         [Ace   => Wide_Wide_Character'Val (16#0001_F0D1#),   --  🃑
          Two   => Wide_Wide_Character'Val (16#0001_F0D2#),   --  🃒
          Three => Wide_Wide_Character'Val (16#0001_F0D3#),   --  🃓
          Four  => Wide_Wide_Character'Val (16#0001_F0D4#),   --  🃔
          Five  => Wide_Wide_Character'Val (16#0001_F0D5#),   --  🃕
          Six   => Wide_Wide_Character'Val (16#0001_F0D6#),   --  🃖
          Seven => Wide_Wide_Character'Val (16#0001_F0D7#),   --  🃗
          Eight => Wide_Wide_Character'Val (16#0001_F0D8#),   --  🃘
          Nine  => Wide_Wide_Character'Val (16#0001_F0D9#),   --  🃙
          Ten   => Wide_Wide_Character'Val (16#0001_F0DA#),   --  🃚
          Jack  => Wide_Wide_Character'Val (16#0001_F0DB#),   --  🃛
          Queen => Wide_Wide_Character'Val (16#0001_F0DD#),   --  🃝
          King  => Wide_Wide_Character'Val (16#0001_F0DE#)]]; --  🃞

   --  2d array containing the joker symbols
   Joker_Symbols : constant array (Black_White) of Card_Symbol :=
      [Black => Wide_Wide_Character'Val (16#0001_F0CF#), --  "🃏"
       White => Wide_Wide_Character'Val (16#0001F0DF#)]; --  "🃟"

end Cards;
