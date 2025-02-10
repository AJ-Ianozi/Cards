pragma Ada_2022;
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Cards; use Cards;
with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers;
use type Ada.Containers.Count_Type;
procedure Tests is
   procedure Print (C : Stack) is
   begin
      Ada.Text_IO.New_Line;
      for A of C loop
         Ada.Text_IO.Put (A.Name (Long_Format => True) & " ");
         Ada.Text_IO.Put (A.Name & " ");
         Ada.Wide_Wide_Text_IO.Put (A.Symbol & " ");
         Ada.Wide_Wide_Text_IO.Put (A.Name_Symbol & " ");
         Ada.Wide_Wide_Text_IO.Put (A.As_Box & " ");
      end loop;
      Ada.Text_IO.New_Line;
   end Print;
   --  Create a card of every type
   C : constant array (Suit, Rank) of Card :=
      [for S in Suit'Range =>
         [for R in Rank'Range => (Suit => S, Rank => R)]];
begin

   --  Test symbols
   Assert ((for all S in Suit_Symbol =>
           S in '♥' | '♣' | '♦' | '♠' | '♡' | '♧' | '♢' | '♤'), "Suit_Symbol");

   Assert ((for all S in Card_Symbol =>
            S in
             '🂲' | '🂳' | '🂴' | '🂵' | '🂶' | '🂷' | '🂸' | '🂹' | '🂺' | '🂻' | '🂽' |
             '🂾' | '🂱' | '🃒' | '🃓' | '🃔' | '🃕' | '🃖' | '🃗' | '🃘' | '🃙' | '🃚' |
             '🃛' | '🃝' | '🃞' | '🃑' | '🃂' | '🃃' | '🃄' | '🃅' | '🃆' | '🃇' | '🃈' |
             '🃉' | '🃊' | '🃋' | '🃍' | '🃎' | '🃁' | '🂢' | '🂣' | '🂤' | '🂥' | '🂦' |
             '🂧' | '🂨' | '🂩' | '🂪' | '🂫' | '🂭' | '🂮' | '🂡' | '🃏' | '🃟' | '🂠'
            ), "Card_Symbol");

   --  Test suits
   Assert (Color (Hearts)   = Red, "suite colors");
   Assert (Color (Diamonds) = Red, "suite colors");
   Assert (Color (Clubs)    = Black, "suite colors");
   Assert (Color (Spades)   = Black, "suite colors");

   --  Suit symbols
   Assert (Symbol_Char (Hearts)          = '♥', "suit symbol funcs");
   Assert (Symbol_Char (Hearts, Black)   = '♥', "suit symbol funcs");
   Assert (Symbol_Char (Clubs, Black)    = '♣', "suit symbol funcs");
   Assert (Symbol_Char (Diamonds, Black) = '♦', "suit symbol funcs");
   Assert (Symbol_Char (Spades, Black)   = '♠', "suit symbol funcs");
   Assert (Symbol_Char (Hearts, White)   = '♡', "suit symbol funcs");
   Assert (Symbol_Char (Clubs, White)    = '♧', "suit symbol funcs");
   Assert (Symbol_Char (Diamonds, White) = '♢', "suit symbol funcs");
   Assert (Symbol_Char (Spades, White)   = '♤', "suit symbol funcs");
   Assert (Symbol (Hearts)          = "♥", "suit symbol func strs");
   Assert (Symbol (Hearts, Black)   = "♥", "suit symbol func strs");
   Assert (Symbol (Clubs, Black)    = "♣", "suit symbol func strs");
   Assert (Symbol (Diamonds, Black) = "♦", "suit symbol func strs");
   Assert (Symbol (Spades, Black)   = "♠", "suit symbol func strs");
   Assert (Symbol (Hearts, White)   = "♡", "suit symbol func strs");
   Assert (Symbol (Clubs, White)    = "♧", "suit symbol func strs");
   Assert (Symbol (Diamonds, White) = "♢", "suit symbol func strs");
   Assert (Symbol (Spades, White)   = "♤", "suit symbol func strs");

   --  Blank card symbols
   Assert (Blank_Card = '🂠', "Blank Card");

   --  Joker symbols
   Assert (Joker_Symbol (Black) = "🃏", "Black Joker symbol");
   Assert (Joker_Symbol (White) = "🃟", "White Joker symbol");

   --  Abbr for suit
   Assert (Abbr (Hearts)   = "H", "suit abbrs");
   Assert (Abbr (Clubs)    = "C", "suit abbrs");
   Assert (Abbr (Diamonds) = "D", "suit abbrs");
   Assert (Abbr (Spades)   = "S", "suit abbrs");

   --  Abbr for rank
   Assert (Abbr (Two)   = "2", "Rank abbr");
   Assert (Abbr (Three) = "3", "Rank abbr");
   Assert (Abbr (Four)  = "4", "Rank abbr");
   Assert (Abbr (Five)  = "5", "Rank abbr");
   Assert (Abbr (Six)   = "6", "Rank abbr");
   Assert (Abbr (Seven) = "7", "Rank abbr");
   Assert (Abbr (Eight) = "8", "Rank abbr");
   Assert (Abbr (Nine)  = "9", "Rank abbr");
   Assert (Abbr (Ten)   = "10", "Rank abbr");
   Assert (Abbr (Jack)  = "J", "Rank abbr");
   Assert (Abbr (Queen) = "Q", "Rank abbr");
   Assert (Abbr (King)  = "K", "Rank abbr");
   Assert (Abbr (Ace)   = "A", "Rank abbr");

   --  Cards

   --  Name_Symbol
   Assert ((for all X of C => X.Name_Symbol =
            To_Wide_Wide_String (Abbr (X.Rank)) & Symbol (X.Suit)) and then
            (for all X of C => X.Name_Symbol (Symbol_Color => Black) =
               To_Wide_Wide_String (Abbr (X.Rank)) & Symbol (X.Suit, Black))
          and then
            (for all X of C => X.Name_Symbol (Symbol_Color => White) =
               To_Wide_Wide_String (Abbr (X.Rank)) & Symbol (X.Suit, White)),
           "Card name_symbol");
   --  As box
   Assert ((for all X of C => X.As_Box = "|" &
            To_Wide_Wide_String (Abbr (X.Rank)) & Symbol (X.Suit) & "|")
           and then
            (for all X of C => X.As_Box (Symbol_Color => Black) =
             "|" & To_Wide_Wide_String (Abbr (X.Rank)) &
              Symbol (X.Suit, Black) & "|")
            and then
             (for all X of C => X.As_Box (Symbol_Color => White) = "|" &
              To_Wide_Wide_String (Abbr (X.Rank)) &
              Symbol (X.Suit, White) & "|"),
           "Card as_box");

   --  Short name
   Assert ((for all X of C => X.Name = Abbr (X.Rank) & Abbr (X.Suit)) and then
            (for all X of C => X.Name (Long_Format => False) =
               Abbr (X.Rank) & Abbr (X.Suit)), "Card short name");

   --  Long name
   --  Hearts
   Assert (Name (C (Hearts, Two), Long_Format => True)   = "Two of Hearts",
            "Card long name, 2 hearts");
   Assert (Name (C (Hearts, Three), Long_Format => True) = "Three of Hearts",
            "Card long name, 3 hearts");
   Assert (Name (C (Hearts, Four), Long_Format => True)  = "Four of Hearts",
            "Card long name, 4 hearts");
   Assert (Name (C (Hearts, Five), Long_Format => True)  = "Five of Hearts",
            "Card long name, 5 hearts");
   Assert (Name (C (Hearts, Six), Long_Format => True)   = "Six of Hearts",
            "Card long name, 6 hearts");
   Assert (Name (C (Hearts, Seven), Long_Format => True) = "Seven of Hearts",
            "Card long name, 7 hearts");
   Assert (Name (C (Hearts, Eight), Long_Format => True) = "Eight of Hearts",
            "Card long name, 8 hearts");
   Assert (Name (C (Hearts, Nine), Long_Format => True)  = "Nine of Hearts",
            "Card long name, 9 hearts");
   Assert (Name (C (Hearts, Ten), Long_Format => True)   = "Ten of Hearts",
            "Card long name, 10 hearts");
   Assert (Name (C (Hearts, Jack), Long_Format => True)  = "Jack of Hearts",
            "Card long name, j hearts");
   Assert (Name (C (Hearts, Queen), Long_Format => True) = "Queen of Hearts",
            "Card long name, q hearts");
   Assert (Name (C (Hearts, King), Long_Format => True)  = "King of Hearts",
            "Card long name, k hearts");
   Assert (Name (C (Hearts, Ace), Long_Format => True)   = "Ace of Hearts",
            "Card long name, a hearts");
   --  Clubs
   Assert (Name (C (Clubs, Two), Long_Format => True)   = "Two of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, Three), Long_Format => True) = "Three of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, Four), Long_Format => True)  = "Four of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, Five), Long_Format => True)  = "Five of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, Six), Long_Format => True)   = "Six of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, Seven), Long_Format => True) = "Seven of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, Eight), Long_Format => True) = "Eight of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, Nine), Long_Format => True)  = "Nine of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, Ten), Long_Format => True)   = "Ten of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, Jack), Long_Format => True)  = "Jack of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, Queen), Long_Format => True) = "Queen of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, King), Long_Format => True)  = "King of Clubs",
            "Card long name, clubs");
   Assert (Name (C (Clubs, Ace), Long_Format => True)   = "Ace of Clubs",
            "Card long name, clubs");
   --  Diamonds
   Assert (Name (C (Diamonds, Two), Long_Format => True)   =
            "Two of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, Three), Long_Format => True) =
            "Three of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, Four), Long_Format => True)  =
            "Four of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, Five), Long_Format => True)  =
            "Five of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, Six), Long_Format => True)   =
            "Six of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, Seven), Long_Format => True) =
            "Seven of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, Eight), Long_Format => True) =
            "Eight of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, Nine), Long_Format => True)  =
            "Nine of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, Ten), Long_Format => True)   =
            "Ten of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, Jack), Long_Format => True)  =
            "Jack of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, Queen), Long_Format => True) =
            "Queen of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, King), Long_Format => True)  =
            "King of Diamonds", "Card long name, diamonds");
   Assert (Name (C (Diamonds, Ace), Long_Format => True)   =
            "Ace of Diamonds", "Card long name, diamonds");
   --  Spades
   Assert (Name (C (Spades, Two), Long_Format => True)   = "Two of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, Three), Long_Format => True) = "Three of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, Four), Long_Format => True)  = "Four of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, Five), Long_Format => True)  = "Five of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, Six), Long_Format => True)   = "Six of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, Seven), Long_Format => True) = "Seven of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, Eight), Long_Format => True) = "Eight of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, Nine), Long_Format => True)  = "Nine of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, Ten), Long_Format => True)   = "Ten of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, Jack), Long_Format => True)  = "Jack of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, Queen), Long_Format => True) = "Queen of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, King), Long_Format => True)  = "King of Spades",
            "card long name, spades");
   Assert (Name (C (Spades, Ace), Long_Format => True)   = "Ace of Spades",
            "card long name, spades");

   --  Hearts
   Assert (Symbol (C (Hearts, Two))   = "🂲", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, Three)) = "🂳", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, Four))  = "🂴", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, Five))  = "🂵", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, Six))   = "🂶", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, Seven)) = "🂷", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, Eight)) = "🂸", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, Nine))  = "🂹", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, Ten))   = "🂺", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, Jack))  = "🂻", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, Queen)) = "🂽", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, King))  = "🂾", "Card symbols, hearts");
   Assert (Symbol (C (Hearts, Ace))   = "🂱", "Card symbols, hearts");
   --  Clubs
   Assert (Symbol (C (Clubs, Two))   = "🃒", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, Three)) = "🃓", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, Four))  = "🃔", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, Five))  = "🃕", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, Six))   = "🃖", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, Seven)) = "🃗", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, Eight)) = "🃘", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, Nine))  = "🃙", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, Ten))   = "🃚", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, Jack))  = "🃛", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, Queen)) = "🃝", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, King))  = "🃞", "Card symbols, clubs");
   Assert (Symbol (C (Clubs, Ace))   = "🃑", "Card symbols, clubs");
   --  Diamonds
   Assert (Symbol (C (Diamonds, Two))   = "🃂", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, Three)) = "🃃", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, Four))  = "🃄", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, Five))  = "🃅", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, Six))   = "🃆", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, Seven)) = "🃇", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, Eight)) = "🃈", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, Nine))  = "🃉", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, Ten))   = "🃊", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, Jack))  = "🃋", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, Queen)) = "🃍", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, King))  = "🃎", "Card symbols, diamonds");
   Assert (Symbol (C (Diamonds, Ace))   = "🃁", "Card symbols, diamonds");
   --  Spades
   Assert (Symbol (C (Spades, Two))   = "🂢", "card symbols, spades");
   Assert (Symbol (C (Spades, Three)) = "🂣", "card symbols, spades");
   Assert (Symbol (C (Spades, Four))  = "🂤", "card symbols, spades");
   Assert (Symbol (C (Spades, Five))  = "🂥", "card symbols, spades");
   Assert (Symbol (C (Spades, Six))   = "🂦", "card symbols, spades");
   Assert (Symbol (C (Spades, Seven)) = "🂧", "card symbols, spades");
   Assert (Symbol (C (Spades, Eight)) = "🂨", "card symbols, spades");
   Assert (Symbol (C (Spades, Nine))  = "🂩", "card symbols, spades");
   Assert (Symbol (C (Spades, Ten))   = "🂪", "card symbols, spades");
   Assert (Symbol (C (Spades, Jack))  = "🂫", "card symbols, spades");
   Assert (Symbol (C (Spades, Queen)) = "🂭", "card symbols, spades");
   Assert (Symbol (C (Spades, King))  = "🂮", "card symbols, spades");
   Assert (Symbol (C (Spades, Ace))   = "🂡", "card symbols, spades");

   ----------------------------------------------------------------------------
   ---------------------------- Test the cards --------------------------------
   ----------------------------------------------------------------------------

   --  Test the decks
   declare
      Ref_Deck : constant Stack := Deck;
      Ref_List : constant Card_List := Ref_Deck.To_List;
      Test_Deck : Stack := Deck;
      Multiply_By : constant Positive := 3;
   begin
      --  Test card list and test deck
      Assert (Ref_Deck.Length = 52, "Deck length");
      Assert (Ref_List'Length = 52, "List length");
      Assert ((for all S of Ref_List => Ref_Deck.Contains (S)), "List not eq");
      Assert (Ref_Deck = Test_Deck, "Two decks not eq");
      --  Test decks
      Ada.Text_IO.Put_Line ("Ref deck:");
      Print (Ref_Deck);
      Ada.Text_IO.Put_Line ("Shuffling now:");
      Test_Deck.Shuffle;
      Assert (Test_Deck.Length = 52, "Shuffle broke Deck length");
      Assert (Ref_Deck /= Test_Deck, "Shuffle did nothing");
      Print (Test_Deck);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
      --  Test Count
      Test_Deck := Create ([C (Spades, Ace),
                           C (Spades, Ace),
                           C (Hearts, King)]);
      Assert (Test_Deck.Count (Which_Suit => Spades, Which_Rank => Ace) = 2,
               "Count");
      Assert (Test_Deck.Count (Which_Suit => Hearts, Which_Rank => Ace) = 0,
               "Count");
      --  Test multiplication
      Test_Deck := Multiply_By * Ref_Deck;
      Assert (Natural (Test_Deck.Length) =
               Natural (Ref_Deck.Length) * Multiply_By,
               "Deck length off");
      Assert ((for all C of Test_Deck =>
               Test_Deck.Count (C.Suit, C.Rank) = Multiply_By),
               "Cards not multiplied");
      --  Test dealing
      declare
         Test_Deck2, Test_Deck3 : Stack;
      begin
         Ada.Text_IO.Put_Line ("Draw 3 from top:");
         Test_Deck := Ref_Deck;
         Test_Deck2 := Test_Deck.Deal (Amount => 3, From => Top);
         Print (Test_Deck2);
         Assert ((for all S of Test_Deck2 => not Test_Deck.Contains (S))
                  and then not Test_Deck.Is_Empty,
                  "Card not removed: top");
         Assert (Test_Deck2.To_List = [C (Spades, Ace),
                                     C (Spades, King),
                                     C (Spades, Queen)], "Draw from Top");
         Ada.Text_IO.Put_Line ("Draw 3 from bottom:");
         Test_Deck := Ref_Deck;
         Test_Deck2 := Test_Deck.Deal (Amount => 3, From => Bottom);
         Print (Test_Deck2);
         Assert ((for all S of Test_Deck2 => not Test_Deck.Contains (S))
                  and then not Test_Deck.Is_Empty,
                  "Card not removed: top");
         Assert (Test_Deck2.To_List = [C (Hearts, Two),
                                     C (Hearts, Three),
                                     C (Hearts, Four)], "Draw from Bottom");
         Ada.Text_IO.Put_Line ("Draw 3 random");
         Test_Deck := Ref_Deck;
         Test_Deck2 := Test_Deck.Deal (Amount => 3, From => Random);
         Print (Test_Deck2);
         Assert ((for all S of Test_Deck2 => not Test_Deck.Contains (S))
                  and then not Test_Deck.Is_Empty,
                  "Card not removed: random");
         Ada.Text_IO.Put_Line ("Draw all from bottom:");
         Test_Deck := Ref_Deck;
         Test_Deck2 := Test_Deck.Deal (Amount => Natural (Test_Deck.Length),
                                       From => Bottom);
         Print (Test_Deck2);
         Assert ((for all S of Test_Deck2 => not Test_Deck.Contains (S))
                  and then Test_Deck.Is_Empty,
                  "Not removed all");
         Ada.Text_IO.Put_Line ("Draw_All from bottom:");
         Test_Deck := Ref_Deck;
         Test_Deck3 := Test_Deck.Deal_All (From => Bottom);
         Print (Test_Deck3);
         Ada.Text_IO.New_Line;
         Assert ((for all S of Test_Deck3 => not Test_Deck.Contains (S))
                  and then Test_Deck.Is_Empty,
                  "Not removed all");
         Assert (Test_Deck2 = Test_Deck3, "Draw_all");
         Ada.Text_IO.Put_Line ("Draw_All from random:");
         Test_Deck := Ref_Deck;
         Test_Deck2 := Test_Deck.Deal_All (From => Random);
         Assert ((for all S of Test_Deck2 => not Test_Deck.Contains (S))
                  and then Test_Deck.Is_Empty,
                  "Not removed all");
         Print (Test_Deck2);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Draw none from top");
         Test_Deck := Ref_Deck;
         Test_Deck2 := Test_Deck.Deal (Amount => 0, From => Top);
         Assert (Test_Deck = Ref_Deck);
         Assert (Test_Deck2.Length = 0);
         Print (Test_Deck2);

      end;
   end;
end Tests;
