pragma Ada_2022;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Characters.Conversions;
with Ada.Numerics.Discrete_Random;

package body Cards is
   package Random_Gen is new Ada.Numerics.Discrete_Random (Positive);

   function Pop (Self : in out Stack) return Card;
   function Under (Self : in out Stack) return Card;
   procedure Push (Self : in out Stack; Item : Card);

   function Joker_Symbol_Char (Symbol_Color : Black_White) return Card_Symbol
      is (Joker_Symbols (Symbol_Color));
   function Joker_Symbol_Char return Card_Symbol is
      (Joker_Symbol_Char (Black));

   function Joker_Symbol (Symbol_Color : Black_White) return Wide_Wide_String
      is ("" & Joker_Symbols (Symbol_Color));
   function Joker_Symbol return Wide_Wide_String is (Joker_Symbol (Black));

   function Color (Item : Suit) return Colors is
      (case Item is
         when Hearts | Diamonds => Red,
         when Clubs | Spades => Black);

   function Symbol_Char (Item : Suit; Symbol_Color : Black_White := Black)
      return Suit_Symbol is (Suit_Symbols (Symbol_Color, Item));

   function Symbol (Item : Suit; Symbol_Color : Black_White := Black)
      return Wide_Wide_String is ("" & Symbol_Char (Item, Symbol_Color));

   function Abbr (Item : Suit) return String is
      (case Item is
         when Hearts   => "H",
         when Clubs    => "C",
         when Diamonds => "D",
         when Spades   => "S");

   function Abbr (Item : Rank) return String is
      use Ada.Strings.Fixed;
   begin
      return (case Item is
                  when Two .. Ten =>
                     Trim (Rank'Enum_Rep (Item)'Image,
                           Side => Ada.Strings.Both),
                  when Jack => "J",
                  when Queen => "Q",
                  when King => "K",
                  when Ace => "A");
   end Abbr;

   function Is_Face (Self : Card) return Boolean is (Self.Rank in Face);
   function Is_Pip (Self : Card) return Boolean is (Self.Rank in Pip);
   function Is_Ace (Self : Card) return Boolean is (Self.Rank = Ace);

   function Name_Symbol (Self : Card;
                         Symbol_Color : Black_White := Black;
                         Align : Boolean := False)
   return Wide_Wide_String is
      use Ada.Characters.Conversions;
   begin
      return To_Wide_Wide_String ((if Align and then Self.Rank /= Ten then
                                       Abbr (Self.Rank) & " "
                                    else
                                       Abbr (Self.Rank))) &
               Symbol (Self.Suit, Symbol_Color);
   end Name_Symbol;

   function As_Box (Self : Card;
       Symbol_Color : Black_White := Black; -- character color; default: Black
       Border : Wide_Wide_Character;
       Align : Boolean := False
   ) return Wide_Wide_String is
         (Border & Name_Symbol (Self, Symbol_Color, Align) & Border);

   function As_Box (Self : Card;
         Symbol_Color : Black_White := Black; -- character color; default: Black
         Border : Character := '|';
         Align : Boolean := False) return Wide_Wide_String
   is
      use Ada.Characters.Conversions;
   begin
      return Self.As_Box (Symbol_Color => Symbol_Color,
                          Border => To_Wide_Wide_Character (Border),
                          Align => Align);
   end As_Box;

   --  "Ace of Spades", "Four of Diamonds" or "4 of Diamonds"
   function Name (Self : Card;
                  Long_Format : Boolean := False)
   return String is
         use Ada.Characters.Handling;
      function To_Case (S : String) return String is
         (S (S'First) & To_Lower (S (S'First + 1 .. S'Last)))
         with Inline_Always;
   begin
      if Long_Format then
         return To_Case (Self.Rank'Image) & " of " & To_Case (Self.Suit'Image);
      else
         return Abbr (Self.Rank) & Abbr (Self.Suit);
      end if;
   end Name;

   function Value (Self : Card; Ace_Value : Positive := 1) return Positive
   is ((case Self.Rank is
         when Ace => Ace_Value,
         when Jack .. King => 10,
         when others => Rank'Enum_Rep (Self.Rank)));

   function Symbol_Char (Self : Card) return Card_Symbol is
      (Card_Symbols (Self.Suit, Self.Rank));

   function Symbol (Self : Card) return Wide_Wide_String is -- returns eg. "🂡"
      ("" & Symbol_Char ((Suit => Self.Suit, Rank => Self.Rank)));

   function To_List (Self : Stack) return Card_List is ([for C of Self => C]);

   procedure Shuffle (Self : in out Stack; Rounds : Positive := 1) is
      use Random_Gen;
      use Card_Vectors;
      I : Positive;
      G : Generator;
   begin
      if Self.Is_Empty then
         return;
      end if;
      for Round in 1 .. Rounds loop
         Reset (G);
         for X in reverse Self.First_Index .. Self.Last_Index loop
            I := (Random (G) mod X) + 1;
            Self.Swap (X, I);
         end loop;
      end loop;
   end Shuffle;

   function Shuffle (Self : Stack; Rounds : Positive := 1) return Stack is
      Result : Stack := Self;
   begin
      Result.Shuffle (Rounds);
      return Result;
   end Shuffle;

   function Deal (Self : in out Stack;
                  Amount : Natural := 1;
                  From : Draw_From := Top) return Stack
   is
      use Card_Vectors;
      Result : Stack;
   begin
      if Self.Is_Empty then
         return Result;
      end if;
      case From is
         when Top =>
            for X in 1 .. Amount loop
               exit when Self.Is_Empty;
               Result.Append (Pop (Self));
            end loop;
         when Bottom =>
            for X in 1 .. Amount loop
               exit when Self.Is_Empty;
               Result.Append (Under (Self));
            end loop;
         when Random =>
            declare
               use Random_Gen;
               I : Natural;
               G : Generator;
               C : Cursor;
            begin
               Reset (G);
               for X in 1 .. Amount loop
                  exit when Self.Is_Empty;
                  I := (Random (G) mod Natural (Self.Length)) + 1;
                  C := Self.To_Cursor (Extended_Index (I));
                  Result.Append (Element (C));
                  Self.Delete (C);
               end loop;
            end;
      end case;
      return Result;
   end Deal;

   function Deal (Self : in out Stack;
                  Amount : Natural := 1;
                  From : Draw_From  := Top) return Card_List
   is (To_List (Deal (Self, Amount, From)));

   function Deal_All (Self : in out Stack;
                      From : Draw_From := Top) return Stack is
   (Deal (Self, Natural (Self.Length), From));
   function Deal_All (Self : in out Stack;
                      From : Draw_From := Top) return Card_List is
   (Deal (Self, Natural (Self.Length), From));

   function "*" (Left : Natural; Right : Stack) return Stack is
      Result : Stack;
   begin
      for I in 1 .. Left loop
         Result := @ & Right;
      end loop;
      return Result;
   end "*";

   function Deck return Stack is
      Result : Stack;
   begin
      for S in Suit'Range loop
         for R in Rank'Range loop
            declare
               New_Card : constant Card := (Suit => S, Rank => R);
            begin
               Result.Append (New_Card);
            end;
         end loop;
      end loop;
      return Result;
   end Deck;

   --  Create a deck of Cards based on a list of existing cards
   function Create (From : Card_List) return Stack is
      Result : Stack;
   begin
      for C of From loop
         Result.Append (C);
      end loop;
      return Result;
   end Create;

   function Create (From : Card) return Stack is
      Result : Stack;
   begin
      Result.Append (From);
      return Result;
   end Create;

   function Count (Self : Stack; Which_Suit : Suit; Which_Rank : Rank) return Natural is
      Result : Natural := 0;
   begin
      for C of Self when C.Suit = Which_Suit and then C.Rank = Which_Rank loop
         Result := @ + 1;
      end loop;
      return Result;
   end Count;

   function Pop (Self : in out Stack) return Card
   is
      Result : Card;
   begin
      Result := Self.Last_Element;
      Self.Delete_Last;
      return Result;
   end Pop;

   function Under (Self : in out Stack) return Card
   is
      Result : Card;
   begin
      Result := Self.First_Element;
      Self.Delete_First;
      return Result;
   end Under;

   procedure Push (Self : in out Stack; Item : Card)
   is
   begin
      Self.Append (Item);
   end Push;

end Cards;
