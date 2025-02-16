-------------------------------------------------------------------------------
--     Klondike utilizing the Cards library
-------------------------------------------------------------------------------
--  Copyright (c) 2025 AJ Ianozi
--  Licensed under the MIT License.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Wide_Wide_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers.Vectors;
with Cards;
procedure Klondike is
   use type Cards.Suit;
   use type Cards.Rank;
   use type Cards.Colors;

   --  Muhahahaha
   NO          : constant Character := Ada.Characters.Latin_1.BEL;

   Header        : constant String :=
      "AJ's Klondike. Q: Quit | R: Restart | U: Undo";
   Blank_Card    : constant String := "|XXX|";
   Empty_Slot    : constant String := "| o |";
   Empty_Card    : constant String := "     ";
   Cursor_Up     : constant String := "  ^  ";

   --  This is what the card is surrounded by
   Selected_Card : constant Character := 'I';

   --  A "slot" holds stacks of cards that may be facing down
   type Slot is record
      S : Cards.Stack;
      --  Where in the stack do cards face down?
      Facing_Down : Natural := 0;
   end record;
   type Slots is array (Positive range <>) of Slot;
   subtype Foundation_Range is Positive range 1 .. 4;
   subtype Tableau_Range is Positive range 1 .. 7;

   --  There are much better ways to do this, but this is the quickest way
   --  I can think of to just drop it in while drawing everything out in a tty
   type Game_Zone is (Stock_Zone, Waste_Zone, Foundation_Zone, Tab_Zone, NA);
   type Specific_Zone is record
      Zone : Game_Zone := NA;
      F_Idx : Foundation_Range := Foundation_Range'First;
      T_Idx : Tableau_Range := Tableau_Range'First;
   end record;

   --  Holds the current "State" of the game
   type Game_State is record
      --  Game's withdraw deck
      Stock : Cards.Stack;
      --  Where the Aces go
      Foundation : Slots (Foundation_Range);
      --  Playing table
      Tableau : Slots (Tableau_Range);
      --  Discard pile
      Waste : Cards.Stack;
      --  The current cursor
      Cursor : Specific_Zone;
      --  For storing cards which are selected
      Selected_Cards : Specific_Zone;
   end record;

   type Choice is (Left, Right, Up, Down, Action, Quit, Restart, Undo, NA);
   subtype Direction is Choice range Left .. Down;

   package Backups is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Game_State);

   function Init return Game_State is
   begin
      return Result : Game_State do
         Result.Stock := Cards.Deck.Shuffle (2);
         Result.Tableau := [for I in Tableau_Range =>
                            (S => Result.Stock.Deal (I),
                            Facing_Down => (I - 1))];
         Result.Waste := Result.Stock.Deal;
         Result.Cursor.Zone := Stock_Zone;
      end return;
   end Init;

   --  ------------------------------------------------------------------------
   --                          Keyboard
   --  ------------------------------------------------------------------------
   function Get_Input return Choice is
      function Get_Character return Character is
         Result : Character;
      begin
         Ada.Text_IO.Get_Immediate (Result);
         return Result;
      end Get_Character;
      Result  : constant Choice :=
                  (case Get_Character is
                     when Ada.Characters.Latin_1.ESC => --  ANSI
                                 (case Get_Character is
                                    when '[' => (case Get_Character is
                                                   when 'A' => Up,
                                                   when 'B' => Down,
                                                   when 'C' => Right,
                                                   when 'D' => Left,
                                                   when others => NA),
                                    when others => NA),
                     when Character'Val (224) => --  Windows
                                 (case Get_Character is
                                    when Character'Val (72) => Up,
                                    when Character'Val (75) => Left,
                                    when Character'Val (77) => Right,
                                    when Character'Val (80) => Down,
                                    when others => NA),
                     when ' ' | Ada.Characters.Latin_1.LF => Action,
                     when 'Q' | 'q' => Quit,
                     when 'R' | 'r' => Restart,
                     when 'U' | 'u' => Undo,
                     when others => NA);
   begin
      return Result;
   end Get_Input;

   --  ------------------------------------------------------------------------
   --                               UI
   --  ------------------------------------------------------------------------

   --  Move the cursor around
   function Move_Cursor (G : in out Game_State; D : Direction) return Boolean
   is
      Result : Boolean := False;
   begin
      case D is
         when Up =>
            --  Can only move up if cursor is in Tableau area
            if G.Cursor.Zone = Tab_Zone then
               case G.Cursor.T_Idx is
                  when 1 =>
                     G.Cursor.Zone := Stock_Zone;
                  when 2 .. 3 =>
                     G.Cursor.Zone := Waste_Zone;
                  when 4 .. 7 =>
                     G.Cursor.Zone  := Foundation_Zone;
                     G.Cursor.F_Idx := Foundation_Range (G.Cursor.T_Idx - 3);
               end case;
               Result := True;
            end if;

         when Down =>
            --  Can only move down if in top row
            case G.Cursor.Zone is
               when Stock_Zone =>
                  G.Cursor.Zone := Tab_Zone;
                  G.Cursor.T_Idx := 1;
                  Result := True;
               when Waste_Zone =>
                  G.Cursor.Zone := Tab_Zone;
                  G.Cursor.T_Idx := 2;
                  Result := True;
               when Foundation_Zone =>
                  G.Cursor.Zone := Tab_Zone;
                  G.Cursor.T_Idx := Tableau_Range (G.Cursor.F_Idx + 3);
                  Result := True;
               when others => null;
            end case;

         when Left =>
            case G.Cursor.Zone is
               when Waste_Zone =>
                  G.Cursor.Zone := Stock_Zone;
                  Result := True;
               when Foundation_Zone =>
                  if G.Cursor.F_Idx = Foundation_Range'First then
                     G.Cursor.Zone := Waste_Zone;
                  else
                     G.Cursor.F_Idx := @ - 1;
                  end if;
                  Result := True;
               when Tab_Zone =>
                  if G.Cursor.T_Idx > Tableau_Range'First then
                     G.Cursor.T_Idx := @ - 1;
                     Result := True;
                  end if;
               when others => null;
            end case;

         when Right =>
            case G.Cursor.Zone is
               when Stock_Zone =>
                  G.Cursor.Zone := Waste_Zone;
                  Result := True;
               when Waste_Zone =>
                  G.Cursor.Zone := Foundation_Zone;
                  G.Cursor.F_Idx := Foundation_Range'First;
                  Result := True;
               when Foundation_Zone =>
                  if G.Cursor.F_Idx < Foundation_Range'Last then
                     G.Cursor.F_Idx := @ + 1;
                     Result := True;
                  end if;
               when Tab_Zone =>
                  if G.Cursor.T_Idx < Tableau_Range'Last then
                     G.Cursor.T_Idx := @ + 1;
                     Result := True;
                  end if;
               when others => null;
            end case;
      end case;
      return Result;
   end Move_Cursor;
   --  Select whatever the cursor is pointing at
   procedure Select_Zone (G : in out Game_State) is
   begin
      G.Selected_Cards.Zone := G.Cursor.Zone;
      case G.Cursor.Zone is
         when Foundation_Zone => G.Selected_Cards.F_Idx := G.Cursor.F_Idx;
         when Tab_Zone        => G.Selected_Cards.T_Idx := G.Cursor.T_Idx;
         when others => null;
      end case;
   end Select_Zone;
   function Longest_Row (G : Game_State) return Positive is
      Result : Positive := 1;
   begin
      for X of G.Tableau loop
         if Natural (X.S.Length) > Result then
            Result := Positive (X.S.Length);
         end if;
      end loop;
      return Result;
   end Longest_Row;
   function Draw_Card (Which : Cards.Card; Border : Character := '|')
   return Wide_Wide_String is
    (Which.As_Box (Symbol_Color =>
                     (if Cards.Color (Which.Suit) = Cards.Red then
                        Cards.White else Cards.Black),
                  Align => True,
                  Border => Border));
   function Draw_Top (S : Cards.Stack; Border : Character := '|')
      return Wide_Wide_String is
   begin
      return (if S.Is_Empty then To_Wide_Wide_String (Empty_Slot)
               else Draw_Card (S.Last_Element, Border));
   end Draw_Top;
   procedure Draw_Game (G : Game_State) is
      procedure Clear_Screen is
      begin
         --  Clear the screen
         Ada.Text_IO.Put (Ada.Characters.Latin_1.ESC & "[H" &
                        Ada.Characters.Latin_1.ESC & "[J");
      end Clear_Screen;
   begin
      Clear_Screen;
      Put_Line (Header);
      --  Draw the Stock and Waste pile
      Put (if G.Stock.Is_Empty then Empty_Slot else Blank_Card);
      Put (" ");
      Ada.Wide_Wide_Text_IO.Put
         (Draw_Top (G.Waste, Border =>
                     (if G.Selected_Cards.Zone = Waste_Zone then
                        Selected_Card
                     else '|')));
      Put ("       ");
      --  Draw the foundation stacks (where As start)
      for I in Foundation_Range loop
         Ada.Wide_Wide_Text_IO.Put
            (Draw_Top (G.Foundation (I).S,
             Border => (if G.Selected_Cards.Zone = Foundation_Zone and then
                              G.Selected_Cards.F_Idx = I
                         then Selected_Card
                         else '|')));
         Put (" ");
      end loop;
      New_Line;
      --  Determine if the cursor should go on this row or not
      case G.Cursor.Zone is
         when Stock_Zone =>
            Put (Cursor_Up);
         when Waste_Zone =>
            Put (Empty_Card & " " & Cursor_Up);
         when Foundation_Zone =>
            for I in 1 .. G.Cursor.F_Idx + 2 loop
               Put (Empty_Card & " ");
            end loop;
            Put (Cursor_Up);
         when others => null;
      end case;
      New_Line;
      --  Draw the Tableau. We have to draw row-by-row, so keep track of the
      --  index.
      for I in 1 .. Longest_Row (G) + 1 loop
         for Col in Tableau_Range loop
            --  Determine if this slot is visible, and draw accordingly
            if Natural (G.Tableau (Col).S.Length) >= I then
               if I <= G.Tableau (Col).Facing_Down then
                  Put (Blank_Card);
               else
                  declare
                     --  Calculate if current card is selected or not
                     Border : constant Character :=
                              (if G.Selected_Cards.Zone = Tab_Zone and then
                                 G.Selected_Cards.T_Idx = Col
                              then Selected_Card
                              else '|');
                  begin
                     Ada.Wide_Wide_Text_IO.Put
                        (Draw_Card (G.Tableau (Col).S (I), Border));
                  end;
               end if;
            elsif I = 1 then
               if  G.Tableau (Col).S.Is_Empty then
                  Put (Empty_Slot);
               else
                  Put (Empty_Card);
               end if;
            elsif --  Should we instead draw the curser under the card?
               (G.Cursor.Zone = Tab_Zone and then
                G.Cursor.T_Idx = Col) and then
               ((I = Natural (G.Tableau (Col).S.Length) + 1) or else
               (G.Tableau (Col).S.Is_Empty and then I = 2))
            then
               Put (Cursor_Up);
            else
               Put (Empty_Card);
            end if;
            Put (' ');
         end loop;
         New_Line;
      end loop;
   end Draw_Game;

   --  ------------------------------------------------------------------------
   --                            Game Logic
   --  ------------------------------------------------------------------------
   --  Gets a card out of the specific zone
   function Get_Card (G     : Game_State;
                      Which : Specific_Zone;
                      C     : out Cards.Card)
      return Boolean
   is
      Result : Boolean := False;
   begin
      case Which.Zone is
         when Waste_Zone =>
            if not G.Waste.Is_Empty then
               C := G.Waste.Last_Element;
               Result := True;
            end if;
         when Tab_Zone =>
            if not G.Tableau (Which.T_Idx).S.Is_Empty then
               C := G.Tableau (Which.T_Idx).S.Last_Element;
               Result := True;
            end if;
         when Foundation_Zone =>
            if not G.Foundation (Which.F_Idx).S.Is_Empty then
               C := G.Foundation (Which.F_Idx).S.Last_Element;
               Result := True;
            end if;
         when others => null;
      end case;
      return Result;
   end Get_Card;
   --  Returns the index of the available foundation slot that cursor points to
   function In_Foundation (G : Game_State)
      return Natural is
      Check : Cards.Card;
      No_Result : constant Natural := 0;
   begin
      --  Get the card we need to check
      case G.Cursor.Zone is
         when Waste_Zone | Tab_Zone =>
            if not Get_Card (G, G.Cursor, Check) then
               return No_Result;
            end if;
         when others =>
            return No_Result;
      end case;

      --  Check the foundation stacks against the card
      for I in G.Foundation'Range loop
         --  Empty stack paired with an Ace
         if G.Foundation (I).S.Is_Empty then
            if Check.Rank = Cards.Ace then
               return I;
            end if;
         elsif Check.Suit = G.Foundation (I).S.Last_Element.Suit then
            declare
               F_Rank : constant Cards.Rank :=
                           G.Foundation (I).S.Last_Element.Rank;
            begin
               --  Two and Ace are disjointed, so just check them manually
               if F_Rank = Cards.Ace then
                  if Check.Rank = Cards.Two then
                     return I;
                  end if;
               --  Card's rank is the next rank
               elsif Cards.Rank'Succ (F_Rank) = Check.Rank then
                  return I;
               end if;
            end;
         end if;
      end loop;

      return No_Result;
   end In_Foundation;

   --  Send whatever the cursor is pointing at to Foundation
   function Send_To_Foundation (G : in out Game_State)
   return Boolean is
      F_Idx            : constant Natural := In_Foundation (G);
      Is_In_Foundation : constant Boolean := F_Idx > 0;
   begin
      if Is_In_Foundation then
         case G.Cursor.Zone is
            when Waste_Zone =>
               G.Foundation (F_Idx).S.Append_Vector (G.Waste.Deal);
            when Tab_Zone => G.Foundation (F_Idx).S.Append_Vector
                                    (G.Tableau (G.Cursor.T_Idx).S.Deal);
            when Foundation_Zone => G.Foundation (F_Idx).S.Append_Vector
                                     (G.Foundation (G.Cursor.F_Idx).S.Deal);
            when others => return False;
         end case;
      end if;
      return Is_In_Foundation;
   end Send_To_Foundation;
   --  Validate if a card can be moved or not.
   function Can_Move (From : Cards.Card; To : Cards.Card) return Boolean is
      --  Must use alternate colors
      (Cards.Color (From.Suit) /= Cards.Color (To.Suit) and then
         --  Two and Ace are disjointed, so just check them manually
         ((From.Rank = Cards.Ace and then To.Rank = Cards.Two)
       or else
         --  From must not be a king
            (From.Rank /= Cards.King and then
            --  To card must not be an Ace
            To.Rank /= Cards.Ace and then
            Cards.Rank'Pred (To.Rank) = From.Rank)));

   --  Move the selected card to the cursor.
   function Move_Card (G : in out Game_State) return Boolean is
      From : Cards.Card;
      To   : Cards.Card;
      --  Get this ahead of time, knowing it may fail
      Have_To : constant Boolean := Get_Card (G, G.Cursor, To);
   begin
      --  Can only move cards within the tab zone
      if G.Cursor.Zone /= Tab_Zone then
         return False;
      end if;
      --  The logic is different depending on if we're just moving from the
      --  waste or foundation pile to the tab
      case G.Selected_Cards.Zone is
         when Waste_Zone | Foundation_Zone =>
            --  Set From to the selected. If nothing is there, leave.
            if not Get_Card (G, G.Selected_Cards, From) then
               return False;
            end if;

            --  If "Have_To suceeds, we can check.  If if failed it may be
            --   empty space so we can stil move it if From is a king
            if
               (Have_To and then Can_Move (From, To))
               or else
               (G.Tableau (G.Cursor.T_Idx).S.Is_Empty and then
                  From.Rank = Cards.King)
            then
               --  Move the card "Selected" to "Cursor"
               G.Tableau (G.Cursor.T_Idx).S.Append_Vector
                  (if G.Selected_Cards.Zone = Waste_Zone then
                     G.Waste.Deal
                  else
                     G.Foundation (G.Selected_Cards.F_Idx).S.Deal);
               return True;
            end if;

         when Tab_Zone =>
            --  We need to check the entire stack against another stack.
            --  Work backwards through the vector until we either find a
            --  card that matches or until we hit cards that we cannot see
            for I in reverse
               G.Tableau (G.Selected_Cards.T_Idx).S.First_Index ..
               G.Tableau (G.Selected_Cards.T_Idx).S.Last_Index
            loop
               exit when I = G.Tableau (G.Selected_Cards.T_Idx).Facing_Down;

               --  Move the stack at this given index
               From := G.Tableau (G.Selected_Cards.T_Idx).S (I);
               if
                  (Have_To and then Can_Move (From, To)) or else
                   (G.Tableau (G.Cursor.T_Idx).S.Is_Empty and then
                    From.Rank = Cards.King)
               then
                  declare
                     --  Doing this to reverse the stack
                     Tmp : Cards.Stack :=
                           G.Tableau (G.Selected_Cards.T_Idx).S.Deal
                           (1 + Natural (G.Tableau
                           (G.Selected_Cards.T_Idx).S.Length) - Natural (I));
                  begin
                     --  Move selected stack to cursor and exit
                     G.Tableau (G.Cursor.T_Idx).S.Append_Vector (Tmp.Deal_All);
                  end;
                  return True;
               end if;
            end loop;
         when others =>
            return False;
      end case;
      return False;
   end Move_Card;
   function Process_Actions (G : in out Game_State; Moved : out Boolean)
      return  Boolean
   is
      Result : Boolean := True;
   begin
      Moved := False;
      case G.Cursor.Zone is
         --  Draw next card
         when Stock_Zone =>
            if G.Selected_Cards.Zone = NA then
               --  Are there no cards left?
               if G.Stock.Is_Empty then
                  --  Re-deal
                  G.Stock.Append_Vector (G.Waste.Deal_All);
               else
                  --  Draw next card
                  G.Waste.Append_Vector (G.Stock.Deal);
               end if;
               Moved := True;
            else
               G.Selected_Cards.Zone := NA;
            end if;
         --  Selected a card
         when Waste_Zone | Foundation_Zone | Tab_Zone =>
            --  For all three of these, we try moving the card first.
            if G.Selected_Cards.Zone = NA then
               --  If not sending to foundation, then select the card.
               if Send_To_Foundation (G) then
                  Moved := True;
               else
                  Select_Zone (G);
               end if;
            --  Unselect waste or foundation if that's where the curser is
            elsif
               G.Cursor.Zone in Waste_Zone | Foundation_Zone and then
                  G.Cursor.Zone = G.Selected_Cards.Zone
            then
               G.Selected_Cards.Zone := NA;
            --  Try moving the selected cards and ring the bell if that fails
            elsif Move_Card (G) then
               Moved := True;
               G.Selected_Cards.Zone := NA;
            else
               G.Selected_Cards.Zone := NA;
               Result := False;
            end if;

         when NA =>
            G.Selected_Cards.Zone := NA;
      end case;
      return Result;
   end Process_Actions;

   procedure Update_Game (G : in out Game_State) is
   begin
      --  Check for cards that need to be visible now
      for X of G.Tableau
         when Natural (X.S.Length) = X.Facing_Down
      loop
         X.Facing_Down := (if Natural (X.S.Length) = 0 then 0 else @ - 1);
      end loop;
   end Update_Game;

   function Game_Is_Won (G : Game_State) return Boolean is
       (for all X of G.Foundation =>
         not X.S.Is_Empty and then X.S.Last_Element.Rank = Cards.King);

   --  The present game
   Game : Game_State := Init;

   --  Vector containing backup of game state for undo
   Backup : Backups.Vector;

   --  To check if the game has been won yet
   Won_Game : Boolean := False;
begin
   --  Draw the inital game once.
   Draw_Game (Game);

   --  Game loop
   while not Won_Game loop
      declare
         --  Collect the next choice
         Key : constant Choice := Get_Input;
         --  Backup of the game state
         Tmp : constant Game_State := Game;
         Backup_Worthy : Boolean := False;
      begin
         exit when Key = Quit;

         case Key is
            --  Cursor moved a direction
            when Direction =>
               if not Move_Cursor (Game, Direction (Key)) then
                  Put (NO);
               end if;

            when Action =>
               if not Process_Actions (Game, Backup_Worthy) then
                  Put (NO);
               end if;

            when Undo =>
               if not Backup.Is_Empty then
                  --  Restore backup
                  Game := Backup.Last_Element;
                  Backup.Delete_Last;
               else
                  Put (NO);
               end if;

            when Restart =>
               --  Delete all backups
               Backup.Delete_Last (Backup.Length);
               --  Reset the game
               Game := Init;

            when others => null;
         end case;
         --  Backup the game if needed
         if Backup_Worthy then
            Backup.Append (Tmp);
         end if;
      end;

      --  Pogress game forward
      Update_Game (Game);

      --  Check for win
      Won_Game := Game_Is_Won (Game);
      --  Draw game
      Draw_Game (Game);
   end loop;

   if Won_Game then
      Put_Line ("You won the game, good job!");
      for I in 1 .. 10 loop
         Put (NO);
         delay 0.2;
      end loop;
   else
      Put_Line ("Goodbye...");
   end if;
end Klondike;
