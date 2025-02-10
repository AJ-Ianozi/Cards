-------------------------------------------------------------------------------
--     A simple blackjack game utilizing the Cards library
-------------------------------------------------------------------------------
--  Copyright (c) 2025 AJ Ianozi
--  Licensed under the MIT License.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Wide_Wide_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Cards;
procedure Blackjack is
   use type Cards.Rank;
   use type Cards.Stack;
   --  Fixed point for various things
   type Decimal is delta 10.0 ** (-2) digits 20;
   --  Percentage for varous purposes
   subtype Pct is Decimal range 0.0 .. 1.0;
   --  Settings for the blackjack game
   package Settings is
      Payout         : constant Decimal  := 1.5;
      Starting_Cash  : constant Decimal  := 150.00;
      Minimum_Bet    : constant Decimal  := 10.00;
      Decks_Per_Shoe : constant Positive := 6;
      Reshuffle      : constant Pct      := 0.33;
   end Settings;

   --  Ordered set to store sums of hands
   package Ordered_Numbers is new Ada.Containers.Ordered_Sets (Natural);

   type Game_Outcome is (Win, Loose, Push);

   type Hand_State is
      (NA, --  Nothing special
       BJ, --  Blackjack
       TO, -- Got 21, just not blackjack
       Bust); --  Lost

   type Participant is record
      Hand  : Cards.Stack;
      Score : Ordered_Numbers.Set;
      State : Hand_State := NA;
   end record;

   procedure Show_Hand (Hand : Cards.Stack) is
   begin
      for X of Hand loop
         Ada.Wide_Wide_Text_IO.Put (X.As_Box & " ");
      end loop;
   end Show_Hand;
   procedure Show_Sums (Sums : Ordered_Numbers.Set) is
   begin
      Ada.Text_IO.Put ("(");
      for S of Sums loop
         Ada.Text_IO.Put (S'Image & " ");
      end loop;
      Ada.Text_IO.Put (")");
   end Show_Sums;

   --  Sum each iteration of the hand

   procedure Set_State (Item : in out Participant) is
      use type Ada.Containers.Count_Type;
      function Sum (Hand : Cards.Stack) return Ordered_Numbers.Set is
         --  This will hold all of our sums
         Sums : Ordered_Numbers.Set;
      begin
         for C of Hand loop
            if C.Rank = Cards.Ace then
               --  add THIS ace to the result with a value of 11. Since a "1"
               --  will be included thanks to the Ace being counted twice,
               --  I add 10 (11 - 1 = 10)
               Sums.Include (10 + [for H of Hand => H.Value]'Reduce ("+", 0));
            end if;
            --  Add the result (including Ace with value of 1)
            Sums.Include ([for H of Hand => H.Value]'Reduce ("+", 0));
         end loop;
         --  If the smallest value is over 21, then return that
         if Sums.First_Element > 21 then
            return [Sums.First_Element];
         else
            --  Return everything up to and including 21
            return [for S of Sums when S <= 21 => S];
         end if;
      end Sum;
   begin
      Item.Score := Sum (Item.Hand);
      if Item.Score.First_Element > 21 then
         Item.State := Bust;
      elsif Item.Score.Contains (21) then
         Item.State := (if Item.Hand.Length = 2 then BJ else TO);
      else
         Item.State := NA;
      end if;
   end Set_State;

   --  Outcome of any given round
   Outcome : Game_Outcome;

   --  Create a stack of cards in the shoe.
   Shoe : Cards.Stack := Settings.Decks_Per_Shoe * Cards.Deck;

   --  The discard tray that we'll use.
   Discard_Tray : Cards.Stack;

   --  Length of the deck, for keeping track of usage
   Maxed_Stack : constant Decimal := Decimal (Shoe.Length);

   Shuffle_Needed : Boolean := True;

   --  Current money for the game
   Wallet      : Decimal := Settings.Starting_Cash;
   Current_Bet : Decimal;

   --  Player and dealer
   Player, Dealer : Participant;

begin
   Put_Line ("Welcome to blackjack!");
   Game_Loop : while Wallet >= Settings.Minimum_Bet loop
      Current_Bet := 0.00;
      Put_Line ("Cash remaining: $" & Wallet'Image);
      --  Get the current bet, or leave the game
      while Current_Bet = 0.00 loop
         Put ("Enter amount (minimum $" & Settings.Minimum_Bet'Image &
               ") to bet or press <enter> to quit: $");
         declare
            Input : constant String := Get_Line;
         begin
            --  Player didn't enter anything.
            exit Game_Loop when Input = "";
            declare
               Result : constant Decimal := Decimal'Value (Input);
            begin
               if Result > Wallet then
                  Put_Line ("Bet must be less than $" & Wallet'Image);
               elsif Result < Settings.Minimum_Bet then
                  Put_Line ("Bet must be greater than $" &
                              Settings.Minimum_Bet'Image);
               else
                  Current_Bet := Result;
               end if;
            end;
         exception
            when others => Put_Line ("Invalid entry.");
         end;
      end loop;

      --  Determine if we should shuffle the cards
      if Shuffle_Needed then
         --  Send discard tray back to shoe
         Shoe.Append_Vector (Discard_Tray.Deal_All);
         Shoe.Shuffle (Rounds => Settings.Decks_Per_Shoe * 3);
         --  Reset discard_tray to burn-card.
         Discard_Tray := Shoe.Deal;
         --  No longer need to shuffle
         Shuffle_Needed := False;
      end if;

      Dealer.Hand := Shoe.Deal;       --  Dealer gets first card.
      Player.Hand := Shoe.Deal (2);   --  Player gets two cards
      Dealer.Hand.Append (Shoe.Deal); --  Dealer gets second card

      --  Set initial state.
      Set_State (Dealer);
      Set_State (Player);

      Put ("Dealer's first card is: ");
      Ada.Wide_Wide_Text_IO.Put (Dealer.Hand.First_Element.As_Box);
      New_Line;
      --  Let the player hit (unless someone alerady has blackjack or bust)
      Player_Loop : while Dealer.State = NA and then Player.State = NA loop

         --  Show player info
         Put ("Player's Hand: ");
         Show_Hand (Player.Hand);
         Show_Sums (Player.Score);
         New_Line;
         Put ("Enter ""H"" for Hit it or ""S"" for Stand: ");
         declare
            Input : constant String := To_Upper (Get_Line);
         begin
            exit Player_Loop when Input = "S";
            if Input = "H" then
               Player.Hand.Append (Shoe.Deal);
               Set_State (Player);
            else
               Put_Line ("Invalid input: " & Input);
            end if;
         end;
      end loop Player_Loop;

      --  Have dealer hit if player didn't bust or  blackjack
      Dealer_Loop : while
         Dealer.State = NA and then
         Player.State not in BJ | Bust and then
         Dealer.Score.First_Element < 17
      loop
         Dealer.Hand.Append (Shoe.Deal);
         Set_State (Dealer);
      end loop Dealer_Loop;

      --  Determine output of the game.  Using expressive function because
      --  it's easist to set the exhaustive output.
      Outcome := (case Player.State is
                     when Bust => Loose,
                     when BJ   => (if Dealer.State = BJ then Push else Win),
                     when TO   => (case Dealer.State is
                                    when BJ => Loose,
                                    when TO => Push,
                                    when others => Win),
                     when NA   => (case Dealer.State is
                                    when Bust => Win,
                                    when BJ => Loose,
                                    when TO => Loose,
                                    when NA => --  Need to calculate score
                                       (if Player.Score.Last_Element =
                                          Dealer.Score.Last_Element
                                       then
                                          Push
                                       elsif
                                          Player.Score.Last_Element >
                                          Dealer.Score.Last_Element
                                       then
                                          Win
                                       else
                                          Loose)));
      --  Report results
      Put_Line ("Results: ");
      Put ("Player's Hand: ");
      Show_Hand (Player.Hand);
      Show_Sums (Player.Score);
      New_Line;
      if Player.State = Bust then
         Put_Line ("Player bust!");
      else
         if Player.State = BJ then
            Put_Line ("Player got blackjack!");
         end if;
         Put ("Dealer's Hand: ");
         Show_Hand (Dealer.Hand);
         Show_Sums (Dealer.Score);
         New_Line;
         case Dealer.State is
            when Bust =>
               Put_Line ("Dealer bust");
            when BJ =>
               Put_Line ("Dealer got blackjack!");
            when others =>
               null;
         end case;
      end if;

      --  Run the numbers
      case Outcome is
         when Win =>
            declare
               Winnings : constant Decimal := Current_Bet * Settings.Payout;
            begin
               Put_Line ("Player won $" & Winnings'Image & "!");
               Wallet := @ + (Winnings);
            end;
         when Loose =>
            Put_Line ("Dealer won.");
            Wallet := @ - Current_Bet;
         when Push =>
            Put_Line ("Push...");
      end case;
      New_Line;
      --  Send all cards in hands to discard tray
      Discard_Tray.Append_Vector (Player.Hand.Deal_All);
      Discard_Tray.Append_Vector (Dealer.Hand.Deal_All);

      --  Check if we need to reshuffle
      Shuffle_Needed := (Decimal (Shoe.Length) / Maxed_Stack) <
                           Settings.Reshuffle;

   end loop Game_Loop;
   Put_Line ("Goodbye!");
end Blackjack;
