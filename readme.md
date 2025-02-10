# Cards: a playing card library written in Ada

Cards is an implementation of the standard 52-card deck in Ada2022.

It supports suits, ranks, cards, as well as stacks of cards with functions for shuffling and dealing.

## Installation

### With [Alire](https://alire.ada.dev/)

Coming soon: Alire index

For now, simply clone the repo and then run `alr build` or `alr install`.

### Without Alire

This library has no dependencies, so you can also just copy `cards.ads` and `cards.adb` from the `src` folder into your own project.

## Usage

This library implements a tagged record named `Card`.  Each card has a `Rank` and a `Suit`, with various functions to derive the value and color, as well as providing the ability to print.

There is also an array of type card (`Card_List`) and a `Stack` of cards (derived from `Ada.Containers.Vectors.Vector`).

A `Stack` of cards can be simply created and added to later or you can initiate it from a `Card_List` using the `Create` function.  You can also initiate it from function `Deck`, which returns the standard 52 card deck.  You can then `Shuffle` the stack to mix the cards up as well as `Deal` from the `Top`, `Bottom`, or a `Random` location on the deck.  Dealing can return either another item of type `Stack` or as type `Card_List`.

Cards can be printed via `Name` (e.g. `Ace of Spades` or `AS`), `Name_Symbol` (e.g.`A♠`), `As_Box` (e.g. `|A♠|`), or `Symbol` (e.g. `🂡`).

Here's an example of creating a 6 deck stack of Blackjack cards, shuffling them through 12 iterations, and then discarding the top card to the discard pile:

```ada
with Cards;
declare
   --  Create a stack of 6 decks in the shoe.
   Shoe : Cards.Stack := 6 * Cards.Deck;
   --  The discard tray that we'll use.
   Discard_Tray : Cards.Stack;
begin
   Shoe.Shuffle (Rounds => 3);
   --  Reset discard_tray to burn-card.
   Discard_Tray := Shoe.Deal;
end;
```

Note that I'm completely resetting the discard try rather than appending it.  If I wanted to simply add the burn card to the Discard Tray, I could do this:
```ada
Discard_Tray.Append_Vector (Shoe.Deal);
```

`.Append_Vector` is needed because `Deal` returns a whole vector (of type `Stack`) instead of specific cards.

Full API documentation can be found in the `docs` folder, as well as `cards.ads` itself.  I recommended reading through that.

## Examples

Each function and type in `cards.ads` has an example, but you can also browse the unit tests in `tests`.  There is also a fully working Blackjack demo located in `examples/src/blackjack.adb` that can be built by `cd`ing to the `examples` directory and running `alr build`.
