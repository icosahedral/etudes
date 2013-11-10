-module(cards).
-export([make_deck/0, show_deck/1, shuffled_deck/1]).

shuffled_deck(NumCards) -> 
  {FirstHalf, _} = lists:split(NumCards, shuffle(make_deck())),
  FirstHalf.

make_deck() ->
  [{Rank, Suit} || Rank <- [2,3,4,5,6,7,8,9,10,"J","Q","K","A"],
    Suit <- ["Spade", "Diamond", "Heart", "Club"]].

show_deck(Deck) ->
  lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).

%% Take a list, pass it to a helper function with an empty list
shuffle(List) -> shuffle(List, []).

%% Hit the end of the list, return our shuffled deck.
shuffle([], Acc) -> Acc;

%% Take our list, and split it at a random point.  Take the first element of
%% the second split list and stick that in the head of our accumulator.
%% Recurse with the remaining cards.
shuffle(List, Acc) ->
  {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
  shuffle(Leading ++ T, [H | Acc]).
