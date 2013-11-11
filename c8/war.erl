%% Etude 8-1: Implementing war
%% @author Neal Shrader <neal@digitalocean.com>
-module(war).
-export([player/2, dealer/1]).

%% Improvements:
%% - make sure that in a war condition, it fairly decides who wins if both players send []
%% - kill player pids at end of game

%% Our entry point into the game.
dealer(CardsPerPlayer) ->
  random:seed(now()),
  {P1Cards, P2Cards} = lists:split(CardsPerPlayer, cards:shuffled_deck(CardsPerPlayer * 2)),
  P1 = spawn(war, player, [p1, P1Cards]),
  P2 = spawn(war, player, [p2, P2Cards]),
  dealer([], P1, P2).

%% New round
dealer([], P1, P2) ->
  P1 ! {report},
  P2 ! {report},
  io:format("[dealer] Asking players for cards~n"),
  P1 ! {send, self()},
  P2 ! {send, self()},
  get_card([], P1, P2);

dealer(GameState, P1, P2) ->
  Status = length(GameState),
  case Status of
    2 ->
      Decision = analyze_play(GameState);
    X when ((X-2) rem 8) == 0  ->
      Decision = analyze_play(GameState);
    _ ->
      Decision = get_card
  end,
  case Decision of
    get_card ->
      get_card(GameState, P1, P2);
    send_to_p1 ->
      io:format("[dealer] Sending cards to P1~n"),
      send_to_player(p1, P1, P2, GameState);
    send_to_p2 ->
      io:format("[dealer] Sending cards to P2~n"),
      send_to_player(p2, P1, P2, GameState);
    declare_war ->
      io:format("[dealer] WAR!  Asking for cards from players~n"),
      P1 ! {send, self()},
      P2 ! {send, self()},
      P1 ! {send, self()},
      P2 ! {send, self()},
      P1 ! {send, self()},
      P2 ! {send, self()},
      P1 ! {send, self()},
      P2 ! {send, self()},
      get_card(GameState, P1, P2)
  end.

get_card(GameState, P1, P2) ->
  receive
    PlayerCard ->
      case PlayerCard of
        {p1, []} ->
          io:format("P1 has no cards.  P2 wins!~n");
        {p2, []} ->
          io:format("P2 has no cards.  P1 wins!~n");
        _ ->
          io:format("[dealer] Got ~p~n", [PlayerCard]),
          dealer([PlayerCard|GameState], P1, P2)
      end
  end.

send_to_player(_, P1, P2, []) -> dealer([], P1, P2);
send_to_player(Winner, P1, P2, Cards) ->
  [{_,CardToSend}|Rest] = Cards,
  case Winner of
    p1 -> P1 ! {take, CardToSend};
    p2 -> P2 ! {take, CardToSend}
  end,
  send_to_player(Winner, P1, P2, Rest).
  

analyze_play(State) -> analyze_play(State, undef, undef).

analyze_play(_, P1Rank, P2Rank) when P1Rank /= undef, P2Rank /= undef ->
  if 
    P1Rank > P2Rank -> send_to_p1;
    P1Rank < P2Rank -> send_to_p2;
    P1Rank == P2Rank -> declare_war
  end;

analyze_play([{Player, {Rank, _}}| Rest], P1Rank, P2Rank) when Player == p1, P1Rank == undef ->
  NumRank = numeric_rank(Rank),
  analyze_play(Rest, NumRank, P2Rank);

analyze_play([{Player, {Rank, _}}| Rest], P1Rank, P2Rank) when Player == p2, P2Rank == undef ->
  NumRank = numeric_rank(Rank),
  analyze_play(Rest, P1Rank, NumRank);

analyze_play([_|Rest], P1Rank, P2Rank) -> analyze_play(Rest, P1Rank, P2Rank).

numeric_rank(Rank) ->
  case Rank of
    "A" -> 14;
    "K" -> 13;
    "Q" -> 12;
    "J" -> 11;
    _   -> Rank
  end.

%% player needs to maintain the state of his cards
%% send a card to a pid
%% receive cards
player(Id, []) ->
  receive 
    {send, Pid} ->
      Pid ! {Id, []},
      player(Id, []);
    {take, Card} ->
      player(Id, [Card]);
    {report} ->
      io:format("~p I got nothin!~n", [Id]),
      player(Id, [])
  end;
player(Id, Cards) ->
  receive 
    {send, Pid} ->
      [Top|Rest] = Cards,
      Pid ! {Id, Top},
      player(Id, Rest);
    {take, Card} ->
      player(Id, Cards ++ [Card]);
    {report} ->
      io:format("~p ~p~n", [Id, Cards]),
      player(Id, Cards)
  end.
