-module(listcomp).
-export([p1/0, p2/0]).


p1() -> 
  People = [{"Federico", $M, 22}, {"Kim", $F, 45}, {"Hansa", $F, 30}, 
    {"Tran", $M, 47}, {"Cathy", $F, 32}, {"Elias", $M, 50}],
  [ Name || {Name, Sex, Age} <- People, Sex == $M, Age > 40].

p2() ->  
  People = [{"Federico", $M, 22}, {"Kim", $F, 45}, {"Hansa", $F, 30}, 
    {"Tran", $M, 47}, {"Cathy", $F, 32}, {"Elias", $M, 50}],
  [ Name || {Name, Sex, Age} <- People, Sex == $M orelse Age > 40].
