-module(ngram).
-export([ngram/2]).

%% Splits the supplied Text into a list of grams
%% Ex: ("Hello", 1) -> ["H","e", "l", "l", "o"]
get_grams(Text, GramLen) when length(Text) =< GramLen -> [Text];
get_grams(Text, GramLen) ->
	[string:substr(Text, 1, GramLen)] ++ get_grams(tl(Text), GramLen).

%% Counts unique item in list (ex [a,a,b] will become {a : 2, b : 1})
histogram(List) ->
	dict:to_list(lists:foldl(fun(X, D) -> dict:update_counter(X, 1, D) end, dict:new(), List)).	

%% Gets a table containing the grams and weights for each gram.
%% Example ngram("AAAAAAAAABBBBB", 4) -> [{"AAAB",1},{"ABBB",1},{"AAAA",6},{"AABB",1},{"BBBB",2}]
ngram(Text, GramLen) ->
	histogram(get_grams(Text, GramLen)).