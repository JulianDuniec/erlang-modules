%% A module that creates an n-gram histogram from a supplied string
-module(jdngram).
-export([create/2]).

%% Splits the supplied Text into a list of grams
%% Ex: ("Hello", 1) -> ["H","e", "l", "l", "o"]
get_grams(Text, GramLen) when length(Text) =< GramLen -> [Text]; %% Last item in recursion
get_grams(Text, GramLen) ->
	[string:substr(Text, 1, GramLen)] ++ get_grams(tl(Text), GramLen). %% Recurse until last in tail

%% Counts unique item in list (ex [a,a,b] will become {a : 2, b : 1})
histogram(List) ->
	dict:to_list(
		lists:foldl(	%% Fold the list into a dictionary
						%% Where the key is mapped to the 
						%% number of occurences of that key.
			fun(X, D) -> 
				dict:update_counter(X, 1, D) 
			end, 
			dict:new(), 
			List)).	

%% Gets a table containing the grams and weights for each gram.
%% Example create("AAAAAAAAABBBBB", 4) -> [{"AAAB",1},{"ABBB",1},{"AAAA",6},{"AABB",1},{"BBBB",2}]
create(Text, GramLen) ->
	histogram(
		get_grams(
			Text, GramLen)).