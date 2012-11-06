%% Different helper-methods 
%% for randoms

-module(jdrand).
-export([random_list/3]).

%% Generates a list of random numbers between Min and Max with the supplied Length
random_list(Length, Min, Max) ->
	lists:foldl(
		%% Fold the created list into a new list with random numbers
		fun(X, L) -> 
			L ++ [random:uniform() * Max - (-Min)] 
		end, 
		[], 
		%% Initialize a list with the supplied Length
		lists:seq(0, Length)).
